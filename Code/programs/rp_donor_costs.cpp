// Exact compiled donor kernels for the HM and MaxMPI algorithms in calculate_rp_indices.R.
// HM uses violation supports and minimum hitting sets; MaxMPI uses Dinkelbach/Bellman-Ford
// bounds and the same Lawler branching. An optional branch handoff returns only a
// lower bound when unfinished; the builder then proves the optimum with HiGHS.
// Expenditure comparisons remain exact integers.
#include <Rcpp.h>
#include <cstdint>
#include <algorithm>
#include <vector>
#include <numeric>
#include <limits>
using namespace Rcpp;
using Mask = uint64_t;
using Vec = std::vector<int>;
// [[Rcpp::plugins(cpp11)]]

struct Graph {
  int n;
  std::vector<double> expenditure, weight;
  std::vector<Mask> weak, strict;
  Vec side;
  Graph(NumericMatrix e, CharacterVector sd): n(e.nrow()), expenditure(n*n),
      weight(n*n), weak(n,0), strict(n,0), side(n) {
    if (n != e.ncol() || n > 63 || sd.size() != n) stop("Expected a square donor graph with at most 63 observations.");
    for (int u=0;u<n;++u) {
      std::string s=as<std::string>(sd[u]);
      if (s!="I" && s!="G") stop("Unknown observation side.");
      side[u]=(s=="G");
      if (!R_finite(e(u,u)) || e(u,u)<=0) stop("Own expenditure must be positive.");
      for (int v=0;v<n;++v) {
        double x=e(u,v);
        expenditure[u*n+v]=x;
        weight[u*n+v]=1-x/e(u,u);
        if (u!=v && x<=e(u,u)) weak[u]|=(Mask(1)<<v);
        if (u!=v && x<e(u,u)) strict[u]|=(Mask(1)<<v);
      }
    }
  }
};
int size_mask(Mask x) { return __builtin_popcountll(x); }
Vec nodes(Mask x) {
  Vec a;
  while (x) {int v=__builtin_ctzll(x);a.push_back(v);x&=x-1;}
  return a;
}

// Same exact-ratio search as rp_cost_ccei; compiled for the one-choice hold-out donors.
// [[Rcpp::export]]
double rp_donor_ccei(NumericMatrix e, CharacterVector side) {
  Graph g(e, side);
  auto violation = [&](double num, double den, bool strict_as_weak = false) {
    std::vector<Mask> reach(g.n, 0), strict(g.n, 0);
    for (int u = 0; u < g.n; ++u) {
      reach[u] = Mask(1) << u;
      for (int v = 0; v < g.n; ++v) {
        double lhs = e(u,v) * den, rhs = e(u,u) * num;
        if (lhs <= rhs) reach[u] |= Mask(1) << v;
        if (u != v && (strict_as_weak ? lhs <= rhs : lhs < rhs)) strict[u] |= Mask(1) << v;
      }
    }
    for (int v = 0; v < g.n; ++v) for (int u = 0; u < g.n; ++u)
      if (reach[u] & (Mask(1) << v)) reach[u] |= reach[v];
    for (int u = 0; u < g.n; ++u) {
      Mask comp = 0;
      bool individual = false, group = false;
      for (int v = 0; v < g.n; ++v) if ((reach[u] & (Mask(1) << v)) && (reach[v] & (Mask(1) << u))) {
        comp |= Mask(1) << v;
        if (g.side[v]) group = true; else individual = true;
      }
      if (individual && group) for (int v : nodes(comp)) if (strict[v] & comp) return true;
    }
    return false;
  };
  if (!violation(1,1)) return 0;
  struct Ratio { double value, num, den; };
  std::vector<Ratio> ratios;
  double max_den = 0;
  for (int u = 0; u < g.n; ++u) for (int v = 0; v < g.n; ++v) {
    double value = e(u,v) / e(u,u);
    if (value > 0 && value < 1) {
      ratios.push_back({value, e(u,v), e(u,u)});
      max_den = std::max(max_den, e(u,u));
    }
  }
  if (max_den && 1/(max_den*max_den) <= 8*std::numeric_limits<double>::epsilon()) stop("Expenditure ratios exceed exact-search precision.");
  std::sort(ratios.begin(), ratios.end(), [](Ratio a, Ratio b) { return a.value < b.value; });
  ratios.erase(std::unique(ratios.begin(), ratios.end(), [](Ratio a, Ratio b) { return a.value == b.value; }), ratios.end());
  int lo = -1, hi = ratios.size();
  while (hi-lo > 1) {
    int mid = (lo+hi)/2;
    if (violation(ratios[mid].num, ratios[mid].den)) hi = mid; else lo = mid;
  }
  bool attained = lo >= 0 && violation(ratios[lo].num, ratios[lo].den, true);
  double threshold = attained ? ratios[lo].value : (hi == int(ratios.size()) ? 1 : ratios[hi].value);
  return 1-threshold;
}
// Assemble each held-out target's donor graphs once, using the same cost kernel above.
// [[Rcpp::export]]
NumericMatrix rp_donor_ccei_batch(NumericMatrix individual, List groups) {
  if (individual.nrow()!=2 || individual.ncol()!=4) stop("Expected two held-out individual choices.");
  NumericMatrix costs(groups.size(),3);
  for (int donor=0; donor<groups.size(); ++donor) {
    NumericMatrix group=groups[donor];
    if (group.ncol()!=4) stop("Expected four choice-coordinate/intercept columns.");
    int n=group.nrow()+2;
    NumericMatrix data(n,4), e(n,n), first(n-1,n-1), second(n-1,n-1);
    for (int j=0;j<4;++j) {
      for (int i=0;i<2;++i) data(i,j)=individual(i,j);
      for (int i=2;i<n;++i) data(i,j)=group(i-2,j);
    }
    for (int u=0;u<n;++u) for (int v=0;v<n;++v)
      e(u,v)=data(u,3)*data(v,0)+data(u,2)*data(v,1);
    for (int u=0;u<n-1;++u) for (int v=0;v<n-1;++v) {
      first(u,v)=e(u==0?0:u+1,v==0?0:v+1);
      second(u,v)=e(u+1,v+1);
    }
    CharacterVector pair_side(n,"G"), single_side(n-1,"G");
    pair_side[0]="I"; pair_side[1]="I"; single_side[0]="I";
    costs(donor,0)=rp_donor_ccei(first,single_side);
    costs(donor,1)=rp_donor_ccei(second,single_side);
    costs(donor,2)=rp_donor_ccei(e,pair_side);
  }
  return costs;
}

std::vector<Mask> components(const Graph& g, Mask keep) {
  std::vector<Mask> reach(g.n,0);
  for (int u:nodes(keep)) reach[u]=(g.weak[u]&keep)|(Mask(1)<<u);
  for (int v:nodes(keep)) for (int u:nodes(keep))
    if (reach[u]&(Mask(1)<<v)) reach[u]|=reach[v];
  std::vector<Mask> result;
  Mask todo=keep;
  while (todo) {
    int u=__builtin_ctzll(todo);
    Mask comp=0;
    for (int v:nodes(reach[u])) if (reach[v]&(Mask(1)<<u)) comp|=(Mask(1)<<v);
    result.push_back(comp);todo&=~comp;
  }
  return result;
}
bool is_cross(const Graph& g, const Vec& cycle) {
  bool a=false,b=false;
  for(int u:cycle) {if(g.side[u])b=true;else a=true;}
  return a&&b;
}
bool strict_cycle(const Graph& g, const Vec& cycle) {
  for(size_t j=0;j<cycle.size();++j)
    if(g.strict[cycle[j]]&(Mask(1)<<cycle[(j+1)%cycle.size()])) return true;
  return false;
}
Mask support(const Graph& g, Mask keep) {
  Mask best=0;
  for(Mask comp:components(g,keep)) {
    Vec ns=nodes(comp);
    if(ns.size()<2 || !is_cross(g,ns)) continue;
    bool has_strict=false;
    for(int u:ns) if(g.strict[u]&comp) has_strict=true;
    if(!has_strict) continue;
    std::vector<Vec> pred(g.n,Vec(g.n,-1));
    for(int source:ns) {
      Vec queue{source};pred[source][source]=source;
      for(size_t k=0;k<queue.size();++k) {
        int u=queue[k];
        for(int v:nodes(g.weak[u]&comp)) if(pred[source][v]<0) {
          pred[source][v]=u;queue.push_back(v);
        }
      }
    }
    auto path=[&](int from,int to) {
      Vec p;
      if(pred[from][to]<0) return p;
      for(int v=to;;v=pred[from][v]) {p.push_back(v);if(v==from)break;}
      std::reverse(p.begin(),p.end());return p;
    };
    for(int a:ns) for(int b:ns) {
      if(!(g.weak[a]&(Mask(1)<<b)) || g.side[a]==g.side[b])continue;
      Vec p=path(b,a),cycle{a};
      for(int v:p) if(v!=a)cycle.push_back(v);
      if(!strict_cycle(g,cycle))continue;
      Mask cut=0;for(int v:cycle)cut|=Mask(1)<<v;
      if(!best || size_mask(cut)<size_mask(best)) best=cut;
      if(size_mask(best)==2)return best;
    }
    // Exact-tie fallback: a strict cycle with a detour to the other side.
    if(!best) for(int a:ns) for(int b:ns) {
      if(!(g.strict[a]&(Mask(1)<<b)))continue;
      Vec p=path(b,a);Mask cycle=Mask(1)<<a;
      for(int v:p)cycle|=Mask(1)<<v;
      if(is_cross(g,nodes(cycle))) {
        if(!best || size_mask(cycle)<size_mask(best))best=cycle;
      } else for(int w:ns) if(g.side[w]!=g.side[a]) {
        Mask cut=cycle;
        for(int v:path(a,w))cut|=Mask(1)<<v;
        for(int v:path(w,a))cut|=Mask(1)<<v;
        if(!best || size_mask(cut)<size_mask(best))best=cut;
      }
    }
  }
  return best;
}
struct HittingSet {
  int n,best_size;
  Mask best;
  HittingSet(std::vector<Mask> cuts,int size):n(size),best_size(size),best(0) {
    std::stable_sort(cuts.begin(),cuts.end(),[](Mask a,Mask b){return size_mask(a)<size_mask(b);});
    std::vector<Mask> minimal;
    for(Mask x:cuts) {
      bool redundant=false;
      for(Mask y:minimal)if((x&y)==y){redundant=true;break;}
      if(!redundant)minimal.push_back(x);
    }
    auto remaining=minimal;
    Mask greedy=0;
    while(!remaining.empty()) {
      Vec freq(n,0);
      for(Mask x:remaining)for(int v:nodes(x))++freq[v];
      int v=std::max_element(freq.begin(),freq.end())-freq.begin();
      Mask bit=Mask(1)<<v;greedy|=bit;
      remaining.erase(std::remove_if(remaining.begin(),remaining.end(),[&](Mask x){return bool(x&bit);}),remaining.end());
    }
    best=greedy;best_size=size_mask(best);search(0,minimal);
  }
  void search(Mask chosen,const std::vector<Mask>& remaining) {
    int count=size_mask(chosen);
    if(remaining.empty()) {if(count<best_size){best=chosen;best_size=count;}return;}
    Mask used=0;int bound=0;
    for(Mask x:remaining)if(!(x&used)){used|=x;++bound;}
    if(count+bound>=best_size)return;
    Mask cut=remaining.front();
    for(int v:nodes(cut)) {
      Mask bit=Mask(1)<<v;std::vector<Mask> next;
      for(Mask x:remaining)if(!(x&bit))next.push_back(x);
      search(chosen|bit,next);
    }
  }
};
// [[Rcpp::export]]
int rp_donor_hm(NumericMatrix e, CharacterVector side) {
  Graph g(e,side);
  Mask all=(Mask(1)<<g.n)-1;
  std::vector<Mask> cuts;
  for(int u=0;u<g.n;++u)for(int v=u+1;v<g.n;++v)
    if(g.side[u]!=g.side[v] && (g.weak[u]&(Mask(1)<<v)) &&
       (g.weak[v]&(Mask(1)<<u)) &&
       ((g.strict[u]&(Mask(1)<<v)) || (g.strict[v]&(Mask(1)<<u))))
      cuts.push_back((Mask(1)<<u)|(Mask(1)<<v));
  Mask s=support(g,all);
  if(!s)return 0;
  cuts.push_back(s);
  for(int i=0;i<500;++i) {
    HittingSet h(cuts,g.n);
    s=support(g,all&~h.best);
    if(!s)return h.best_size;
    cuts.push_back(s);
  }
  stop("HM constraint generation did not converge.");return 0;
}
struct Ratio {double lambda=0;Vec cycle;bool tight=false;};
double mpi(const Graph& g,const Vec& cycle) {
  long double sum=0;
  for(size_t j=0;j<cycle.size();++j)sum+=g.weight[cycle[j]*g.n+cycle[(j+1)%cycle.size()]];
  return double(sum/cycle.size());
}
Ratio ratio(const Graph& g,const Vec& ns,const Vec& banned={},const Vec& forced={}) {
  int n=ns.size();Ratio out;
  if(n<2){out.tight=true;return out;}
  std::vector<bool> weak(n*n,false);Vec local(g.n,-1);
  for(int a=0;a<n;++a)local[ns[a]]=a;
  for(int a=0;a<n;++a)for(int b=0;b<n;++b)
    weak[a*n+b]=bool(g.weak[ns[a]]&(Mask(1)<<ns[b]));
  for(int edge:banned) {
    int a=local[edge/g.n],b=local[edge%g.n];
    if(a>=0&&b>=0)weak[a*n+b]=false;
  }
  for(int edge:forced) {
    int a=local[edge/g.n],b=local[edge%g.n];
    if(a<0||b<0||!weak[a*n+b]){out.tight=true;return out;}
    for(int j=0;j<n;++j){weak[a*n+j]=false;weak[j*n+b]=false;}
    weak[a*n+b]=true;
  }
  // A cross cycle must lie in a mixed-side strongly connected component.
  // Forcing/deleting edges can split a component; internal-only components
  // cannot improve the cross-cycle bound and need no Lawler branching.
  std::vector<Mask> reach(n,0), component(n,0);
  for(int a=0;a<n;++a) {
    reach[a]=Mask(1)<<a;
    for(int b=0;b<n;++b)if(weak[a*n+b])reach[a]|=Mask(1)<<b;
  }
  for(int k=0;k<n;++k)for(int a=0;a<n;++a)
    if(reach[a]&(Mask(1)<<k))reach[a]|=reach[k];
  for(int a=0;a<n;++a) {
    Vec block;
    for(int b:nodes(reach[a]))if(reach[b]&(Mask(1)<<a))block.push_back(ns[b]);
    if(is_cross(g,block))for(int b:nodes(reach[a]))
      if(reach[b]&(Mask(1)<<a))component[a]|=Mask(1)<<b;
  }
  for(int a=0;a<n;++a)for(int b=0;b<n;++b)
    if(!(component[a]&(Mask(1)<<b)))weak[a*n+b]=false;
  if(std::find(weak.begin(),weak.end(),true)==weak.end()){out.tight=true;return out;}
  for(int it=0;it<200;++it) {
    std::vector<double> dist(n,0),mins(n);Vec pred(n,-1),arg(n);int upd=-1;
    for(int r=0;r<n;++r) {
      for(int b=0;b<n;++b) {
        mins[b]=R_PosInf;arg[b]=0;
        for(int a=0;a<n;++a)if(weak[a*n+b]) {
          double x=(out.lambda-g.weight[ns[a]*g.n+ns[b]])+dist[a];
          if(x<mins[b]){mins[b]=x;arg[b]=a;}
        }
      }
      upd=-1;
      for(int b=0;b<n;++b)if(mins[b]<dist[b]-1e-15) {
        dist[b]=mins[b];pred[b]=arg[b];if(upd<0)upd=b;
      }
      if(upd<0)break;
    }
    if(upd<0){out.tight=true;break;}
    int x=upd;
    for(int r=0;r<n;++r){if(pred[x]<0)break;x=pred[x];}
    if(pred[x]<0)break;
    int cur=x;Vec seen;
    do {seen.push_back(cur);cur=pred[cur];}while(cur>=0&&cur!=x&&int(seen.size())<=n);
    if(cur<0||cur!=x)break;
    std::reverse(seen.begin(),seen.end());out.cycle.clear();
    for(int v:seen)out.cycle.push_back(ns[v]);
    double value=mpi(g,out.cycle);
    if(value<=out.lambda){out.lambda+=1e-12;continue;}
    out.lambda=value;
  }
  return out;
}
struct Branch {Vec banned,forced;};
struct Cross {double value;bool exhausted=true;};
Cross cross_cycle(const Graph& g,const Vec& ns,double lower,int branch_limit) {
  Cross result{lower,true};
  for(size_t a=0;a<ns.size();++a)for(size_t b=a+1;b<ns.size();++b) {
    int u=ns[a],v=ns[b];
    if(g.side[u]!=g.side[v] && (g.weak[u]&(Mask(1)<<v)) && (g.weak[v]&(Mask(1)<<u)))
      result.value=std::max(result.value,mpi(g,Vec{u,v}));
  }
  // A triangle gives a stronger valid incumbent than two-cycles alone.
  // Every mixed triangle has at least one I-to-G edge.
  std::vector<Mask> incoming(g.n,0);
  Mask members=0;for(int u:ns)members|=Mask(1)<<u;
  for(int u:ns)for(int v:nodes(g.weak[u]&members))incoming[v]|=Mask(1)<<u;
  for(int u:ns)if(!g.side[u])for(int v:nodes(g.weak[u]&members))if(g.side[v]) {
    for(int w:nodes(g.weak[v]&incoming[u]&members))if(w!=u&&w!=v)
      result.value=std::max(result.value,mpi(g,Vec{u,v,w}));
  }
  std::vector<Branch> stack(1);int used=0;
  while(!stack.empty()) {
    if(branch_limit>0 && used>=branch_limit){result.exhausted=false;break;}
    Branch node=std::move(stack.back());stack.pop_back();++used;
    if(used%256==0)checkUserInterrupt();
    Ratio mr=ratio(g,ns,node.banned,node.forced);
    if(mr.cycle.empty()){if(!mr.tight)stop("MaxMPI empty subproblem on an unproven bound.");continue;}
    if(mr.tight&&mr.lambda<=result.value+1e-12)continue;
    if(is_cross(g,mr.cycle)) {
      result.value=std::max(result.value,mpi(g,mr.cycle));
      if(mr.tight)continue;
    }
    Vec edges;
    for(size_t j=0;j<mr.cycle.size();++j)
      edges.push_back(mr.cycle[j]*g.n+mr.cycle[(j+1)%mr.cycle.size()]);
    for(int j=int(edges.size())-1;j>=0;--j) {
      Branch child=node;child.banned.push_back(edges[j]);
      child.forced.insert(child.forced.end(),edges.begin(),edges.begin()+j);
      stack.push_back(std::move(child));
    }
  }
  result.exhausted=stack.empty();return result;
}
// [[Rcpp::export]]
List rp_donor_mpi(NumericMatrix e,CharacterVector side,int branch_limit=0) {
  Graph g(e,side);double best=0;bool certified=true,exhausted=true;
  Mask all=(Mask(1)<<g.n)-1;
  for(Mask comp:components(g,all)) {
    Vec ns=nodes(comp);
    if(ns.size()<2||!is_cross(g,ns))continue;
    Ratio mr=ratio(g,ns);
    if(mr.cycle.empty()){if(!mr.tight)stop("MaxMPI empty component on an unproven bound.");continue;}
    if(mr.lambda<=0&&mr.tight)continue;
    if(mr.tight&&is_cross(g,mr.cycle))best=std::max(best,mpi(g,mr.cycle));
    else {
      certified=false;Cross result=cross_cycle(g,ns,best,branch_limit);
      best=std::max(best,result.value);exhausted=exhausted&&result.exhausted;
    }
  }
  return List::create(_["value"]=best,_["certified"]=certified,_["exhausted"]=exhausted);
}
