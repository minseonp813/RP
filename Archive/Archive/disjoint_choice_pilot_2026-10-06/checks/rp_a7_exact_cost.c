#include <R.h>
#include <Rinternals.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>

typedef struct { double value, num, den; int order; } Ratio;
typedef struct {
    int n, next, top, ncomp;
    unsigned char *weak, *onstack;
    int *number, *low, *stack, *component;
} Graph;

static void visit(Graph *g, int u) {
    g->number[u] = g->low[u] = g->next++;
    g->stack[g->top++] = u;
    g->onstack[u] = 1;
    for (int v=0; v<g->n; v++) if (u != v && g->weak[u + g->n*v]) {
        if (g->number[v] == -1) {
            visit(g, v);
            if (g->low[v] < g->low[u]) g->low[u] = g->low[v];
        } else if (g->onstack[v] && g->number[v] < g->low[u]) {
            g->low[u] = g->number[v];
        }
    }
    if (g->low[u] == g->number[u]) {
        int v;
        do {
            v = g->stack[--g->top];
            g->onstack[v] = 0;
            g->component[v] = g->ncomp;
        } while (v != u);
        g->ncomp++;
    }
}

static int violates(const double *e, const double *diagonal, const int *side,
                    double num, double den, int attained, Graph *g,
                    unsigned char *strict, int *flags, int *sizes) {
    int n = g->n;
    for (int u=0; u<n; u++) {
        double rhs = diagonal[u] * num;
        for (int v=0; v<n; v++) {
            double lhs = e[u+n*v] * den;
            g->weak[u+n*v] = lhs <= rhs;
            strict[u+n*v] = u != v && (attained ? lhs <= rhs : lhs < rhs);
        }
        g->number[u] = -1;
        g->onstack[u] = 0;
        flags[u] = sizes[u] = 0;
    }
    g->next = g->top = g->ncomp = 0;
    for (int u=0; u<n; u++) if (g->number[u] == -1) visit(g, u);
    for (int u=0; u<n; u++) {
        flags[g->component[u]] |= side ? side[u] : 3;
        sizes[g->component[u]]++;
    }
    for (int u=0; u<n; u++) {
        int c = g->component[u];
        if (sizes[c] < 2 || flags[c] != 3) continue;
        for (int v=0; v<n; v++)
            if (g->component[v] == c && strict[u+n*v]) return 1;
    }
    return 0;
}

static int compare_ratio(const void *a, const void *b) {
    const Ratio *x=a, *y=b;
    if (x->value < y->value) return -1;
    if (x->value > y->value) return 1;
    return (x->order > y->order) - (x->order < y->order);
}

SEXP rp_exact_cost(SEXP matrix, SEXP sides) {
    SEXP dim = Rf_getAttrib(matrix, R_DimSymbol);
    if (TYPEOF(matrix) != REALSXP || Rf_length(dim) != 2 ||
        INTEGER(dim)[0] != INTEGER(dim)[1]) Rf_error("Expected a numeric square matrix");
    int n = INTEGER(dim)[0];
    if (n < 2) return Rf_ScalarReal(0);
    if (sides != R_NilValue && (TYPEOF(sides) != STRSXP || Rf_length(sides) != n))
        Rf_error("Invalid side vector");
    const double *e = REAL(matrix);
    double *dg = (double *)R_alloc(n, sizeof(double));
    int *sd = sides == R_NilValue ? NULL : (int *)R_alloc(n, sizeof(int));
    for (int u=0; u<n; u++) {
        dg[u] = e[u+n*u];
        if (sd) {
            const char *s = CHAR(STRING_ELT(sides,u));
            sd[u] = strcmp(s,"I") == 0 ? 1 : strcmp(s,"G") == 0 ? 2 : 0;
        }
    }
    Graph g = {0};
    g.n = n;
    g.weak = (unsigned char *)R_alloc(n*n, sizeof(unsigned char));
    g.onstack = (unsigned char *)R_alloc(n, sizeof(unsigned char));
    g.number = (int *)R_alloc(n, sizeof(int));
    g.low = (int *)R_alloc(n, sizeof(int));
    g.stack = (int *)R_alloc(n, sizeof(int));
    g.component = (int *)R_alloc(n, sizeof(int));
    unsigned char *strict = (unsigned char *)R_alloc(n*n, sizeof(unsigned char));
    int *flags = (int *)R_alloc(n, sizeof(int));
    int *sizes = (int *)R_alloc(n, sizeof(int));
    if (!violates(e,dg,sd,1,1,0,&g,strict,flags,sizes)) return Rf_ScalarReal(0);
    Ratio *ratios = (Ratio *)R_alloc(n*n, sizeof(Ratio));
    int m=0;
    double max_den=0;
    for (int v=0; v<n; v++) for (int u=0; u<n; u++) {
        double value = e[u+n*v]/dg[u];
        if (value > 0 && value < 1) {
            ratios[m++] = (Ratio){value,e[u+n*v],dg[u],u+n*v};
            if (dg[u] > max_den) max_den=dg[u];
        }
    }
    if (max_den > 0 && 1/(max_den*max_den) <= 8*DBL_EPSILON)
        Rf_error("Expenditure ratios do not meet exact-comparison bounds");
    qsort(ratios,m,sizeof(Ratio),compare_ratio);
    int unique=0;
    for (int k=0; k<m; k++)
        if (k==0 || ratios[k].value != ratios[unique-1].value) ratios[unique++]=ratios[k];
    m=unique;
    int lo=0, hi=m+1;
    while (hi-lo > 1) {
        int mid=(lo+hi)/2;
        Ratio r=ratios[mid-1];
        if (violates(e,dg,sd,r.num,r.den,0,&g,strict,flags,sizes)) hi=mid;
        else lo=mid;
    }
    int attained=0;
    if (lo>0) {
        Ratio r=ratios[lo-1];
        attained=violates(e,dg,sd,r.num,r.den,1,&g,strict,flags,sizes);
    }
    double efficiency = attained ? ratios[lo-1].value : hi==m+1 ? 1 : ratios[hi-1].value;
    return Rf_ScalarReal(1-efficiency);
}
