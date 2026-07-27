## ===========================================================================
## Validation suite for e^x(S) / cross_garp() in programs/ex_cross.R
##
## Run from the project root (the folder containing 01_calculate_ccei.R):
##     Rscript programs/ex_cross_tests.R
##
## Uses only reads. Six independent checks:
##   T1  Hand-computed anchor (a 4-observation dataset with e^x = 0.9 by hand)
##   T2  Algorithm cross-check: cross_garp() [Warshall-reachability] vs an
##       igraph strong-component test vs a brute-force DFS simple-cycle search,
##       agreeing at every e on a grid, on reduced 4+4 real datasets.
##   T3  e^x cross-check: ex_cross() [Warshall] vs ex_cross_igraph() on full data.
##   T4  Structural identities: range, coalition monotonicity, Ihat efficiency.
##   T5  e^x isolation: when ccei(D_S)=ccei(D_g)=1, Lemma 1 forces
##       ccei(D_Sg) == e^x(S) exactly (isolates e^x against the existing CCEI code).
##   T6  Invariance: side-label swap and observation reordering leave e^x fixed.
##   (Lemma 1 itself, ccei(D_Sg)=min{ccei(D_S),ccei(D_g),e^x(S)}, is also checked.)
## ===========================================================================

suppressMessages({ library(haven); library(dplyr) })
source("programs/warshall.R"); source("programs/garp.R"); source("programs/ccei_garp.R")
source("programs/ex_cross.R")
source("programs/clean_pair_raw.R")
have_igraph <- requireNamespace("igraph", quietly = TRUE)

## ---- edge matrices at efficiency e ----------------------------------------
edges <- function(p, x, e) {
  n <- ncol(p); R0 <- matrix(FALSE, n, n); P0 <- matrix(FALSE, n, n)
  for (i in 1:n) for (j in 1:n) {
    bi <- e * sum(p[, i] * x[, i])
    R0[i, j] <- bi >= sum(p[, i] * x[, j]); P0[i, j] <- bi > sum(p[, i] * x[, j])
  }
  list(R0 = R0, P0 = P0)
}

## ---- independent impl A: igraph strong components --------------------------
cross_garp_igraph <- function(p, x, side, e) {
  ee <- edges(p, x, e); R0 <- ee$R0; P0 <- ee$P0
  g <- igraph::graph_from_adjacency_matrix(R0, mode = "directed", diag = FALSE)
  mem <- igraph::components(g, mode = "strong")$membership
  for (cc in unique(mem)) {
    nd <- which(mem == cc)
    if (length(nd) >= 2 && any(side[nd] == "I") && any(side[nd] == "G") &&
        any(P0[nd, nd, drop = FALSE])) return(0)
  }
  1
}
## ---- independent impl B: brute-force DFS over simple cycles ----------------
cross_garp_dfs <- function(p, x, side, e) {
  ee <- edges(p, x, e); R0 <- ee$R0; P0 <- ee$P0; n <- ncol(p)
  found <- FALSE
  visit <- function(start, u, path, strict, sides) {
    if (found) return(invisible())
    for (v in which(R0[u, ])) {
      s2 <- strict || P0[u, v]
      if (v == start) {
        full <- unique(c(sides, side[start]))
        if (s2 && ("I" %in% full) && ("G" %in% full)) { found <<- TRUE; return(invisible()) }
      } else if (!(v %in% path)) {
        visit(start, v, c(path, v), s2, unique(c(sides, side[v])))
      }
    }
  }
  for (s in 1:n) { visit(s, s, s, FALSE, side[s]); if (found) break }
  if (found) 0 else 1
}
## ---- e^x via igraph impl (for T3) -----------------------------------------
ex_cross_igraph <- function(p, x, side) {
  if (cross_garp_igraph(p, x, side, 1) == 1) return(1)
  eL <- 0; eH <- 1; es <- 1
  while (eH - eL > 1e-6) { e <- (eL + eH) / 2
    if (cross_garp_igraph(p, x, side, e) == 1) { es <- e; eL <- e } else { eH <- e } }
  es
}

## ---- helpers to pull subsets ----------------------------------------------
px <- function(sub) {
  s <- sub[!is.na(sub$coord_x) & !is.na(sub$coord_y) & !is.na(sub$intercept_x) &
             !is.na(sub$intercept_y) & sub$intercept_x != 0 & sub$intercept_y != 0, ]
  list(p = rbind(1/s$intercept_x, 1/s$intercept_y), x = rbind(s$coord_x, s$coord_y), n = nrow(s))
}
ccei_sub <- function(sub){ q <- px(sub); if (q$n < 2) return(NA); ccei_garp(q$p, q$x) }
merge_px <- function(a, b) list(p = cbind(a$p, b$p), x = cbind(a$x, b$x),
                                side = c(rep("I", ncol(a$p)), rep("G", ncol(b$p))))

## ---- load real data (reuse cleaned files if present, else build in memory) -
if (file.exists("data/base_raw.dta")) {
  base_raw <- read_dta("data/base_raw.dta")
} else {
  base_raw <- read_dta("data/riskpreference_pre.dta") %>%
    mutate(partner_id = partner) %>% select(all_of(keep_cols)) %>% clean_pair_raw()
}
base_raw <- base_raw %>% mutate(across(c(coord_x,coord_y,intercept_x,intercept_y), as.numeric))

get_group_sets <- function(raw, g) {
  sub <- raw[raw$group_id == g, ]
  mv <- unique(sub$id[sub$round_number==1 & sub$mover==1])
  nm <- unique(sub$id[sub$round_number==1 & sub$mover==0])
  if (length(mv)!=1 || length(nm)!=1) return(NULL)
  list(Di = sub[sub$id==mv & sub$round_number<=18,],
       Dj = sub[sub$id==nm & sub$round_number<=18,],
       Dg = sub[sub$id==mv & sub$round_number>=19 & sub$round_number<=36,])
}
gids <- unique(base_raw$group_id); set.seed(1)
gsample <- sample(gids, min(60, length(gids)))

pass <- function(name, ok, extra="") cat(sprintf("[%s] %s%s\n", ifelse(ok,"PASS","FAIL"), name,
                                                 ifelse(nchar(extra)>0, paste0("  (",extra,")"), "")))

## ===== T1: hand-computed anchor ============================================
## D_i = {(3,1;.3,.1),(1,3;.1,.3),(1.4,.4;.64,.26)} , D_g = {(1,1;.5,.5)}
p1 <- rbind(c(3,1,1.4,1), c(1,3,0.4,1)); x1 <- rbind(c(0.3,0.1,0.64,0.5), c(0.1,0.3,0.26,0.5))
sd1 <- c("I","I","I","G")
ex_w <- ex_cross(p1, x1, sd1)
ex_g <- if (have_igraph) ex_cross_igraph(p1, x1, sd1) else NA
pass("T1 hand anchor  e^x == 0.90 (Warshall)", abs(ex_w - 0.9) < 2e-3, sprintf("got %.4f", ex_w))
if (have_igraph) pass("T1 hand anchor  e^x == 0.90 (igraph)", abs(ex_g - 0.9) < 2e-3, sprintf("got %.4f", ex_g))

## ===== T2: three algorithms agree at every e on reduced 4+4 data ===========
egrid <- seq(0.30, 1.00, by = 0.05); mism <- 0; ncmp <- 0; ncross <- 0
for (g in gsample[1:min(30,length(gsample))]) {
  S <- get_group_sets(base_raw, g); if (is.null(S)) next
  qi <- px(S$Di[1:4,]); qg <- px(S$Dg[1:4,]); if (qi$n<4 || qg$n<4) next
  M <- list(p=cbind(qi$p,qg$p), x=cbind(qi$x,qg$x), side=c(rep("I",4),rep("G",4)))
  for (e in egrid) {
    a <- cross_garp(M$p, M$x, M$side, e)
    b <- cross_garp_dfs(M$p, M$x, M$side, e)
    d <- if (have_igraph) cross_garp_igraph(M$p, M$x, M$side, e) else a
    ncmp <- ncmp + 1; if (a==0) ncross <- ncross + 1
    if (a != b || a != d) mism <- mism + 1
  }
}
pass("T2 SCC vs DFS vs igraph agree on all (group,e)", mism == 0,
     sprintf("%d comparisons, %d with a cross-violation, %d mismatch", ncmp, ncross, mism))

## ===== T3: ex_cross (Warshall) vs ex_cross_igraph on full merged data =======
maxd3 <- 0; n3 <- 0
if (have_igraph) for (g in gsample) {
  S <- get_group_sets(base_raw, g); if (is.null(S)) next
  qi <- px(S$Di); qg <- px(S$Dg); if (qi$n<2 || qg$n<2) next
  M <- merge_px(qi, qg)
  d <- abs(ex_cross(M$p,M$x,M$side) - ex_cross_igraph(M$p,M$x,M$side))
  maxd3 <- max(maxd3, d); n3 <- n3 + 1
}
pass("T3 ex_cross Warshall vs igraph identical", maxd3 < 2e-3, sprintf("max|diff|=%.2e over %d groups", maxd3, n3))

## ===== T4: structural identities + Lemma 1 ==================================
bad_range<-0; bad_mono<-0; bad_eff<-0; maxL<-0; n4<-0
for (g in gsample) {
  S <- get_group_sets(base_raw, g); if (is.null(S)) next
  qi<-px(S$Di); qj<-px(S$Dj); qg<-px(S$Dg); if (qi$n<2||qj$n<2||qg$n<2) next
  exi <- ex_cross(cbind(qi$p,qg$p), cbind(qi$x,qg$x), c(rep("I",qi$n),rep("G",qg$n)))
  exj <- ex_cross(cbind(qj$p,qg$p), cbind(qj$x,qg$x), c(rep("I",qj$n),rep("G",qg$n)))
  exij<- ex_cross(cbind(qi$p,qj$p,qg$p), cbind(qi$x,qj$x,qg$x), c(rep("I",qi$n+qj$n),rep("G",qg$n)))
  n4 <- n4 + 1
  if (exi<=0 || exi>1 || exj<=0 || exj>1 || exij<=0 || exij>1) bad_range <- bad_range+1
  if (exij > min(exi,exj) + 1e-6) bad_mono <- bad_mono+1
  Ii <- ihat_from_ex(exi,exj,exij); Ij <- ihat_from_ex(exj,exi,exij)
  if (!is.na(Ii) && !is.na(Ij) && abs(Ii+Ij-1) > 1e-9) bad_eff <- bad_eff+1
  # Lemma 1 (single-member)
  eg<-ccei_sub(S$Dg); ei<-ccei_sub(S$Di); eig<-ccei_sub(rbind(S$Di,S$Dg))
  if (!any(is.na(c(eg,ei,eig)))) maxL <- max(maxL, abs(eig - min(ei,eg,exi)))
}
pass("T4a range  e^x in (0,1]",            bad_range==0, sprintf("%d groups",n4))
pass("T4b coalition monotone e^x(ij)<=min", bad_mono==0)
pass("T4c efficiency  Ihat_i + Ihat_j = 1", bad_eff==0)
pass("T4d Lemma 1  ccei(D_ig)=min{ei,eg,e^x(i)}", maxL < 2e-3, sprintf("max|resid|=%.2e",maxL))

## ===== T5: e^x isolation when ccei(D_S)=ccei(D_g)=1 =========================
n5<-0; bad5<-0; maxd5<-0
for (g in gids) {                      # scan all groups to find qualifying ones
  S <- get_group_sets(base_raw, g); if (is.null(S)) next
  ei<-ccei_sub(S$Di); eg<-ccei_sub(S$Dg); if (any(is.na(c(ei,eg)))) next
  if (abs(ei-1)<1e-9 && abs(eg-1)<1e-9) {
    qi<-px(S$Di); qg<-px(S$Dg)
    exi <- ex_cross(cbind(qi$p,qg$p), cbind(qi$x,qg$x), c(rep("I",qi$n),rep("G",qg$n)))
    eig <- ccei_sub(rbind(S$Di,S$Dg))       # existing code, must equal exi exactly
    n5<-n5+1; d<-abs(eig-exi); maxd5<-max(maxd5,d); if (d>2e-3) bad5<-bad5+1
  }
  if (n5 >= 40) break
}
pass("T5 isolation  ccei(D_ig)==e^x(i) when ei=eg=1", bad5==0,
     sprintf("%d qualifying groups, max|diff|=%.2e", n5, maxd5))

## ===== T6: invariance to side-swap and column reordering ===================
bad6<-0; n6<-0
for (g in gsample[1:min(25,length(gsample))]) {
  S <- get_group_sets(base_raw, g); if (is.null(S)) next
  qi<-px(S$Di); qg<-px(S$Dg); if (qi$n<2||qg$n<2) next
  P<-cbind(qi$p,qg$p); X<-cbind(qi$x,qg$x); sd<-c(rep("I",qi$n),rep("G",qg$n))
  base_ex <- ex_cross(P,X,sd)
  swap_ex <- ex_cross(P,X, ifelse(sd=="I","G","I"))        # label swap: symmetric -> invariant
  o <- sample(ncol(P)); perm_ex <- ex_cross(P[,o],X[,o],sd[o])  # reorder columns -> invariant
  n6<-n6+1
  if (abs(base_ex-swap_ex)>1e-9 || abs(base_ex-perm_ex)>1e-9) bad6<-bad6+1
}
pass("T6 invariance  side-swap & reorder leave e^x fixed", bad6==0, sprintf("%d groups",n6))

## ===== T7: exact-tie / non-attained-sup handling (regression guard) =========
## Adversarial verification found that at an EXACT expenditure tie the only cross structure
## can sit at the closed endpoint e=1; e^x must snap to sup=1 and Ihat must be NA (no revealed
## disagreement). Before the fix, ex_cross returned 0.9999990 and ihat_from_ex returned a huge
## finite value (guard 1e-9 below the bisection floor 2^-20). These two hand cases lock the fix.
t7 <- TRUE; det <- c()
pA <- matrix(c(3,4,2,4,3,3,2,3),2,4); xA <- matrix(c(4,3,4,2,2,3,1,4),2,4); sdA <- c('G','G','I','I')
exA <- ex_cross(pA,xA,sdA); if (abs(exA-1) > 1e-9) { t7 <- FALSE; det <- c(det, sprintf("A ex=%.7f",exA)) }
pB <- rbind(c(1,1),c(1,2)); xB <- rbind(c(1,0),c(0,1)); sdB <- c('I','G')
exB <- ex_cross(pB,xB,sdB); ihB <- ihat_from_ex(0.80,0.95,exB)
if (abs(exB-1) > 1e-9 || !is.na(ihB)) { t7 <- FALSE; det <- c(det, sprintf("B ex=%.7f ihat=%s",exB,format(ihB))) }
pass("T7 exact-tie: e^x snaps to 1 and Ihat=NA (guard)", t7, paste(det, collapse="; "))

## NOTE on T2/T3 oracles: cross_garp and cross_garp_igraph both use CLOSED-WALK (SCC) semantics
## -- the reading that matches garp.R/ccei_garp.R and makes Lemma 1 hold. cross_garp_dfs uses
## SIMPLE-cycle semantics, which diverges from the other two ONLY at exact ties (e = an exact
## expenditure ratio). On the tie-free real 4+4 data used in T2/T3 all three agree; treat the
## igraph SCC oracle (not the DFS one) as the ground truth if you extend T2 to tie data.

cat("\nDone.\n")
