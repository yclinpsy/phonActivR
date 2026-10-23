# linking_tests.R -- Can a response-stage linking function repair the model's
# fit to the C (small-grain) competition of Lin et al. (2023)?
#
# Reported in the manuscript, Step 6b. Run from any directory with the
# package installed:  Rscript linking_tests.R
# Writes linking_tests_results.csv next to the working directory and prints
# the best fit (over delta and latency) for every linking setting.
#
# Every model below is fitted exactly as fit_components() does: the CV and C
# curves are fitted jointly to the empirical CV and C effects with one shared
# non-negative least-squares scale and one shared latency (0-50 steps).
#
# Tests
#   0. C alone: C curve fitted by itself with its own scale and latency
#      (an upper bound on how well the model's C shape can fit).
#   1. Linear response filter: both effect curves convolved with a causal,
#      normalized exponential (shape 1) or gamma-2 (shape 2) kernel with time
#      constant tau, i.e., a leaky integration of the competition signal.
#   2. Luce choice rule (Allopenna et al., 1998): for a two-alternative
#      display, P(competitor) = 1 / (1 + exp(k * (a_target - a_comp))); the
#      effect is P(competitor) - P(control), averaged over items, optionally
#      followed by the exponential filter.
#   3. Extended support (not a linking function): after its shared segments
#      end, each competitor and its control keep a fraction rho of their
#      input for `tail` more steps, applied identically to CV and C.

suppressPackageStartupMessages(library(phonActivR))

st    <- example_stimuli()
items <- st$items
on    <- st$onsets
feat  <- trace_features()
dat   <- lin2023_english
deltas <- seq(0, 30, 5)
seg    <- 10

# Raw activations (target, competitor, control) for every item. With
# rho = 0 this reproduces run_simulation() exactly (checked below).
act_run <- function(oc, ok, on1, off, rho = 0, tail = 0, ts = 101,
                    alpha = .08, gamma = .04, decay = .02) {
  ac <- ak <- at <- numeric(ts)
  pt <- pc <- pk <- 0   # resting state before step 1 (as in run_activation)
  for (t in seq_len(ts)) {
    wc <- if (t >= on1 && t < off) 1 else if (t >= off && t < off + tail) rho else 0
    wk <- if (t < off) 1 else if (t < off + tail) rho else 0
    at[t] <- min(1, max(0, pt + alpha * (1 - pt) - gamma * pc - gamma * pk - decay * pt))
    ac[t] <- min(1, max(0, pc + alpha * oc * wc * (1 - pc) - gamma * pt - gamma * pk - decay * pc))
    ak[t] <- min(1, max(0, pk + alpha * ok * wk * (1 - pk) - gamma * pt - gamma * pc - decay * pk))
    pt <- at[t]; pc <- ac[t]; pk <- ak[t]
  }
  list(t = at, c = ac, k = ak)
}

item_acts <- function(delta, rho = 0, tail = 0) {
  L <- S <- vector("list", nrow(items))
  for (i in seq_len(nrow(items))) {
    r <- items[i, ]
    ov <- function(w, type, sc = 1)
      compute_overlap(on[[r$target]], on[[w]], type, features = feat) * sc
    nl <- shared_segments(st$large_type, on[[r$large_comp]])
    ns <- shared_segments(st$small_type, on[[r$small_comp]])
    L[[i]] <- act_run(ov(r$large_comp, st$large_type),
                      ov(r$large_ctrl, st$large_type, .15),
                      1, 1 + nl * seg, rho, tail)
    S[[i]] <- act_run(ov(r$small_comp, st$small_type),
                      ov(r$small_ctrl, st$small_type, .15),
                      1 + delta, 1 + delta + ns * seg, rho, tail)
  }
  list(L = L, S = S)
}

lin_effect  <- function(lst) rowMeans(sapply(lst, function(a) a$c - a$k))
luce_effect <- function(lst, k) rowMeans(sapply(lst, function(a)
  1 / (1 + exp(k * (a$t - a$c))) - 1 / (1 + exp(k * (a$t - a$k)))))
kern_filter <- function(x, tau, shape = 1) {
  if (tau == 0) return(x)
  h <- (0:200)^(shape - 1) * exp(-(0:200) / tau); h <- h / sum(h)
  vapply(seq_along(x), function(i) sum(x[1:i] * h[i:1]), numeric(1))
}
r2 <- function(y, yh) 1 - sum((y - yh)^2) / sum((y - mean(y))^2)

fit_joint <- function(cv, cc, x, lags = 0:50) {
  y <- c(x$cv_effect, x$c_effect); best <- NULL
  for (L in lags) {
    m1 <- c(rep(0, L), cv)[1:101]; m2 <- c(rep(0, L), cc)[1:101]; m <- c(m1, m2)
    b <- max(0, sum(m * y) / sum(m * m)); rss <- sum((y - b * m)^2)
    if (is.null(best) || rss < best$rss)
      best <- list(rss = rss, lag = L, r2 = r2(y, b * m),
                   r2_cv = r2(x$cv_effect, b * m1), r2_c = r2(x$c_effect, b * m2))
  }
  best
}

groups <- lapply(split(dat, dat$group), function(x) x[order(x$time), ])

# Sanity check against the package
sim <- run_simulation(st, delta_values = 10, verbose = FALSE)
a10 <- item_acts(10)
stopifnot(max(abs(lin_effect(a10$S) - sim$results[["10"]]$small_effect)) < 1e-12,
          max(abs(lin_effect(a10$L) - sim$results[["10"]]$large_effect)) < 1e-12)

ACT <- lapply(deltas, item_acts); names(ACT) <- deltas
rows <- list()
add <- function(test, setting, cv, cc, delta) {
  for (g in names(groups)) {
    f <- fit_joint(cv, cc, groups[[g]])
    rows[[length(rows) + 1]] <<- data.frame(
      test = test, setting = setting, group = g, delta = delta, lag = f$lag,
      r2 = f$r2, r2_cv = f$r2_cv, r2_c = f$r2_c, rss = f$rss)
  }
}

# 0. C alone
for (d in deltas) for (g in names(groups)) {
  x <- groups[[g]]; cc <- lin_effect(ACT[[as.character(d)]]$S); best <- NULL
  for (L in 0:50) {
    m <- c(rep(0, L), cc)[1:101]; b <- max(0, sum(m * x$c_effect) / sum(m * m))
    rr <- r2(x$c_effect, b * m)
    if (is.null(best) || rr > best$r2) best <- list(r2 = rr, lag = L)
  }
  rows[[length(rows) + 1]] <- data.frame(test = "0 C alone", setting = "none",
    group = g, delta = d, lag = best$lag, r2 = NA, r2_cv = NA,
    r2_c = best$r2, rss = -best$r2)
}
# 1. Linear filter
for (shape in 1:2) for (tau in c(0, 2, 4, 6, 8, 10, 15, 20, 30)) for (d in deltas) {
  A <- ACT[[as.character(d)]]
  add("1 linear filter", sprintf("shape=%d tau=%g", shape, tau),
      kern_filter(lin_effect(A$L), tau, shape),
      kern_filter(lin_effect(A$S), tau, shape), d)
}
# 2. Luce choice (+ optional filter)
for (k in c(1, 3, 5, 8, 12, 20)) for (tau in c(0, 4, 8)) for (d in deltas) {
  A <- ACT[[as.character(d)]]
  add("2 Luce choice", sprintf("k=%g tau=%g", k, tau),
      kern_filter(luce_effect(A$L, k), tau), kern_filter(luce_effect(A$S, k), tau), d)
}
# 3. Extended support
for (rho in c(.25, .5, .75, 1)) for (tail in c(10, 20)) for (d in deltas) {
  A <- item_acts(d, rho, tail)
  add("3 extended support", sprintf("rho=%g tail=%g", rho, tail),
      lin_effect(A$L), lin_effect(A$S), d)
}

res <- do.call(rbind, rows)
write.csv(res, "linking_tests_results.csv", row.names = FALSE)

best <- do.call(rbind, lapply(split(res, list(res$test, res$setting, res$group), drop = TRUE),
                              function(q) q[which.min(q$rss), ]))
best <- best[order(best$group, best$test, -best$r2_c), ]
num <- c("r2", "r2_cv", "r2_c"); best[num] <- round(best[num], 3)
for (tst in unique(best$test)) {
  cat("\n==", tst, "(best delta per setting; sorted by r2_c) ==\n")
  print(best[best$test == tst, c("group", "setting", "delta", "lag", "r2", "r2_cv", "r2_c")],
        row.names = FALSE)
}
cat("\nMaximum r2_c per test and group:\n")
print(aggregate(r2_c ~ test + group, best, max))
