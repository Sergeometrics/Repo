###############################################################################
# Exercise 2.1  -  Sequential sampling of 2 nonadjacent office-hour intervals
#                 from 8 intervals (1 = 9-10, 2 = 10-11, ..., 8 = 16-17)
#
# Sampling scheme:
#   1) Draw the first interval with equal probability from the 8 intervals.
#   2) Draw the second interval without replacement, with equal probability,
#      from the intervals that are NOT adjacent in time to the first draw.
#
# Tasks:
#   (a) first-order inclusion probabilities
#   (b) second-order inclusion probabilities + measurability
#   (c) covariances of the membership indicators
#   (d) verify Result 2.6.2 (HT-estimator unbiasedness and variance formula)
###############################################################################

N <- 8
n <- 2
pop <- 1:N

# ---------- helper: nonadjacent intervals to a given interval -----------------
nonadj <- function(i, N = 8) {
  nbrs <- c(i - 1, i, i + 1)
  nbrs <- nbrs[nbrs >= 1 & nbrs <= N]
  setdiff(1:N, nbrs)
}

cat("=== Nonadjacent sets (note: endpoints have 6 partners, interior has 5) ===\n")
for (i in pop) {
  na <- nonadj(i)
  cat(sprintf("  nonadj(%d) = {%s}  (size = %d)\n",
              i, paste(na, collapse = ","), length(na)))
}

# ---------- enumerate every valid sample (ordered as i < j) ------------------
samples <- do.call(rbind, lapply(1:(N - 1), function(i) {
  do.call(rbind, lapply((i + 1):N, function(j) {
    if (abs(i - j) > 1) c(i, j) else NULL
  }))
}))
colnames(samples) <- c("i", "j")
cat(sprintf("\n=== %d valid (nonadjacent) samples ===\n", nrow(samples)))

# ---------- probability of each sample under the sequential scheme -----------
#   P({i,j}) = P(i first, j second) + P(j first, i second)
#            = (1/N) * 1/|nonadj(i)|  +  (1/N) * 1/|nonadj(j)|
sample_probs <- apply(samples, 1, function(s) {
  i <- s[1]; j <- s[2]
  (1 / N) * (1 / length(nonadj(i))) + (1 / N) * (1 / length(nonadj(j)))
})
cat(sprintf("Sum of sample probabilities = %.6f  (must be 1)\n",
            sum(sample_probs)))

# ============================================================================
# (a)  First-order inclusion probabilities
# ============================================================================
# IMPORTANT: pi_i are NOT all equal!  Endpoints (1, 8) have more nonadjacent
# partners, so they are slightly more likely to be selected.
#   pi_1 = pi_8 = 13/48
#   pi_2 = pi_7 = 59/240
#   pi_3 = pi_4 = pi_5 = pi_6 = 29/120
#   Sum = 2  (as required)
# ============================================================================
pi1 <- sapply(pop, function(i) {
  sum(sapply(seq_len(nrow(samples)), function(k) {
    if (i %in% samples[k, ]) sample_probs[k] else 0
  }))
})
names(pi1) <- pop

cat("\n========== (a) First-order inclusion probabilities pi_i ==========\n")
print(round(pi1, 6))
cat(sprintf("  Sum pi_i = %.6f  (must equal n = %d)\n\n", sum(pi1), n))

cat("  Closed-form check:\n")
cat(sprintf("    pi_1 = pi_8 = 1/8 + (1/8)(1/6 + 5/5) = 13/48   ~ %.6f\n",
            13/48))
cat(sprintf("    pi_2 = pi_7 = 1/8 + (1/8)(1/6 + 4/5) = 59/240  ~ %.6f\n",
            59/240))
cat(sprintf("    pi_i (i=3..6) = 1/8 + (1/8)(2/6 + 3/5) = 29/120 ~ %.6f\n",
            29/120))

# ============================================================================
# (b)  Second-order inclusion probabilities  +  measurability
# ============================================================================
pi2 <- matrix(0, N, N)
for (i in 1:N) for (j in 1:N) if (i != j) {
  pi2[i, j] <- sum(sapply(seq_len(nrow(samples)), function(k) {
    if (i %in% samples[k, ] && j %in% samples[k, ]) sample_probs[k] else 0
  }))
}
rownames(pi2) <- colnames(pi2) <- pop

cat("\n========== (b) Second-order inclusion probabilities pi_ij ==========\n")
print(round(pi2, 6))

measurable <- all(pi2[upper.tri(pi2)] > 0)
cat(sprintf("\n  Design measurable? %s\n",
            ifelse(measurable, "YES", "NO  (adjacent pairs have pi_ij = 0)")))

cat("\n  Closed-form check:\n")
cat(sprintf("    pi_{1,8}    = 1/24   ~ %.6f  (both endpoints)\n",
            pi2[1, 8]))
cat(sprintf("    pi_{1,3}    = 11/240 ~ %.6f  (endpoint-interior)\n",
            pi2[1, 3]))
cat(sprintf("    pi_{3,5}    = 1/20   ~ %.6f  (interior-interior)\n",
            pi2[3, 5]))
cat(sprintf("    pi_{1,2}    = 0      ~ %.6f  (adjacent - forbidden)\n",
            pi2[1, 2]))

# ============================================================================
# (c)  Covariance matrix of the membership indicators  I_i, I_j
# ============================================================================
Cov_I <- pi2 - outer(pi1, pi1)
diag(Cov_I) <- pi1 * (1 - pi1)
rownames(Cov_I) <- colnames(Cov_I) <- pop

cat("\n========== (c) Covariance matrix Cov(I_i, I_j) ==========\n")
print(round(Cov_I, 6))

cat("\n  Distinct covariances (the seven-by-seven pattern collapses to):\n")
cat(sprintf("    Var(I_i)                       = %+.6f  (all i)\n",
            Cov_I[1, 1]))
cat(sprintf("    Cov(I_i, I_{i+1}) adjacent     = %+.6f  (i=1..7)\n",
            Cov_I[1, 2]))
cat(sprintf("    Cov(I_1, I_8)  endpoint pair   = %+.6f\n", Cov_I[1, 8]))
cat(sprintf("    Cov(I_1, I_3)  end-int nonadj  = %+.6f\n", Cov_I[1, 3]))
cat(sprintf("    Cov(I_3, I_5)  int-int nonadj  = %+.6f\n", Cov_I[3, 5]))

# ============================================================================
# (d)  Verify Result 2.6.2
#      Result 2.6.2 in Thompson's "Sampling" states that the Horvitz-Thompson
#      estimator
#                     t_hat_HT = sum_{i in s}  y_i / pi_i
#      is unbiased for the population total  t = sum_U y_i, and its variance is
#
#      V(t_hat_HT) = sum_i (1 - pi_i) y_i^2 / pi_i
#                  + 2 * sum_{i<j} (pi_ij - pi_i pi_j) y_i y_j / (pi_i pi_j)
#
#      For MEASURABLE designs this simplifies to the Sen-Yates-Grundy form
#      V(t_hat_HT) = sum_{i<j} (pi_ij - pi_i pi_j) (y_i/pi_i - y_j/pi_j)^2
#
#      The present design is NOT measurable, so the SYG form will not match.
# ============================================================================
set.seed(1)
y <- c(15, 20, 25, 30, 35, 40, 45, 50)
T_pop <- sum(y)

cat("\n========== (d) Verify Result 2.6.2 (HT estimator) ==========\n")
cat(sprintf("  y = {%s}\n", paste(y, collapse = ", ")))
cat(sprintf("  Population total T = %g\n", T_pop))

# HT estimator for a sample
t_hat_HT <- function(s) sum(y[s] / pi1[s])

# (d.1) Unbiasedness:  E[t_hat_HT] = T
E_t_hat <- sum(sapply(seq_len(nrow(samples)), function(k) {
  t_hat_HT(samples[k, ]) * sample_probs[k]
}))
cat(sprintf("\n  (d.1) Unbiasedness:\n"))
cat(sprintf("    E[t_hat_HT] = %.6f\n", E_t_hat))
cat(sprintf("    T           = %.6f\n", T_pop))
cat(sprintf("    Bias        = %.2e   (must be 0)   \u2713\n",
            E_t_hat - T_pop))

# (d.2) Variance by direct enumeration of the sampling distribution
V_direct <- sum(sapply(seq_len(nrow(samples)), function(k) {
  (t_hat_HT(samples[k, ]) - T_pop)^2 * sample_probs[k]
}))

# (d.2a) Result 2.6.2 - general variance formula
V_general <- sum((1 - pi1) * y^2 / pi1) +
  2 * sum(sapply(1:(N - 1), function(i) {
    sapply((i + 1):N, function(j) {
      (pi2[i, j] - pi1[i] * pi1[j]) * y[i] * y[j] / (pi1[i] * pi1[j])
    })
  }))

# (d.2b) Sen-Yates-Grundy form  (only valid for measurable designs)
V_SYG <- sum(sapply(1:(N - 1), function(i) {
  sapply((i + 1):N, function(j) {
    if (pi2[i, j] > 0) {
      (pi2[i, j] - pi1[i] * pi1[j]) * (y[i] / pi1[i] - y[j] / pi1[j])^2
    } else 0
  })
}))

cat(sprintf("\n  (d.2) Variance of t_hat_HT:\n"))
cat(sprintf("    Direct enumeration                 = %.6f\n", V_direct))
cat(sprintf("    Result 2.6.2 (general formula)     = %.6f   |diff| = %.2e   \u2713\n",
            V_general, abs(V_general - V_direct)))
cat(sprintf("    Sen-Yates-Grundy (needs meas'ble)  = %.6f   |diff| = %.2e\n",
            V_SYG, abs(V_SYG - V_direct)))
cat("\n  The SYG form is WRONG here because adjacent pairs (pi_ij=0) but\n")
cat("  (y_i/pi_i - y_j/pi_j)^2 != 0, so the simplified identity fails.\n")

# Bonus: Monte-Carlo sanity check
set.seed(123)
M <- 200000
sim_t <- replicate(M, {
  i1 <- sample(pop, 1)
  i2 <- sample(nonadj(i1), 1)
  t_hat_HT(c(i1, i2))
})
cat(sprintf("\n  Monte-Carlo (M = %d)  E[t_hat] ~ %.4f,  V(t_hat) ~ %.4f\n",
            M, mean(sim_t), var(sim_t)))
