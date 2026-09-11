###############################################################################
# Build the two probability matrices for Exercise 2.1
###############################################################################
N <- 8
pop <- 1:N
nonadj <- function(i, N = 8) {
  nbrs <- c(i - 1, i, i + 1)
  setdiff(1:N, nbrs[nbrs >= 1 & nbrs <= N])
}

# enumerate valid samples and their probabilities
samples <- do.call(rbind, lapply(1:(N - 1), function(i) {
  do.call(rbind, lapply((i + 1):N, function(j) {
    if (abs(i - j) > 1) c(i, j) else NULL
  }))
}))
sample_probs <- apply(samples, 1, function(s)
  (1/N) * (1/length(nonadj(s[1]))) + (1/N) * (1/length(nonadj(s[2]))))

# ---- first-order inclusion probabilities  pi_i  ------------------------------
pi1 <- sapply(pop, function(i)
  sum(sample_probs[samples[, 1] == i | samples[, 2] == i]))
names(pi1) <- pop
cat("pi_i =\n"); print(round(pi1, 6))

# ---- second-order inclusion probabilities  pi_ij  ---------------------------
pi2 <- matrix(0, N, N)
for (k in seq_len(nrow(samples))) {
  i <- samples[k, 1]; j <- samples[k, 2]
  pi2[i, j] <- pi2[j, i] <- pi2[i, j] + sample_probs[k]
}
dimnames(pi2) <- list(i = pop, j = pop)
cat("\npi_ij =\n"); print(round(pi2, 6))

# ---- covariance matrix of membership indicators  ---------------------------
Cov_I <- pi2 - outer(pi1, pi1)
diag(Cov_I) <- pi1 * (1 - pi1)
dimnames(Cov_I) <- list(i = pop, j = pop)
cat("\nCov(I_i, I_j) =\n"); print(round(Cov_I, 6))

# ---- dump to CSV ------------------------------------------------------------
write.csv(round(pi1, 8),         file = "pi1.csv",  row.names = TRUE)
write.csv(round(pi2, 8),         file = "pi2.csv",  row.names = TRUE)
write.csv(round(Cov_I, 8),       file = "cov.csv",  row.names = TRUE)
cat("\nWritten: pi1.csv, pi2.csv, cov.csv\n")
