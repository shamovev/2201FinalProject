# STA2201 Final Project
# Generating Data on the Clifford Torus
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov
# Output: CliffTor4.csv

library(circular)
library(plotly)

set.seed(20937635)

################################################################################
# Generate von Mises Mixture
vonMisesMix = function(n, pi_vec, mu_vec, zeta_vec, nu_vec, eta_vec){
  K = length(pi_vec)
  Z = sample.int(K, size = n, replace = TRUE, prob = pi_vec)
  A = rep(Inf, n)
  B = rep(Inf, n)
  
  for (k in 1:K){
    nk = sum(Z == k)
    if (nk > 0){
      A[Z == k] = as.numeric(rvonmises(nk, circular(mu_vec[k]), zeta_vec[k]))
      B[Z == k] = as.numeric(rvonmises(nk, circular(nu_vec[k]), eta_vec[k]))
    }
  }
  return(list(alpha = A, beta = B, cluster = Z))
}

# Embed into R4
Pushforward = function(a, b, cluster){
  x1 = cos(a)/sqrt(2)
  x2 = sin(a)/sqrt(2)
  x3 = cos(b)/sqrt(2)
  x4 = sin(b)/sqrt(2)
  return(list(x1 = x1, x2 = x2, x3 = x3, x4 = x4, cluster = cluster))
}

################################################################################
# Parameters for von Mises
pi_vec = rep(1,4)/4
mu_vec = c(2*pi - 0.5, pi, 6*pi/5, 7*pi/6)
zeta_vec = c(75, 15, 10, 50)
nu_vec = c(pi/2 - 0.25, 3/4 - pi/2, pi, pi/4)
eta_vec = c(75, 10, 15, 50)

# Parameters for Curve
n_add = 500
A_add = seq(from = -pi, to = pi, length.out = n_add)/2
B_add = seq(from = -pi, to = pi, length.out = n_add)*1.5
cluster_add = rep(5, n_add)

################################################################################
# Generate vMM and attach curve
n_clust = 1250
vMM = vonMisesMix(n_clust, pi_vec, mu_vec, zeta_vec, nu_vec, eta_vec)

CliffTor4 = Pushforward(c(vMM$alpha, A_add), 
                        c(vMM$beta, B_add), 
                        c(vMM$cluster, cluster_add))

# Add White Noise (Optional)
noise_sd = 0.1
CliffTor4$x1 = CliffTor4$x1 + rnorm(n_clust + n_add, mean = 0, sd = noise_sd)
CliffTor4$x2 = CliffTor4$x2 + rnorm(n_clust + n_add, mean = 0, sd = noise_sd)
CliffTor4$x3 = CliffTor4$x3 + rnorm(n_clust + n_add, mean = 0, sd = noise_sd)
CliffTor4$x4 = CliffTor4$x4 + rnorm(n_clust + n_add, mean = 0, sd = noise_sd)

CliffTor4_df = data.frame(x1 = CliffTor4$x1, 
                          x2 = CliffTor4$x2, 
                          x3 = CliffTor4$x3, 
                          x4 = CliffTor4$x4, 
                          cluster = CliffTor4$cluster)

# Export File
write.csv(CliffTor4_df, 'CliffTor4.csv', row.names = FALSE)