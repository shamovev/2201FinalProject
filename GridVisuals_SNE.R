# STA2201 Final Project
# SNE Grid Plots
# Authors: Evan Shamov and Ian Zhang
# Require: 'HAG1.csv' and 'CliffTor4.csv'; Require that SNE script was run.
# Warning: Generating the CliffTor4 plot can take up to 1 hour.

old_par = par(no.readonly = TRUE)

################################################################################
# HAG
HAG1 = read.csv("HAG1.csv", header = TRUE)
X = as.matrix(HAG1[, c("x1", "x2")])

labels = factor(HAG1$class)
cols = c("#EC0B43", "#3FB9DE", "#FAA80F", "#77D321", "#715AFF")[labels]

# SNE Parameters - SWEEP
ppx_vals = c(12, 15, 20, 25, 30)
eta_vals = c(0.03, 0.05, 0.075, 0.1, 0.15)

n_eta = length(eta_vals)
n_ppx = length(ppx_vals)
par(mfrow = c(n_eta, n_ppx), mar = c(3, 3, 3, 1), oma = c(0, 0, 2, 0))

seed = 2201
tol = 1e-4
max_iter = 500
sigma0 = 1
mom = c(rep(0.5, 125), rep(0.8, iter_T - 125))

for (eta in eta_vals) {
  for (ppx in ppx_vals) {
    cat("Running SNE with ppx =", ppx, "and eta =", eta, "\n")
    
    Y = SNE(X = X, ppx = ppx, dim_map = 2, iter_T = iter_T, eta = eta, 
            momentum = mom, tol = tol, max_iter = max_iter, sigma0 = sigma0,
            seed = seed)
    
    plot(Y, col = cols, pch = 19, cex = 0.7,
         xlab = "SNE 1", ylab = "SNE 2",
         main = paste0("ppx = ", ppx, ", eta = ", eta))
  }
}
mtext("SNE parameter sweep", outer = TRUE, cex = 1.2)
par(old_par)

# SNE Parameters - CROWDING
ppx_vals = c(12, 15, 20, 25, 30)
eta_vals = c(0.002)

n_eta = length(eta_vals)
n_ppx = length(ppx_vals)
par(mfrow = c(n_eta, n_ppx), mar = c(3, 3, 3, 1), oma = c(0, 0, 2, 0))

for (eta in eta_vals) {
  for (ppx in ppx_vals) {
    cat("Running SNE with ppx =", ppx, "and eta =", eta, "\n")
    
    Y = SNE(X = X, ppx = ppx, dim_map = 2, iter_T = iter_T, eta = eta, 
            momentum = mom, tol = tol, max_iter = max_iter, sigma0 = sigma0,
            seed = seed)
    
    plot(Y, col = cols, pch = 19, cex = 0.7,
         xlab = "SNE 1", ylab = "SNE 2",
         main = paste0("ppx = ", ppx, ", eta = ", eta))
  }
}
mtext("SNE parameter sweep", outer = TRUE, cex = 1.2)
par(old_par)

################################################################################
# CliffTor4
ppx_vals = c(12, 15, 20, 25, 30)
eta_vals = c(0.03, 0.05, 0.075, 0.1)

n_eta = length(eta_vals)
n_ppx = length(ppx_vals)
par(mfrow = c(n_eta, n_ppx), mar = c(3, 3, 3, 1), oma = c(0, 0, 2, 0))

iter_T = 500

seed = 2201
tol = 1e-4
max_iter = 500
sigma0 = 1
mom = c(rep(0.5, 125), rep(0.8, iter_T - 125))

for (eta in eta_vals) {
  for (ppx in ppx_vals) {
    cat("Running SNE with ppx =", ppx, "and eta =", eta, "\n")
    
    Y = SNE(X = X, ppx = ppx, dim_map = 2, iter_T = iter_T, eta = eta, 
            momentum = mom, tol = tol, max_iter = max_iter, sigma0 = sigma0,
            seed = seed)
    
    plot(Y, col = cols, pch = 19, cex = 0.7,
         xlab = "SNE 1", ylab = "SNE 2",
         main = paste0("ppx = ", ppx, ", eta = ", eta))
  }
}
mtext("SNE Parameter Sweep", outer = TRUE, cex = 1.2)
par(old_par)