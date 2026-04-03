# STA2201 Final Project
# tSNE Grid Plots
# Authors: Evan Shamov and Ian Zhang
# Require: 'HAG1.csv' and 'CliffTor4.csv'; Require that tSNE script was run.
# Warning: Generating the CliffTor4 plot can take up to 1 hour.

old_par = par(no.readonly = TRUE)

################################################################################
# HAG
HAG1 = read.csv("HAG1.csv", header = TRUE)
X = as.matrix(HAG1[, c("x1", "x2")])

labels = factor(HAG1$class)
cols = c("#EC0B43", "#3FB9DE", "#FAA80F", "#77D321", "#715AFF")[labels]

iter_T = 1000
mom = c(rep(0.5, 250), rep(0.8, iter_T - 250))

# tSNE Parameter
ppx_vals = c(12, 15, 20, 25, 30)
eta_vals = c(0.03, 0.05, 0.075, 0.1, 0.15)

n_eta = length(eta_vals)
n_ppx = length(ppx_vals)
par(mfrow = c(n_eta, n_ppx), mar = c(3, 3, 3, 1), oma = c(0, 0, 2, 0))

seed = 2201
tol = 1e-4
max_iter = 1000
sigma0 = 1

for (eta in eta_vals) {
  for (ppx in ppx_vals) {
    cat("Running t-SNE with ppx =", ppx, "and eta =", eta, "\n")
    
    Y = tSNE(X = X, ppx = ppx, dim_map = 2, iter_T = iter_T, eta = eta, 
             momentum = mom, tol = tol, max_iter = max_iter, sigma0 = sigma0,
             seed = seed)
    
    plot(Y, col = cols, pch = 19, cex = 0.7,
      xlab = "t-SNE 1", ylab = "t-SNE 2",
      main = paste0("ppx = ", ppx, ", eta = ", eta))
  }
}
mtext("t-SNE parameter sweep", outer = TRUE, cex = 1.2)
par(old_par)

################################################################################
# CliffTor4
CliffTor4 = read.csv('CliffTor4.csv', header = TRUE)
X = as.matrix(CliffTor4[, c('x1', 'x2', 'x3', 'x4')])

labels = factor(CliffTor4$cluster)
cols = c("#EC0B43", "#3FB9DE", "#FAA80F", "#77D321", "#715AFF")[labels]

## tSNE Parameters
iter_T = 1000
mom = c(rep(0.5, 250), rep(0.8, iter_T - 250))

ppx_vals = c(8, 12, 20, 30)
eta_vals = c(0.1, 0.25, 1, 5)

n_eta = length(eta_vals)
n_ppx = length(ppx_vals)
par(mfrow = c(n_eta, n_ppx), mar = c(3, 3, 3, 1), oma = c(0, 0, 2, 0))

seed = 2201
tol = 1e-4
max_iter = 1000
sigma0 = 1

for (eta in eta_vals) {
  for (ppx in ppx_vals) {
    cat("Running t-SNE with ppx =", ppx, "and eta =", eta, "\n")
    
    Y = tSNE(X = X, ppx = ppx, dim_map = 2, iter_T = iter_T, eta = eta, 
             momentum = mom, tol = tol, max_iter = max_iter, sigma0 = sigma0,
             seed = seed)
    
    plot(Y, col = cols, pch = 19, cex = 0.7,
         xlab = "t-SNE 1", ylab = "t-SNE 2",
         main = paste0("ppx = ", ppx, ", eta = ", eta))
  }
}
mtext("t-SNE parameter sweep", outer = TRUE, cex = 1.2)
par(old_par)