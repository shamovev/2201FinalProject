# STA2201 Final Project
# Implementation of tSNE/SNE
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov

# Input: p, a probability distribution vector.
# Output: H(p)
ShannonEntropy_row = function(p){
  p = p[p > 0]
  # Assume that p_i = (p_{j|i})_i
  Hp = -sum(p*log(p, base = 2))
  return(Hp)
}

# Input: p, a probability distribution vector.
# Output:  Perp(p)
Perplexity_row = function(p){
  p = p[p>0]
  # Assume that p_i = (p_{j|i})_i
  Perpp = 2^(ShannonEntropy_row(p))
  return(Perpp)
}

# Input:
# - i: row of X.
# - X: Data matrix with rows corresponding to observations.
# - ppx: Perplexity. Should be kept between 5 and 50.
# Output:
# - SIGMA: sigma producing a conditional distribution P_i with desired ppx.
# - P: conditional distribution P_i with desired ppx.
SigmaBisection_row = function(j, X, ppx, tol = 1e-6, max_iter = 100, sigma0 = 1){
  Converged = FALSE
  x = X[j, ]
  D = apply(X, MARGIN = 1, FUN = function(y){sum((x-y)^2)})
  
  sigma = sigma0
  sigma_min = 0
  sigma_max = Inf
  
  for (i in 1:max_iter){
    # Compute Conditional Prob Dstbn, P_i
    p_num = exp(-D / (2 * sigma^2))
    p_num[j] = 0
    p_denom = sum(p_num)
    p = p_num / p_denom
    
    # Compute Shannon Entropy of P_i
    H = ShannonEntropy_row(p)
    
    # If sufficiently close, break.
    if (abs(H - log(ppx, 2)) < tol){
      Converged = TRUE
      break
    }
    # If H is too low, increase sigma.
    else if (H - log(ppx, 2) < 0){
      sigma_min = sigma
      if (is.infinite(sigma_max)){
        sigma = 2*sigma
      }
      else{
        sigma = (sigma + sigma_max) / 2
      }
    }
    # If H is too high, decrease it.
    else if (H - log(ppx, 2) > 0){
      sigma_max = sigma
      sigma = (sigma + sigma_min) / 2
    }
  }
  
  p_num = exp(-D / (2*sigma^2))
  p_num[j] = 0
  p_denom = sum(p_num)
  p = p_num/p_denom
  
  Output = list(SIGMA = sigma, P = p, PPX = 2^(ShannonEntropy_row(p)),
                iterations = i, converged = Converged)
  return(Output)
}

# Input:
# - X: Data matrix with rows corresponding to observations.
# - ppx: Perplexity. Should be kept between 5 and 50.
# Output: Matrix of pairwise affinities, [p_{j|i}]_ij.
ComputeAffn_p = function(X, ppx, tol = 1e-6, max_iter = 100, sigma0 = 1){
  n = dim(X)[1]
  P = matrix(data = NA, nrow = n, ncol = n)
  # P_ij = p_{j|i}
  for (row in 1:n){
    Bisectn_Output = SigmaBisection_row(row, X, ppx, tol, max_iter, sigma0)
    P[row, ] = Bisectn_Output$P
  }
  diag(P)=0
  return(P)
}

# Input: Matrix of pairwise affinities, [p_{j|i}]_ij.
# Output: Matrix of symmetrized affinities, [p_{ij}]_{ij}.
Symmetrize_p = function(P){
  n = dim(P)[1]
  P_symm = (P + t(P)) / (2*n)
  P_symm = P_symm/sum(P_symm)
  return(P_symm)
}

# Input: Matrix of pairwise affinities, [p_{j|i}]_ij.
# Output: Matrix of symmetrized affinities, [p_{ij}]_{ij}.
ComputeAffn_q = function(Y){
  rowNorm_sq = rowSums(Y^2)
  D = outer(rowNorm_sq, rowNorm_sq, '+') - 2 * Y %*% t(Y)
  D[D < 0] = 0
  
  E = 1/(1+D) # Use t1
  diag(E) = 0 # we require qii = 0, see p5.
  Q = E / sum(E)
  
  return(Q)
}

# Input:
# - Y: Current points in mapping.
# - P: Symmetrized probabilities [pij]
# - Q: Affinities in Y. See eq (5).
# Output: Gradient at Y.
ComputeGradient = function(Y, P, Q){
  n = dim(Y)[1]
  d = dim(Y)[2] # Can be 2 or 3.
  grad = matrix(data = NA, nrow = n, ncol = d)
  
  for (i in 1:n){
    D = Y[i,] - Y
    D2 = rowSums(D^2)
    W = (P[i, ] - Q[i, ])/(1 + D2)
    grad[i, ] = 4* colSums(D * W)
  }
  
  return(grad)
}

# Input:
# - X: Data Matrix.
# - ppx: Perplexity.
# - dim_map: Dimension of desired mapping.
# - iter_T: Number of iterations.
# - eta: Learning rate.
# - momentum. Vector of length iter_T containing momentum update schedule.
# Output: Mapped points Y.
tSNE = function(X, ppx, dim_map, iter_T, eta, momentum, tol = 1e-6, max_iter = 1000, sigma0 = 1){
  P = ComputeAffn_p(X, ppx, tol = tol, max_iter = max_iter, sigma0 = sigma0)
  P = Symmetrize_p(P)
  n = dim(X)[1]
  Yinit = matrix(rnorm(n * dim_map, mean = 0, sd = 0.01), nrow = n)
  # Y0 = Yt, Y1 = Y(t-1), Y2 = Y(t-2)
  Y0 = Yinit
  Y1 = Yinit
  Y2 = Yinit
  
  for (t in 1:iter_T){
    Y2 = Y1
    Y1 = Y0
    
    Q = ComputeAffn_q(Y1)
    
    if(t < 250){
      Grad = ComputeGradient(Y1, 4*P, Q)
    }
    else{
      Grad = ComputeGradient(Y1, P, Q)
    }
    
    Y0 = Y1 - eta*Grad + momentum[t]*(Y1 - Y2)
    Y0 = sweep(Y0, 2, colMeans(Y0))
    print(t)
  }
  return(Y0)
}

################################################################################
# Test - Our Implementation on CliffTor4
CliffTor4 = read.csv(file = 'CliffTor4.csv', header = TRUE)
X = as.matrix(CliffTor4[, c("x1", "x2", "x3", "x4")])
labels = factor(CliffTor4$cluster)

iter_T = 2000
mom = c(rep(0.5, 50), rep(0.8, 1950))
Y = tSNE(X, ppx = 30, dim_map = 2, iter_T = iter_T, eta = 25, momentum = mom, tol = 1e-4)

cols = c('#EC0B43', '#3FB9DE', '#FAA80F','#77D321', '#715AFF')[labels]

plot(Y, col = cols, pch = 19, xlab = "t-SNE 1", ylab = "t-SNE 2",
  main = "t-SNE of CliffordTorus4")

# Test - tsne Implementation on CliffTor4
library(tsne)
set.seed(1)

Ynew = tsne(X, k = 2, perplexity = 30, max_iter = 1000, whiten = FALSE)
plot(Ynew, col = cols, pch = 19, xlab = "t-SNE 1", ylab = "t-SNE 2",
     main = "t-SNE of CliffordTorus4")
