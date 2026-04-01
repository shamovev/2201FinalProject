# STA2201 Final Project
# Implementation of tSNE/SNE
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov
# Descr: This program implements the sigma bisection described in p.4 and also
# does the first step in the begin loop of Alg.1

# X = matrix(data = rnorm(100*10, mean= 0, sd = 5), nrow = 100, ncol = 10)

# Input: p, a probability distribution vector.
# Output: H(p)
ShannonEntropy_row = function(p){
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
    p_num = exp(-D / (2*sigma^2))
    p_denom = sum(p_num)
    p = p_num/p_denom
    
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
    x = X[row, ]
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

ComputeAffn_q = function(Y){
  rowNorm_sq = rowSums(Y^2)
  D = outer(rowNorm_sq, rowNorm_sq, '+') - 2 * Y %*% t(Y)
  D[D < 0] = 0
  
  E = exp(-D)
  diag(E) = 0 # we require qii = 0, see p5.
  Q = E / sum(E)
  
  return(Q)
}

