#' Causal discovery with high dimensional non-Gaussian data
#' 
#' 
#' Simulates data corresponding to a graph with hubs
#' 
#'  
#' @param p The number of variables
#' @param n The number of samples
#' @param maxInDegree the maximum number of parents any node cna have
#' @param dist the distribution of the errors. Optios are:
#' \itemize{
#' \item  "gamma" for gamma distribution
#' \item "unif" for uniform distribution
#' \item "gauss" for Gaussian distribution
#' \item  "t" for T distribution with 7 df
#' \item "dexp" for Laplace distribution
#' }
#' @param lowScale smallest possible value error standard deviations
#' @param highScale largest possible value for error standard deviation
#' @param lowEdge smallest possible value (in abs value) for linear coefficient
#' @param highEdge largest possible value (in abs value) for linear coefficient 
#' @return \item{B}{true linear coefficients}
#'    \item{Y}{realized values}
#'    \item{errs}{the idiosyncratic errors}
#'    \item{scale.param}{the sd of each error variable}
rDAG_hub_tiered <- function(p, n, lowScale = 1, highScale = 1, lowEdge = .3, highEdge = 1, numHubs = 3){
  B <- matrix(0, nrow = p, ncol = p)
  # fill in hubs
  hubs <- floor(p / numHubs * c(0:(numHubs - 1))) + 1
  hubParent <- sapply(1:p, FUN = function(x){max(hubs[hubs <= x])})
  B[matrix(c(1:p, hubParent), ncol = 2)] <- 1/5 * sample(c(-1, 1), size = p, replace = T)
  # take out diag since hub will be it's own parent from above construction
  diag(B) <- rep(0, p)
  B[matrix(c(2:p, 1:(p-1)), ncol = 2)] <- runif(p - 1, lowEdge, highEdge) * sample(c(-1, 1), size = p - 1, replace = T)
  
  
  
  errs.unif <- matrix(runif(n * p, -sqrt(3), sqrt(3)), nrow = n, ncol = p)
  scale.param <- runif(p, lowScale, highScale)
  errs.unif <- t(t(errs.unif) * scale.param)
    
  Y.unif <- solve(diag(rep(1,p)) - B, t(errs.unif))
  Y.unif <- t(Y.unif)
  
  return(list(B = B, Y.unif = Y.unif, errs = errs.unif))
}
