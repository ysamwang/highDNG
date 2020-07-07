#' Causal discovery with high dimensional non-Gaussian data
#' 
#' 
#' Generates data corresponding to a Hub graph as described in Wang and Drton (2019)
#' 
#'  
#' @param p the number of variables in the graph
#' @param n the number of samples of the p variables
#' @param lowScale the standard deviation for the error term for each node is drawn uniformly from [lowScale, highScale]
#' @param highScale the standard deviation for the error term for each node is drawn uniformly from [lowScale, highScale]
#' @param lowEdge the coefficient for each edge v -> v+1 is drawn uniformly from +/-[lowEdge, highEdge]
#' @param highEdge the coefficient for each edge v -> v+1 is drawn uniformly from +/-[lowEdge, highEdge]
#' @param nHubs the number of hub nodes
#' @param hubEdge the coefficient for each edge from a hub to a downstream node is set to hubEdge. This should be appropriately small
#'    so the variance of Y does not blow up
#' @param p
#' @return \item{B}{the random coefficient matrix}
#'    \item{Y.unif}{the sampled data}
#'    \item{scale.param}{the error standard deviations}

rDAG_hub <- function(p, n, lowScale = .8, highScale = 1, lowEdge = .65, highEdge = 1, nHubs = 3, hubEdge = .2){
  B <- matrix(0, nrow = p, ncol = p)
  
  # assign each non-hub to be the child of a hub
  hub.assignment <- matrix(c((nHubs + 1):p,
                             sample(c(1:nHubs), size = (p - nHubs), replace = T)),
                           ncol = 2)
  
  
  B[hub.assignment] <- sample(c(-hubEdge, hubEdge), size = p-nHubs, replace = T)
  
  # ensure unique ordering
  B[matrix(c(2:p, 1:(p-1)), ncol = 2)] <- runif(p-1, lowEdge, highEdge) * sample(c(-1, 1), size = p-1, replace = T)
  
  errs.unif <- matrix(runif(n * p, -sqrt(3), sqrt(3)), nrow = n, ncol = p)
  scale.param <- runif(p, lowScale, highScale)
  errs.unif <- t(t(errs.unif) * scale.param)
  
  
  Y.unif <- solve(diag(rep(1,p)) - B, t(errs.unif))
  Y.unif <- t(Y.unif)
  
  return(list(B = B, Y.unif = Y.unif, scale.param = scale.param))
}

