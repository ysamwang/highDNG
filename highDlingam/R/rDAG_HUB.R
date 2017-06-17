# Generate DAG with set maxInDegree
# p - number of nodes
# n - number of samples
# maxInDeg - maximum number of samples
# dist - distribution of errors
rDAG_hub <- function(p, n, lowScale = 1, highScale = 1, lowEdge = .3, highEdge = 1, numHubs = 3){
  B <- matrix(0, nrow = p, ncol = p)
  B[matrix(c(2:p, 1:(p-1)), ncol = 2)] <- runif(p - 1, lowEdge, highEdge) * sample(c(-1, 1), size = p - 1, replace = T)
  hubParent <- sample(c(1:numHubs), size = p - numHubs, replace = T)
  B[matrix(c((numHubs + 1):p, hubParent), ncol = 2)] <- 1/5 * sample(c(-1, 1), size = p - numHubs, replace = T)
  
  errs.unif <- matrix(runif(n * p, -sqrt(3), sqrt(3)), nrow = n, ncol = p)
  scale.param <- runif(p, lowScale, highScale)
  errs.unif <- t(t(errs.unif) * scale.param)
    
  errs.gauss <- matrix(rnorm(n * p), nrow = n, ncol = p)
  errs.gauss <- t(t(errs.gauss) * scale.param)
    
  Y.gauss <- solve(diag(rep(1,p)) - B, t(errs.gauss))
  Y.gauss <- t(Y.gauss)
  
  Y.unif <- solve(diag(rep(1,p)) - B, t(errs.unif))
  Y.unif <- t(Y.unif)
  
  return(list(B = B, Y.gauss = Y.gauss, Y.unif = Y.unif))
}
