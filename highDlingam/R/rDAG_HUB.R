# Generate DAG with set maxInDegree
# p - number of nodes
# n - number of samples
# maxInDeg - maximum number of samples
# dist - distribution of errors
rDAG_hub <- function(p, n, lowScale = .8, highScale = 1, lowEdge = .5, highEdge = 1, nHubs = 3, hubEdge = .2){
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

