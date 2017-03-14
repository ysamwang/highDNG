# Generate DAG with set maxInDegree
# p - number of nodes
# n - number of samples
# maxInDeg - maximum number of samples
# dist - distribution of errors

rDAG_degree <- function(p, n, maxInDeg = 4, dist = "gamma"){
  B <- matrix(0, nrow = p, ncol = p)
  for(i in 2:p){
    
    # randomly select number of parents between 1 and maxInDeg
    numParents <- min(sample.int(maxInDeg, size = 1), i - 1) - 1
    
    # ensure that i-1 is parent so that there is a unique ordering
    parents <- c(sample(1:(i-2), size = numParents), i-1)
    
    # set edge weights
    B[i, parents] <- runif(numParents + 1, .2, .95) * ((runif(numParents + 1) < .5) * 2 - 1)
  }
  
  if(dist == "gamma"){  
  avec <- rgamma(p, 1, 1) + 1
  bvec <- rgamma(p, 1, 1) + 1
  errs <- mapply(function(a,b,n){rgamma(n, a, b) - a/b}, avec, bvec, n)
  } else if (dist == "unif"){
    errs <- matrix(runif(n * p, -1, 1), nrow = n, ncol = p)
  }
  Y <- solve(diag(rep(1,p)) - B, t(errs))
  return(list(B = B, Y = Y))
}




