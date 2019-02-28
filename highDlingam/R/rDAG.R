# Generate DAG with set maxInDegree
# p - number of nodes
# n - number of samples
# maxInDeg - maximum number of samples
# dist - distribution of errors
rDAG <- function(p, n, maxInDegree = 4, dist = "unif", lowScale = 1, highScale = 1, lowEdge = .3, highEdge = 1){
  B <- matrix(0, nrow = p, ncol = p)
  for(i in 3:p){
    
    if(maxInDegree > 1){
      # randomly select number of parents between 1 and maxInDegree
      numParents <- sample.int(min(maxInDegree - 1, i - 2), size = 1)
      
      # ensure that i-1 is parent so that there is a unique ordering
      parents <- c(sample(1:(i-2), size = numParents))
      # set edge weights
      B[i, parents] <- 1/5 * sample(c(-1, 1), size = length(parents), replace = T)
    }
    
    B[i, i-1] <- runif(1, lowEdge, highEdge) * sample(c(-1, 1), size = 1)
  }
  B[2, 1] <- runif(1, lowEdge, highEdge) * sample(c(-1, 1), size = 1)
  
  if(dist == "gamma"){  
      errs <- matrix(rgamma(n * p, 2, sqrt(2)) - 2/ sqrt(2), nrow = n, ncol = p)
      scale.param <- runif(p, lowScale, highScale)
      errs <- t(t(errs) * scale.param)
  } else if (dist == "unif"){
      errs <- matrix(runif(n * p, -sqrt(3), sqrt(3)), nrow = n, ncol = p)
      scale.param <- runif(p, lowScale, highScale)
      errs <- t(t(errs) * scale.param)
  } else if(dist == "gauss"){
    errs <- matrix(rnorm(n * p), nrow = n, ncol = p)
    scale.param <- runif(p, lowScale, highScale)
    errs <- t(t(errs) * scale.param)
  } else if (dist == "t"){
      errs <- matrix(rt(n * p, df = 7) / sqrt(7 / 5), nrow = n, ncol = p)
      scale.param <- runif(p, lowScale, highScale)
      errs <- t(t(errs) * scale.param)
  }else if (dist == "dexp"){
      errs <- matrix(rmutil::rlaplace(n * p, s = 1 / sqrt(2)), nrow = n, ncol = p)
      scale.param <- runif(p, lowScale, highScale)
      errs <- t(t(errs) * scale.param)
    
  }
  Y <- solve(diag(rep(1, p)) - B, t(errs))
  Y <- t(Y)
  

  
  return(list(B = B, Y = Y, errs = errs, scale.param = scale.param))
}
