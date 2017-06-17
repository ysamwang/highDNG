# Generate DAG with set maxInDegree
# p - number of nodes
# n - number of samples
# maxInDeg - maximum number of samples
# dist - distribution of errors
rDAG_degree <- function(p, n, maxInDeg = 4, dist = "gamma", varyScale = F){
  B <- matrix(0, nrow = p, ncol = p)
  for(i in 3:p){
    
    if(maxInDeg > 1){
      # randomly select number of parents between 1 and maxInDeg
      numParents <- sample.int(min(maxInDeg - 1, i - 2), size = 1)
      
      # ensure that i-1 is parent so that there is a unique ordering
      parents <- c(sample(1:(i-2), size = numParents))
      # set edge weights
      B[i, parents] <- 1/5 # runif(numParents + 1, .2, 1) * ((runif(numParents + 1) < .5) * 2 - 1)
    }
    
    B[i, i-1] <- runif(1, .2, .8)
  }
  B[2, 1] <- runif(1, .2, .8)
  
  if(dist == "gamma"){  
    if(varyScale) {
      errs <- matrix(rgamma(n * p, 1, 1) - 1, nrow = n, ncol = p)
      scale.param <- runif(p, 1, 1.5)
      errs <- t(t(errs) * scale.param)
    } else {
      errs <- matrix(rgamma(n * p, 1, 1) - 1, nrow = n, ncol = p)
    }
    
  } else if (dist == "unif"){
    if(varyScale) {
    errs <- matrix(runif(n * p, -sqrt(3), sqrt(3)), nrow = n, ncol = p)
    scale.param <- runif(p, 1, 1.5)
    errs <- t(t(errs) * scale.param)
    } else {
      errs <- matrix(runif(n * p, -sqrt(3), sqrt(3)), nrow = n, ncol = p)
    }
  } else if(dist == "gauss"){
    errs <- matrix(rnorm(n * p), nrow = n, ncol = p)
  } else if (dist == "t"){
    if(varyScale) {
      errs <- matrix(rt(n * p, df = 7) / sqrt(7 / 5), nrow = n, ncol = p)
      scale.param <- runif(p, 1, 1.5)
      errs <- t(t(errs) * scale.param)
    } else {
      errs <- matrix(rt(n * p, df = 7) / sqrt(7 / 5), nrow = n, ncol = p)
    }
  }else if (dist == "dexp"){
      if(varyScale) {
        errs <- matrix(rmutil::rlaplace(n * p, s = 1 / sqrt(2)), nrow = n, ncol = p)
        scale.param <- runif(p, 1, 1.5)
        errs <- t(t(errs) * scale.param)
      } else {
        errs <- matrix(rmutil::rlaplace(n * p, s = 1 / sqrt(2)), nrow = n, ncol = p)
      }
    
  }
  Y <- solve(diag(rep(1,p)) - B, t(errs))
  return(list(B = B, Y = t(Y), errs = errs))
}
