library(RcppArmadillo)
tau.stat <- function(A, B, k) {
  abs(mean(A^(k - 1) * B) * mean(A^2) - mean(A^k) * mean(A * B))
}

tau.stat.normalized <- function(A, B, k) {
  abs(cor(A^(k - 1), B) - mean(A^k) / sqrt(mean(A^(2 * k-2)) * mean(A^2)) * cor(A, B))
}


getMinTau <- function(i, unordered, k, lastRoot, possiblePa, maxInDegree, Y){
  # if there are no possible parents, then just calculate tau
  if(length(possiblePa) == 0){
    return(sapply(setdiff(unordered, i), function(j){tau.stat(Y[, i], Y[, j], k)}))
  
      # if there is one possible parents, then just calculate the residual, then get tau  
  } else if(length(possiblePa) == 1){
    res <- fastLm(X = Y[, possiblePa], Y = Y[, i])$residual
    return(sapply(setdiff(unordered, i), function(j){tau.stat(res, Y[, j], k)}))

  } else if(length(possiblePa) > 1) {
    possible_sets <- t(combn(setdiff(possiblePa, newNode), min(maxInDegree, length(possiblePa)) - 1))
    possible_sets <- cbind(rep(newNode, dim(possible_sets)[1]), possible_sets)
    min.tau <- 1e5
    for(z in 1:dim(possible_sets)[1]){
      res <- fastLm(X = Y[, possible_sets[z, ]], Y = Y[, i])$residual
      min.tau <- min(min.tau, max(sapply(setdiff(unordered, i), function(j){tau.stat(res, Y[, j], k)})))
    }
    return(min.tau)
  }
}



findDAG <- function(Y, maxInDegree = 5, k = 3) {
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  # list of possible nodes to condition on for the unordered nodes
  # C[i, j] = 1 indicates that j should be considered as a conditioning node of i
  conditioningSets <- matrix(0, nrow = p, ncol = p)
  
  # possible parents of each node
  # P[i, j] = 1 indicates that j could be a parent of i
  # start with dense matrix, then remove 1's using rising cutoff
  # possibleRemainingParents <- matrix(1, nrow = p, ncol = p) - diag(rep(1,p))
  
  
  # matrix of tau statistics
  # tauStats[i, j] = min_C (tau of j -> i adjusted for C)
  # tauStats <- matrix(1e5, nrow = p, ncol = p)
  cutoff <- 0
  
  while(length(unordered) > 1){
  
    tauStats <- rep(0, length(unordered))
  
    for(i in unordered){

    
    ### Test to see if i has any parents left in unordered
      tauStats[i] <- 
    
  }

  # Force pick lowest
  # see if cutoff should be raised
  maxTauStar <- sapply(unordered, function(i){
    max(tauStats[setdiff(unordered,i), i])
  })  
    
  cutoff <- max(cutoff, min(maxTauStar))
  
  
  
  root <- unordered[which.min(maxTauStar)]
  ordered <- c(ordered, root)
  unordered <- setdiff(unordered, root)
  conditioningSets[unordered, root] <- 1
  cat("Roots: ")
  cat(root)
  cat("\t; cutoff: ")
  cat(cutoff)
  cat("\n")
  }
  ordered <- c(ordered, unordered) 

  return(list(topOrder = ordered, parents = conditioningSets))
}



data <- rDAG_degree(p = 10, n = 1000, maxInDeg = 5, dist = "gamma")
cov(t(data$Y))
st <- proc.time()
output2 <- findDAG(t(data$Y))
end <- proc.time()
cor(output2$topOrder, 1:length(output2$topOrder), method = "kendall")


tester.helper1 <- function(Y){
  mean(Y)
}

tester.helper <- function(){
  mean(Y)
}

tester <- function(Y){
  tester.helper()  
}

Y <- matrix(runif(100000), nrow = 1000)
microbenchmark::microbenchmark(tester(Y))

