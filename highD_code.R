library(RcppArmadillo)
library(doParallel)
foreach(i = 1:100, .combine = c) %dopar% {
  sqrt(i)
}

# Calculate tau statistic
tau.stat <- function(A, B, k) {
  abs(mean(A^(k - 1) * B) * mean(A^2) - mean(A^k) * mean(A * B))
}



getMinTauMax <- function(i, unordered, k, lastRoot, possiblePa, maxInDegree, Y){
  # if there are no possible parents, then just calculate tau
  if(length(possiblePa) == 0){
    return(max(sapply(setdiff(unordered, i), function(j){tau.stat(Y[, i], Y[, j], k)})))
  
      # if there is one possible parents, then just calculate the residual, then get tau  
  } else if(length(possiblePa) <= maxInDegree){
    res <- fastLm(X = Y[, possiblePa], y = Y[, i])$residual
    return(max(sapply(setdiff(unordered, i), function(j){tau.stat(res, Y[, j], k)})))

  } else if(length(possiblePa) > maxInDegree) {
    
    # Only consider sets which include lastRoot
    possible_sets <- t(combn(setdiff(possiblePa, lastRoot), maxInDegree - 1))
    possible_sets <- cbind(rep(lastRoot, dim(possible_sets)[1]), possible_sets)

    test.stat <- 1e5
    for(z in 1:dim(possible_sets)[1]){
      res <- fastLm(X = Y[, possible_sets[z, ]], y = Y[, i])$residual
      test.stat <- min(test.stat, max(sapply(setdiff(unordered, i), function(j){tau.stat(res, Y[, j], k)})))
    }
    return(test.stat)
  }
}





findDAGForceRoot <- function(Y, maxInDegree = 3, k = 3) {
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  # list of possible nodes to condition on for the unordered nodes
  # C[i, j] = 1 indicates that j should be considered as a conditioning node of i
  # conditioningSets <- matrix(0, nrow = p, ncol = p)
  
  # possible parents of each node
  # P[i, j] = 1 indicates that j could be a parent of i
  # start with dense matrix, then remove 1's using rising cutoff
  # possibleRemainingParents <- matrix(1, nrow = p, ncol = p) - diag(rep(1,p))
  
  
  # matrix of tau statistics
  # tauStats[i, j] = min_C (tau of j -> i adjusted for C)
  # tauStats <- matrix(1e5, nrow = p, ncol = p)
  # cutoff <- 0
  tauStats <- rep(1e5, p)
  
  while(length(unordered) > 1){
  
    
    for(i in unordered) {
      
      ### Test to see if i has any parents left in unordered
      tauStats[i] <-min(tauStats[i], getMinTauMax(i, unordered, k,
                                                  lastRoot = ordered[length(ordered)],
                                                  ordered, maxInDegree, Y))
    }

    # Force pick lowest
    # see if cutoff should be raised
    root <- unordered[which.min(tauStats[unordered])]
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    cat("Roots: ")
    cat(root)
    cat(" ; ")
    cat("\n")
    # cat(unordered)
    # cat("\n")
  }
  ordered <- c(ordered, unordered) 

  return(list(topOrder = ordered))
}






findDAGPrune <- function(Y, maxInDegree = 3, k = 3) {
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  # list of possible nodes to condition on for the unordered nodes
  # C[i, j] = 1 indicates that j should be considered as a conditioning node of i
  cutoff <- 0
  conditioningSets <- matrix(cutoff -1, nrow = p, ncol = p)
  
  # possible parents of each node
  # P[i, j] = 1 indicates that j could be a parent of i
  # start with dense matrix, then remove 1's using rising cutoff
  # possibleRemainingParents <- matrix(1, nrow = p, ncol = p) - diag(rep(1,p))
  
  
  # matrix of tau statistics
  # tauStats[i, j] = min_C (tau of j -> i adjusted for C)
  # tauStats <- matrix(1e5, nrow = p, ncol = p)
  # cutoff <- 0
  tauStats <- rep(1e5, p)
  while(length(unordered) > 1){
    
    # Testing for Root
    for(i in unordered) {
      ### Test to see if i has any parents left in unordered
      tauStats[i] <- min(tauStats[i], getMinTauMax(i, unordered, k,
                                    lastRoot = ordered[length(ordered)], which(conditioningSets[i, ] > cutoff), maxInDegree, Y))
    }
    
    
    
    # Force pick lowest
    cutoff <- max(cutoff, min(tauStats[unordered]))
    root <- unordered[which.min(tauStats[unordered])]
    
    # Update quantities
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    conditioningSets[, root] <- 1
    # Pruning
    for(i in unordered) {
      ### Test to see if i has any parents left in unordered
      ### Do not need to fit new regressions, use the previous regressions to check
      ### For newest root, you can look back and see previous regressions
      conditioningSets[i, which(conditioningSets[i, ] > cutoff)] <- pmin(conditioningSets[i, which(conditioningSets[i, ] > cutoff)], 
                                                                         getMinTauMax(i, which(conditioningSets[i, ] > cutoff), k,
                                                   lastRoot = ordered[length(ordered)], which(conditioningSets[i, ] > cutoff), maxInDegree, Y))
    }
    
    
    
    cat("Roots: ")
    cat(root)
    cat(" ; ")
    cat("\n")
    
  }
  ordered <- c(ordered, unordered) 
  
  return(list(topOrder = ordered))
}



  set.seed(566)
  out_dag <- rDAG_degree(p = 10, n = 100000, maxInDeg = 4, dist = "gamma")
  st <- proc.time()
  output2 <- findDAGForceRoot(Y)
  end <- proc.time()
  cor(output2$topOrder, 1:length(output2$topOrder), method = "kendall")

