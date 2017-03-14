getTau.Prune <- function(i, j, degree, lastRoot, conditionSet, maxInDegree, Y){
  # if there are no nodes in conditionSet, then just calculate tau on the raw data
  
  if(length(conditionSet) == 0){
    return(sapply(setdiff(j, i), function(j){.calcTau(degree, Y[, i], Y[, j])}))
    
    
  } else if(length(conditionSet) > 0) { # could be just an else here
    
    # Only consider conditioning subsets which include lastRoot
    # NB: if the size of conditionSet is <= maxInDegree
    #   then there will be no pruning since all subsets will include
    
    conditionSubSet <- t(combn(conditionSet, min(length(conditionSet), maxInDegree)))

    # Using global indices here so have test.stat of length p
    test.stat <- rep(1e5, dim(Y)[2])
    
    # calculate regression for each possible conditioning set    
    for(z in 1:dim(conditionSubSet)[1]){
      res <- RcppArmadillo::fastLm(X = Y[, conditionSubSet[z, ]], y = Y[, i])$residual
      
      # What is in j that I can still test (ie not in the conditioning set)
      testAgainst <- setdiff(j, conditionSubSet[z, ])
      
      # Update test.stat for the nodes in testAgainst which decrease      
      test.stat[testAgainst] <- pmin(test.stat[testAgainst],
                                     sapply(testAgainst, function(j){.calcTau(k = degree, pa = res,
                                                                              ch = Y[, j])}))
    }
    # only return the test stat values which were ever touched
    return(test.stat[setdiff(j, i)])
  }
}


findGraphRisingCut <- function(Y, maxInDegree = 4, degree = 3, B = NULL) {
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  # list of possible nodes to condition on for the unordered nodes
  # C[i, j] = 1 indicates that j should be considered as a conditioning node of i
  
  
  
  # possible parents of each node
  # P[i, j] = 1 indicates that j could be a parent of i
  # start with dense matrix, then remove 1's using rising cutoff
  # possibleRemainingParents <- matrix(1, nrow = p, ncol = p) - diag(rep(1,p))
  
  
  # matrix of tau statistics
  # tauStats[i, j] = min_C (tau_i -> j adjusted for C)
  # tauStats <- matrix(1e5, nrow = p, ncol = p)
  tauStats <- matrix(1e5, nrow = p, ncol = p)
  diag(tauStats) <- 0
  cutoff <- 0
  
  while(length(unordered) > 1){
    
    # Testing for Root
    for(i in unordered) {
      # cat("Testing: ")
      # cat(i)
      # cat("; Cond set: ")
      # cat(intersect(which(tauStats[i,] > cutoff), ordered))
      # cat("\n")
      if(!is.null(B)){
        condSet <- intersect(which(B[i, ] != 0), ordered)
      } else {
        condSet <- intersect(which(tauStats[i,] > cutoff), ordered) 
      }
      ### Test to see if i has any parents left in unordered
      tauStats[i, union(which(tauStats[i,] > cutoff), setdiff(unordered,i))] <- pmin(tauStats[i, union(which(tauStats[i,] > cutoff), setdiff(unordered,i))],
                                                        getTau.Prune(i, j = union(which(tauStats[i,] > cutoff), setdiff(unordered,i)),
                                                                 degree = degree, 
                                                                  lastRoot = NULL,
                                                                  conditionSet = condSet,
                                                                 maxInDegree = maxInDegree, Y = Y))
    }
    
    
    
    # Force pick lowest
    maxTau <- apply(tauStats[unordered, unordered], MAR = 1, FUN = max)
    cutoff <- max(cutoff, min(maxTau))
    root <- unordered[which.min(maxTau)]
    
    # Update quantities

    cat("Roots: ")
    cat(root)
    # cat(" ; ")
    # cat(cutoff)
    cat("\n")
    # print(round(tauStats[unordered,],3))
    # cat("\n")
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    
  }
  ordered <- c(ordered, unordered) 
  
  return(list(topOrder = ordered))
}