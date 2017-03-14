

findGraphSumMultiSet.test <- function(Y, maxInDegree = 3, degree = 3, B  = NULL, cutoff = 0) {
  
  .calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }
  
  getTau.MultiSet <- function(i, j, degree, lastRoot, conditionSet, maxInDegree, Y){
    # if there are no nodes in conditionSet, then just calculate tau on the raw data
    p <- dim(Y)[2]
    pruneStat <- rep(1e5, p)
    
    if(length(conditionSet) == 0){
      pruneStat[setdiff(j, i)] <- sapply(setdiff(j, i), function(j){.calcTau(degree, Y[, i], Y[, j])}) 
      return(pruneStat)
      
      
    } else if(length(conditionSet) > 0) { # could be just an else here
      
      # Only consider conditioning subsets which include lastRoot
      # NB: if the size of conditionSet is <= maxInDegree
      #   then there will be no pruning since all subsets will include
      #
      conditionSubSet <- t(combn(setdiff(conditionSet, lastRoot),
                                 min(length(conditionSet), maxInDegree) - 1))
      conditionSubSet <- cbind(rep(lastRoot, dim(conditionSubSet)[1]), conditionSubSet)
      # conditionSubSet <- t(combn(conditionSet,
      #                            min(length(conditionSet), maxInDegree)))
      # 
      
      # calculate regression for each possible conditioning set    
      for(z in 1:dim(conditionSubSet)[1]){
        res <- RcppArmadillo::fastLm(X = Y[, conditionSubSet[z, ]], y = Y[, i])$residual
        tempStats <- sapply(setdiff(j,i), function(j){.calcTau(k = degree, pa = res,
                                                               ch = Y[, j])})
        
        pruneStat[setdiff(j,i)] <- pmin(pruneStat[setdiff(j,i)], tempStats)
        # Update test.stat for the nodes in testAgainst which decrease      
        
        conditionNodesToTest <- setdiff(conditionSet, conditionSubSet[z, ])
        pruneStat[conditionNodesToTest] <- pmin(pruneStat[conditionNodesToTest],
                                                sapply(conditionNodesToTest, function(j){.calcTau(k = degree, pa = res,
                                                                                                  ch = Y[, j])}))
      }
      # only return the test stat values which were ever touched
      return(pruneStat)
    }
  }
  
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  # list of possible nodes to condition on for the unordered nodes
  # C[i, j] = 1 indicates that j should be considered as a conditioning node of i
  # cutoff <- .2
  
  
  # possible parents of each node
  # P[i, j] = 1 indicates that j could be a parent of i
  # start with dense matrix, then remove 1's using rising cutoff
  # possibleRemainingParents <- matrix(1, nrow = p, ncol = p) - diag(rep(1,p))
  
  
  # matrix of tau statistics
  # tauStats[i, j] = min_C (tau_i -> j adjusted for C)
  # tauStats <- matrix(1e5, nrow = p, ncol = p)
  # cutoff <- 0
  pruneStats <- matrix(1e5, nrow =  p, ncol = p)
  diag(pruneStats) <- 0
  
  while(length(unordered) > 1){
    
    # Testing for Root
    output <- foreach(i = unordered, .combine = rbind) %dopar%{
      
      if(!is.null(B)){
        condSet <- intersect(which(B[i, ] != 0), ordered)
      } else {
        condSet <- intersect(ordered, which(pruneStats[i, ] > cutoff))  
      }
      ### Test to see if i has any parents left in unordered
      getTau.MultiSet(i, j = unordered, degree = degree, lastRoot = ordered[length(ordered)],
                      conditionSet = condSet,
                      maxInDegree = maxInDegree, Y = Y)
    }
    
    pruneStats[unordered, ] <- pmin(pruneStats[unordered, ], output)
    root.stats <- apply(pruneStats[unordered, unordered], MAR = 1, sum)
    root <- unordered[which.min(root.stats)]
    # Update quantities
    # 
    cat("Roots: ")
    cat(root)
    cat("\n")
    # print(round(pruneStats[unordered,unordered],3))
    # # print(singleTauStat[unordered])
    # # cat(" ; ")
    # # print(cut.print)
    # cat("\n")
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    
  }
  ordered <- c(ordered, unordered) 
  
  return(list(topOrder = ordered, prune = pruneStats))
}