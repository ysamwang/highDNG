



findGraphMinMax <- function(Y, maxInDegree = 3, degree = 3) {
  
  getTau.MinMax <- function(i, j, degree, lastRoot, conditionSet, maxInDegree, Y){
    # if there are no nodes in conditionSet, then just calculate tau on the raw data
    if(length(conditionSet) == 0){
      return(max(sapply(setdiff(j, i), function(j){.calcTau(degree, Y[, i], Y[, j])})))
      
      
    } else if(length(conditionSet) > 0) { # could be just an else here
      
      # Only consider conditioning subsets which include lastRoot
      # NB: if the size of conditionSet is <= maxInDegree
      #   then there will be no pruning since all subsets will include
      #
      conditionSubSet <- t(combn(setdiff(conditionSet, lastRoot), min(length(conditionSet), maxInDegree) - 1))
      conditionSubSet <- cbind(rep(lastRoot, dim(conditionSubSet)[1]), conditionSubSet)
      
      testStat <- 1e5
      # calculate regression for each possible conditioning set    
      for(z in 1:dim(conditionSubSet)[1]){
        res <- RcppArmadillo::fastLm(X = Y[, conditionSubSet[z, ]], y = Y[, i])$residual
        
        # Update test.stat for the nodes in testAgainst which decrease      
        testStat <- min(testStat, max(sapply(setdiff(j,i), function(j){.calcTau(k = degree, pa = res,
                                                                                ch = Y[, j])})))
      }
      # only return the test stat values which were ever touched
      return(testStat)
    }
  }
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  # list of possible nodes to condition on for the unordered nodes
  # C[i, j] = 1 indicates that j should be considered as a conditioning node of i
  cutoff <- 0
  
  
  # possible parents of each node
  # P[i, j] = 1 indicates that j could be a parent of i
  # start with dense matrix, then remove 1's using rising cutoff
  # possibleRemainingParents <- matrix(1, nrow = p, ncol = p) - diag(rep(1,p))
  
  
  # matrix of tau statistics
  # tauStats[i, j] = min_C (tau_i -> j adjusted for C)
  # tauStats <- matrix(1e5, nrow = p, ncol = p)
  # cutoff <- 0
  tauStats <- rep(1e5, p)

  
  while(length(unordered) > 1){
    
    # Testing for Root
    for(i in unordered) {
      ### Test to see if i has any parents left in unordered
      tauStats[i] <- min(tauStats[i], getTau.MinMax(i, j = unordered, degree = degree, lastRoot = ordered[length(ordered)],
                                                                conditionSet = ordered,
                                                                maxInDegree = maxInDegree, Y = Y))
    }
    
    
    
    # Force pick lowest
    
    root <- unordered[which.min(tauStats[unordered])]
    
    # Update quantities
    # 
    # cat("Roots: ")
    # cat(root)
    # cat(" ; ")
    # print(round(tauStats[unordered],3))
    # cat("\n")
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    
  }
  ordered <- c(ordered, unordered) 
  
  return(list(topOrder = ordered))
}