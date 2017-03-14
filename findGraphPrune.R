findGraphPrune <- function(Y, maxInDegree = 3, degree = 3) {
  
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
  tauStats <- matrix(cutoff + 1, nrow = p, ncol = p)
  diag(tauStats) <- 0
  
  
  while(length(unordered) > 1){
    
    # Testing for Root
    for(i in unordered) {
      ### Test to see if i has any parents left in unordered
      tauStats[i, which(tauStats[i, ] > cutoff)] <- pmin(tauStats[i, which(tauStats[i, ] > cutoff)],
                                                         getTau(i, j = which(tauStats[i, ] > cutoff),
                                                                degree = degree, lastRoot = ordered[length(ordered)],
                                                                conditionSet = intersect(which(tauStats[i, ] > cutoff), ordered),
                                                                maxInDegree = maxInDegree, Y = Y))
    }
    
    
    
    # Force pick lowest
    maxTau <- apply(tauStats[unordered,unordered], MAR = 1, max)
    cutoff <- max(cutoff, min(maxTau))
    root <- unordered[which.min(maxTau)]
    
    # Update quantities
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    cat("Roots: ")
    cat(root)
    cat(" ; ")
    cat("cutoff- ")
    cat(cutoff)
    cat("\n")
    print(round(tauStats,3))
    cat("\n")
    
  }
  ordered <- c(ordered, unordered) 
  
  return(list(topOrder = ordered))
}