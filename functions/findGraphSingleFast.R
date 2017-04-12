#############################################################################################################
#     Recover graph from observational non-Gaussian Data
#
#     We calculate tau_i <- min_C max_j tau_{i-> j.C}
#
#
# Y:              n x p matrix of observations with sample in row, variable in column
# maxInDegree:    Assumed largest in-degree
# degree:         The assumed degree of non-Gaussianity
# pruningCut:     Fixed cut-off value
# fun:            The aggregation function. Should be max
# subsets:        Whether to condition on all subsets of variables, or just test sets of largest size
# B:              True DAG structure if available
# cutOffScaling:  Scaling factor for how to set the cutOff
# verbose:        Print progress updates
#
#############################################################################################################

findGraphSingleFast <- function(Y, maxInDegree = 3, degree = 3, fun = max,
                            subsets = F, B = NULL, cutOffScaling = .5, verbose = T) {
  
  # check to make sure the function used to aggregate is either sum or max
  if(!identical(fun, sum) && !identical(fun, max)){
    print("Unrecognized function")
    stop()
  }
  
  # helper to calculate the Test Statistic
  .calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }
  
  yty <- t(Y) %*% Y  
  
  
  
  
  # helper to go through all sets
  .getTauSingleFast <- function(i, j, degree, conditionSet, maxInDegree){
    ####### Set up #######
    j <- setdiff(j, i)
    p <- dim(Y)[2]
    pruneStat <- rep(1e10, p)

    if(length(conditionSet) == 0){ 
      ### Should only be run when ordered = NULL
      pruneStat[j] <- sapply(j, function(j){.calcTau(degree, Y[, i], Y[, j])})
      tauStat <- fun( pruneStat[j] )
      return(list(pruneStat = pruneStat, tauStat = tauStat))
      
    } else {
      
      
      ### If considering all subsets smaller than maxInDegree, then start at subset of 
      ### largest.set.size is the largest set we should consider
      largest.set.size <- min(maxInDegree, length(conditionSet))
  
        ### Ensure that each conditioning sub-set includes the last node
        ### So that we are not re-testing conditioning sets
        if(length(conditionSet) == 1){
          conditionSubSet <- matrix(conditionSet, nrow = 1, ncol = 1)
        } else {
          conditionSubSet <- t(combn(conditionSet, largest.set.size))
        }
          
          anSets <- t(apply(conditionSubSet, MAR = 1, function(x){setdiff(conditionSet, x)}))
          if(dim(anSets)[1] == 1 & dim(anSets)[2] > 0){
            anSets <- t(anSets)
          }
          
          tau.C <- calcTauCHelper::calcTauC(i - 1, j - 1, degree,
                                            conditionSubSet - 1, anSets - 1, Y,  yty)
      
    }
        
      
      
      # only return the test stat values which were ever touched
      return(list(pruneStat = tau.C$an, tauStat = tau.C$tauStat))
    }
  
  ################### End getTau Function ###################
  
    cutOffFast <- 0

  # topological ordering of nodes
  orderedFast <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unorderedFast <- c(1:p)
  
  ### Record of lowest tau value achieved so far
  pruneStatsFast <- matrix(1e5, nrow =  p, ncol = p)
  diag(pruneStatsFast) <- 0
  
  while(length(unorderedFast) > 1) {
    
    # Testing for Root
    outputFast <- foreach(i = unorderedFast, .packages = "calcTauCHelper") %dopar% {
      
      if(!is.null(B)){
        condSet <- intersect(which(B[i, ] != 0), orderedFast)
      } else {
        condSet <- union(intersect(orderedFast, which(pruneStatsFast[i, ] > cutOffFast)), orderedFast[length(orderedFast)])  
      }
      ### Test to see if i has any parents left in unordered
      .getTauSingleFast(i, j = unorderedFast, degree = degree, conditionSet = condSet,
                    maxInDegree = maxInDegree)
    }
    
    output_pruneStatsFast <- round(t(sapply(outputFast, function(x){x$pruneStat})),7)
    
    output_tauStatsFast <- sapply(outputFast, function(x){x$tauStat})
    
    # Update pruneStats
    pruneStatsFast[unorderedFast, orderedFast] <- pmin(pruneStatsFast[unorderedFast, orderedFast], output_pruneStatsFast[, orderedFast])

    
    root <- unorderedFast[which.min(output_tauStatsFast)]
    pa <- union(intersect(orderedFast, which(pruneStatsFast[root, ] > cutOffFast)), orderedFast[length(orderedFast)])
    
    # Update cutoff if pruningCut is not set to a fixed value

    cutOffFast <- max(cutOffFast, min(output_tauStatsFast) * cutOffScaling)

    # Print
    if(verbose){
      cat("====")
      cat(length(orderedFast))
      cat("====\n")
      cat("Roots: "); cat(root); cat("\n"); cat("; Parents: "); cat(pa)
      cat("\n")
      cat("\n")
      
      names(output_tauStatsFast) <- unorderedFast
      print(round(output_tauStatsFast, 3))
      cat("Cutoff: "); cat(round(cutOffFast,3)); cat("\n\n")
    }
    
    # Update topological ordering
    orderedFast <- c(orderedFast, root)
    unorderedFast <- setdiff(unorderedFast, root)
    
  }
  
  return(list(topOrder = c(orderedFast, unorderedFast), prune = pruneStatsFast))
}
