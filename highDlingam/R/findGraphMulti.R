#############################################################################################################
#     Recover graph from observational non-Gaussian Data
#
#     We calculate tau_i <- max_j min_C tau_{i-> j.C}
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


findGraphMulti <- function(Y, maxInDegree = 3, degree = 3, B = NULL,
                           cutOffScaling = .5, verbose = T) {
  
  parents <- list()
  # Test Statistic
  .calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }
  
  yty <- t(Y) %*% Y 
  
  # helper to go through all sets
  .getTauMulti <- function(i, j, degree, lastRoot, conditionSet, maxInDegree, Y){
    
    ####### Set up #######
    j <- setdiff(j,i)
    p <- dim(Y)[2]
    pruneStat <- rep(1e5, p)
    
    
    if(is.null(lastRoot)){ 
      ### Should only be run when ordered = NULL
      pruneStat[j] <- sapply(j, function(j){.calcTau(degree, Y[, i], Y[, j])})
      return(pruneStat)
      
      
    } else {
      ### There was a new root added and it is in the conditionSet
      ### So we have new sets to test
      
      
      ### Only consider conditioning subsets which include lastRoot ###
      ### This ensures we don't re-test a set, but storing everything in memory
      ### can be prohibitive if we look at the min_{over sets} aggregateStat(all other nodes)
      ### instead of aggregateStat(min_{over sets} all other nodes )
      
      
      ### If considering all subsets smaller than maxInDegree, then start at subset of 
      ### largest.set.size is the largest set we should consider
      size.of.set <- min(maxInDegree, length(conditionSet))
      
      if(length(setdiff(conditionSet, lastRoot)) == 1){
        conditionSubSet <- matrix(setdiff(conditionSet, lastRoot), nrow = 1, ncol = 1)
      }  else {
        conditionSubSet <- t(combn(setdiff(conditionSet, lastRoot),
                                   size.of.set - 1))
      }
      
      anSets <- t(apply(conditionSubSet, MAR = 1, function(x){setdiff(conditionSet, x)}))
      if(dim(anSets)[1] == 1 & dim(anSets)[2] > 0){
        anSets <- t(anSets)
      }
      
      # always include last root in the conditioning set  
      conditionSubSet <- cbind(rep(lastRoot, dim(conditionSubSet)[1]), conditionSubSet)
      pruneStat <- calcTauMultiC(i - 1, j - 1, degree, 
                                 conditionSubSet - 1, anSets - 1, Y,  yty)
      # only return the test stat values which were ever touched
      return(pruneStat)
    }
  }
  ################### End getTau Function ###################
  
  cutOff <- 0
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  ### Record of lowest tau value achieved so far
  pruneStats <- matrix(1e5, nrow =  p, ncol = p)
  diag(pruneStats) <- 0
  
  
  while(length(unordered) > 1){
    
    # Testing for Root
    output <- foreach(i = unordered) %dopar%{
      
      if(!is.null(B)){
        condSet <- union(intersect(ordered, which(B[i, ] != 0) ), ordered[length(ordered)])
      } else {
        condSet <- union(intersect(ordered, which(pruneStats[i, ] > cutOff)), ordered[length(ordered)])  
      }
      ### Test to see if i has any parents left in unordered
      list(condSet = condSet,
           tauMin = .getTauMulti(i, j = unordered, degree = degree, lastRoot = ordered[length(ordered)],
                                 conditionSet = condSet,
                                 maxInDegree = maxInDegree, Y = Y)
      )
    }
    
    
    # Update pruneStats
    outputTau <- t(sapply(output, function(x){x$tauMin}))
    
    pruneStats[unordered, ] <- pmin(pruneStats[unordered, ], outputTau)
    diag(pruneStats) <- 0
    
    output_tauStats <- apply(pruneStats[unordered, unordered], MAR = 1,
                             # uses either sum or max
                             max)
    
    root <- unordered[which.min(output_tauStats)]
    
    cutOff <- max(cutOff, min(output_tauStats) * cutOffScaling)
    
    
    
    # Update quantities
    # 
    if(verbose){
      cat("====")
      cat(length(ordered))
      cat("====\n")
      cat("Roots: "); cat(root); cat("; Parents: ")
      cat(output[[which.min(output_tauStats)]]$condSet)
      cat("\n")
      cat(round(output_tauStats,4))
      cat("\n")
      cat("\n")
      
      # names(output_tauStats) <- unordered
      # print(round(output_tauStats, 3))
      cat("Cutoff: "); cat(round(cutOff,3)); cat("\n\n")
    }
    parents[[length(parents) + 1]] <- output[[which.min(output_tauStats)]]$condSet
    
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    
  }
  ordered <- c(ordered, unordered) 
  parents[[length(parents) + 1]] <- union(intersect(ordered, which(pruneStats[unordered, ] > cutOff)), ordered[length(ordered)])
  
  return(list(topOrder = ordered, prune = pruneStats, cutoff = cutOff, parents = parents))
}
