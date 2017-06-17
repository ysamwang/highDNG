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

findGraphSingle <- function(Y, maxInDegree = 3, degree = 3, pruningCut = NULL, fun = max,
                            B = NULL, cutOffScaling = .5, verbose = T) {
  
  # check to make sure the function used to aggregate is either sum or max
  if(!identical(fun, sum) && !identical(fun, max)){
    print("Unrecognized function")
    stop()
  }
  
  # helper to calculate the Test Statistic
  .calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }
  
  # helper to go through all sets
  .getTauSingle <- function(i, j, degree, conditionSet, maxInDegree){
    
    ####### Set up #######
    j <- setdiff(j, i)
    p <- dim(Y)[2]
    pruneStat <- rep(1e5, p)
    tauStat <- 1e5
    
    if(length(conditionSet) == 0){ 
      ### Should only be run when ordered = NULL
      pruneStat[j] <- sapply(j, function(j){.calcTau(degree, Y[, i], Y[, j])})
      tauStat <- fun( pruneStat[j] )
      return(list(pruneStat = pruneStat, tauStat = tauStat))
      
    } else {

      
      ### If considering all subsets smaller than maxInDegree, then start at subset of 
      ### largest.set.size is the largest set we should consider
      size.of.set <- min(maxInDegree, length(conditionSet))
        

        if(length(conditionSet) == 1){
          conditionSubSet <- matrix(conditionSet, nrow = 1, ncol = 1)
        } else {
          conditionSubSet <- t(combn(conditionSet, size.of.set))
        }
        
        ### calculate regression for each possible conditioning set
        for(z in 1:dim(conditionSubSet)[1]){
          res <- RcppArmadillo::fastLm(X = Y[, conditionSubSet[z, ]], y = Y[, i])$residual
          
          # Update the tau for all currently unordered nodes
          tau.C <- sapply(j, function(j){.calcTau(k = degree, pa = res, ch = Y[, j])})
          
          # update tauStat
          tauStat <- min(tauStat, fun( tau.C ))
          
          # Update the tau for ancestors but possibly not parents      
          conditionNodesToTest <- setdiff(conditionSet, conditionSubSet[z, ])
          
          pruneStat[conditionNodesToTest] <- pmin(pruneStat[conditionNodesToTest],
                                                  sapply(conditionNodesToTest, function(j){.calcTau(k = degree, pa = res,
                                                                                                    ch = Y[, j])}))
        }
        
      
      
      # only return the test stat values which were ever touched
      return(list(pruneStat = pruneStat, tauStat = tauStat))
    }
  }
  
  ################### End getTau Function ###################
  
  if(is.null(pruningCut)){
    cutOff <- 0
  } else {
    cutOff <- pruningCut
  }  
  
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
        condSet <- intersect(which(B[i, ] != 0), ordered)
      } else {
        condSet <- union(intersect(ordered, which(pruneStats[i, ] > cutOff)), ordered[length(ordered)])
        
      }
      ### Test to see if i has any parents left in unordered
      .getTauSingle(i, j = unordered, degree = degree, conditionSet = condSet,
                   maxInDegree = maxInDegree)
    }
    
    output_pruneStats <- round(t(sapply(output, function(x){x$pruneStat})),7)
    
    output_tauStats <- round(sapply(output, function(x){x$tauStat}), 7)
    
    root <- unordered[which.min(output_tauStats)]
    pa <- union(intersect(ordered, which(pruneStats[root, ] > cutOff)), ordered[length(ordered)])
    
    
    pruneStats[unordered, ordered] <- pmin(pruneStats[unordered, ordered], output_pruneStats[, ordered])

    # Update cutoff if pruningCut is not set to a fixed value
    if(is.null(pruningCut)){
      cutOff <- max(cutOff, min(output_tauStats) * cutOffScaling)
    }
    
    # Print
    if(verbose){
      cat("====")
      cat(length(ordered))
      cat("====\n")
      cat("Roots: "); cat(root); cat("; Parents: "); cat(pa)
      cat("\n")
      cat("\n")
      
      names(output_tauStats) <- unordered
      print(round(output_tauStats, 3))
      cat("Cutoff: "); cat(round(cutOff,3)); cat("\n\n")
    }
    
    # Update topological ordering
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
  }
   
  
  return(list(topOrder = c(ordered, unordered), prune = pruneStats))
}
