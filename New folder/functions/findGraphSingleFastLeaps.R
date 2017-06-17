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

findGraphSingleFastLeaps <- function(Y, maxInDegree = 3, degree = 3, fun = max,
                                subsets = F, B = NULL, verbose = T) {
  
  # check to make sure the function used to aggregate is either sum or max
  if(!identical(fun, sum) && !identical(fun, max)){
    print("Unrecognized function")
    stop()
  }
  
  # helper to calculate the Test Statistic
  .calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }

  leapsModelSearch <- rep(list(list(condSet = c(), rsq = 0)), dim(Y)[2])
  
  
  # helper to go through all sets
  .getTauSingleLeaps <- function(i, j, degree, conditionSet, Y){
    ####### Set up #######
    j <- setdiff(j, i)
    p <- dim(Y)[2]

    if(length(conditionSet) == 0){ 
      ### Should only be run when ordered = NULL
      tauStat <- fun( sapply(j, function(j){.calcTau(degree, Y[, i], Y[, j])}) )
    } else {
        resid <- RcppArmadillo::fastLm(X = Y[, conditionSet], y = Y[, i])$residuals
        tauStat <- fun(sapply(j, function(j){.calcTau(degree, resid, Y[, j])}))
    }
      return(tauStat)
    }
  
  ################### End getTau Function ###################
  
  # topological ordering of nodes
  ordered <- c()
  p <- dim(Y)[2]
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  

  while(length(unordered) > 1) {
    
    # Testing for Root
    output <- foreach(i = unordered, .packages = "leaps", .combine = c) %do% {
      
      if(!is.null(B)){
        condSet <- intersect(which(B[i, ] != 0), ordered)
      } else {
        if( length(ordered) <= maxInDegree) {
          condSet <- ordered
        } else {
          out <- leaps::regsubsets(x = Y[, ordered], y = Y[, i], nvmax = maxInDegree, intercept = F,
                                 really.big = (length(ordered) > 50) )
          if(leapsModelSearch[[i]])
          condSet <- ordered[which(summary(out)$which[maxInDegree, ])]
        }
      }
      ### Test to see if i has any parents left in unordered
      .getTauSingleLeaps(i, j = unordered, degree = degree, conditionSet = condSet, Y)
    }

    root <- unordered[which.min(output)]
    
    # Print
    if(verbose){
      cat("====")
      cat(length(ordered))
      cat("====\n")
      cat("Roots: "); cat(root); cat("\n")
      
      names(output) <- unordered
      print(round(output, 3))
    }
    
    # Update topological ordering
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
  }
  
  
  return(list(topOrder = c(ordered, unordered)))
}

