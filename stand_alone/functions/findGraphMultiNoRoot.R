findGraphMultiNoRoot <- function(Y, maxInDegree = 3, degree = 3, pruningCut = NULL, fun = max,
                           subsets = F, B = NULL, verbose = T) {
  
  
  if(!identical(fun, sum) && !identical(fun, max)){
    print("Unrecognized function")
    stop()
  }
  
  # Test Statistic
  .calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }
  
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
      
      
      
      
    } else if(!(lastRoot %in% conditionSet)){
      ### There was a new root added, but we don't have any new sets to test
      ### Note this includes the scenario where conditionSet is the empty set 
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
      largest.set.size <- min(maxInDegree, length(conditionSet))
      
      ### Note that we first get all subsets of size - 1, since we'll add back in
      ### lastRoot after forming all subsets
      
      if(subsets){  
        
        # Get residuals when only conditioning on lastRoot
        res <- RcppArmadillo::fastLm(X = Y[, lastRoot], y = Y[, i])$residual
        pruneStat[j] <- pmin(pruneStat[j], 
                             sapply(j,function(j){.calcTau(k = degree, pa = res, ch = Y[, j])}
                             )
        )
        
        
        # Update the tau for ancestors but possibly not parents      
        conditionNodesToTest <- setdiff(conditionSet, lastRoot)
        
        pruneStat[conditionNodesToTest] <- pmin(pruneStat[conditionNodesToTest],
                                                sapply(conditionNodesToTest,
                                                       function(j){.calcTau(k = degree, pa = res, ch = Y[, j])}
                                                )
        )
        
        
        size.of.set <- 2
        
        
      } else {
        # If we only test subsets of the max size, then go to that size immediately
        size.of.set <- largest.set.size
      }
      
      ### Start testing subsets of an appropriate size
      while(size.of.set <= largest.set.size){
        
        ### Ensure that each conditioning sub-set includes the last node
        ### So that we are not re-testing conditioning sets
        conditionSubSet <- t(combn(setdiff(conditionSet, lastRoot),
                                   size.of.set - 1))
        conditionSubSet <- cbind(rep(lastRoot, dim(conditionSubSet)[1]), conditionSubSet)
        
        
        ### calculate regression for each possible conditioning set
        for(z in 1:dim(conditionSubSet)[1]){
          res <- RcppArmadillo::fastLm(X = Y[, conditionSubSet[z, ]], y = Y[, i])$residual
          
          # Update the tau for all currently unordered nodes
          pruneStat[j] <- pmin(pruneStat[j], 
                               sapply(j,function(j){.calcTau(k = degree, pa = res, ch = Y[, j])}))
          
          # Update the tau for ancestors but possibly not parents      
          conditionNodesToTest <- setdiff(conditionSet, conditionSubSet[z, ])
          
          pruneStat[conditionNodesToTest] <- pmin(pruneStat[conditionNodesToTest],
                                                  sapply(conditionNodesToTest, function(j){.calcTau(k = degree, pa = res,
                                                                                                    ch = Y[, j])}))
        }
        
        # increase the size of the set
        size.of.set <- size.of.set + 1
        
        
      } # end while 
      
      
      
      # only return the test stat values which were ever touched
      return(pruneStat)
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
  root <- NULL
  
  # set of nodes which have not yet been ordered
  unordered <- c(1:p)
  
  ### Record of lowest tau value achieved so far
  pruneStats <- matrix(1e5, nrow =  p, ncol = p)
  diag(pruneStats) <- 0
  
  while(length(unordered) > 1){
    
    # Testing for Root
    output <- foreach(i = unordered, .combine = rbind) %dopar%{
      
      if(!is.null(B)){
        condSet <- intersect(which(B[i, ] != 0), ordered)
      } else {
        condSet <- union(intersect(ordered, which(pruneStats[i, ] > cutOff)), root)  
      }
      
      j <- intersect(unordered, which(pruneStats[i, ] > cutOff))
      ### Test to see if i has any parents left in unordered
      .getTauMulti(i, j = j, degree = degree, lastRoot = ordered[length(ordered)],
                   conditionSet = condSet,
                   maxInDegree = maxInDegree, Y = Y)
    }
    
    
    # Update pruneStats
    pruneStats[unordered, ] <- pmin(pruneStats[unordered, ], output)
    
    
    output_tauStats <- apply(pruneStats[unordered, unordered], MAR = 1,
                             # uses either sum or max
                             fun)
    # Update cutoff if pruningCut is not set to a fixed value
    if(is.null(pruningCut)){
      cutOff <- max(cutOff, min(output_tauStats))
    }
    
    root <- unordered[which(output_tauStats <= cutOff)]
    
    
    
    
    # Update quantities
    # 
    if(verbose){
      cat("====")
      cat(length(ordered))
      cat("====\n")
      cat("Roots: "); cat(root); cat("; Parents: ")
      
      cat(ordered[which(pruneStats[root, ordered] > cutOff)])
      cat("\n")
      cat("\n")
      
      # names(output_tauStats) <- unordered
      # print(round(output_tauStats, 3))
      cat("Cutoff: "); cat(round(cutOff,3)); cat("\n\n")
    }
    
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    
    
  }
  ordered <- c(ordered, unordered) 
  
  return(list(topOrder = ordered, prune = pruneStats))
}
