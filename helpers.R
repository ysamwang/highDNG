#### getTau ####
# Tests whether node pa is a parent of node ch
#
# pa- vector of data
# ch- vector of data
# k- moment which is non-gaussian
.calcTau <- function(k, pa, ch) {
  abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
}

#### getTauMulti ####
# Tests whether node pa is a parent of node ch. Same as getTau, but
# may test multiple moments simultaneously
#
# pa- vector of data
# ch- vector of data
# k- vector of non-gaussian moments
.calcTauMulti <- function(k, pa, ch) {
  sum(sapply(k, FUN = .calcTau, pa = pa, ch = ch))
}



getTau <- function(i, j, degree, lastRoot, conditionSet, maxInDegree, Y){
  # if there are no nodes in conditionSet, then just calculate tau on the raw data
  if(length(conditionSet) == 0){
    return(sapply(setdiff(j, i), function(j){.calcTau(degree, Y[, i], Y[, j])}))
    
    
  } else if(length(conditionSet) > 0) { # could be just an else here
    
    # Only consider conditioning subsets which include lastRoot
    # NB: if the size of conditionSet is <= maxInDegree
    #   then there will be no pruning since all subsets will include
    #
    conditionSubSet <- t(combn(setdiff(conditionSet, lastRoot), min(length(conditionSet), maxInDegree) - 1))
    conditionSubSet <- cbind(rep(lastRoot, dim(conditionSubSet)[1]), conditionSubSet)
    
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
    return(test.stat[j])
  }
}



