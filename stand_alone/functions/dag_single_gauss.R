testFindGraph <- function(Y, degree = 3, maxInDegree = 3){
  ordered <- c()
  unordered <- c(1:dim(Y)[2])
  
  check.ind <- function(j, i, conditionSet){
    res <- RcppArmadillo::fastLm(X = Y[, conditionSet], y = Y[, j])$residual
    res <- res + Y[, i]
    return(abs(mean(Y[, i]^(degree - 1) * res) * mean(Y[, i]^2)
               - mean(Y[, i]^degree) * mean(Y[, i] * res)))
  }
  
  tau.stats <- rep(1e5, dim(Y)[2])
    
  while(length(unordered) > 1){
    
    for(i in unordered){
      tau.i <- 1e5
      
      if(length(ordered) <= 1){
        condSet <- matrix(c(ordered, i), nrow = 1)
      } else {
        condSet <- t(combn(ordered, m = min(length(ordered), maxInDegree)))
        condSet <- cbind(condSet, rep(i, dim(condSet)[1]))
      }
      
      
      
      for(z in 1:dim(condSet)[1]){
        tau.i <- min(tau.i,
                     max(sapply(setdiff(unordered, i), check.ind,
                                i = i, conditionSet = condSet[z,])))
      }
      
      tau.stats[i] <- tau.i
    }
    
    root <- unordered[which.min(tau.stats[unordered])]
    ordered <- c(ordered, root)
    unordered <- setdiff(unordered, root)
    print("========")
    print(tau.stats)
    print(root)
  }
  return(ordered)
}


###############################################################################
n <- 10000
p <- 5
errs <- matrix(rnorm(n * p), nrow = p, ncol = n)
errs[1, ] <- rgamma(n, .2, .2) - 1
B <- matrix(0, nrow = p, ncol = p)
for(i in 1:(p-1)){
  B[i+1, i] <- .8
}
# B[-1, 1] <- .8

Y <- t(solve(diag(rep(1, p))- B, errs))
Y <- scale(Y)
testFindGraph(Y)


# helper to calculate the Test Statistic
.calcTau <- function(k, pa, ch) {
  abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
}



test.i.pa.of.j <- function(Y, i, j, condSets, degree = 3){
  if(length(condSets) == 0){
    return(.calcTau(degree, Y[, i], Y[, j]))
  } else {
    mod <- lm(Y[, j] ~ Y[, c(condSets, i)] - 1)
    res <- mod$res + mod$coeff[length(condSets) + 1] * Y[, i]
    return(.calcTau(degree, Y[, i], res))
  }
}

i <- 3
j <- 2
condSets <- 1
mod <- lm(Y[, j] ~ Y[, c(condSets, i)] - 1)
res <- mod$res + mod$coeff[length(condSets) + 1] * Y[, i]
.calcTau(degree, Y[, i], res)


test.i.pa.of.j(Y, 2, 3, condSets = c(1))
test.i.pa.of.j(Y, 3, 2, condSets = c(1))






