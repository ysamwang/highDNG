rDAG <- function(p, n, edgeProb = .1){
  B <- matrix(0, nrow = p, ncol = p)
  B[lower.tri(B, diag = F)] <- (runif(p*(p-1)/2) < edgeProb) * rnorm(p*(p-1)/2)
  avec <- rgamma(p, 1, 1)
  bvec <- rgamma(p, 1, 1)
  errs <- mapply(function(a,b,n){rgamma(n, a, b) - a/b}, avec, bvec, n)
  Y <- solve(diag(rep(1,p)) - B, errs)
  return(list(B = B, Y = Y))
}

rDAG_degree <- function(p, n, maxInDeg = 4, dist = "gamma"){
  B <- matrix(0, nrow = p, ncol = p)
  for(i in 2:p){
    numParents <- min(sample.int(maxInDeg, size = 1), i - 1) - 1
    parents <- c(sample(1:(i-2), size = numParents), i-1)
    B[i, parents] <- runif(numParents + 1, .2, .95) * ((runif(numParents + 1) < .5) * 2 - 1)
  }
  
  if(dist == "gamma"){  
  avec <- c(p:1)
  bvec <- sqrt(c(p:1))
  errs <- mapply(function(a,b,n){rgamma(n, a, b) - a/b}, avec, bvec, n)
  } else if (dist == "unif"){
    errs <- matrix(runif(n * p, -1, 1), nrow = n, ncol = p)
  }
  Y <- solve(diag(rep(1,p)) - B, t(errs))
  return(list(B = B, Y = Y))
}





ng_dag_search <- function(data, K = 3, cutoff = .05){
  ordering <- c()
  V <- dim(data)[2]
  
  data.mod <- t(t(data) - rowMeans(data))
  
  remaining.edges <- c(1:V)
  
  # Get Ordering
  while(length(remaining.edges) > 1) {
    tau.min <- rep(0, length(remaining.edges))
    
    for(i in 1:length(remaining.edges)){
      tau.i <- c()
      A <- 
        for(j in remaining.edges[-i]){
          tau.i <- c(tau.i, tau.stat(data.mod[, remaining.edges[i]], data.mod[, j], K)) 
        }
    }
    cat("Root Found: ")
    
    root.index <- which.min(abs(tau.min))
    root <- remaining.edges[root.index]
    cat(root)
    cat("\n")
    ordering <- c(ordering, root)
    remaining.edges <- remaining.edges[-root.index]
    
    # remove effect
    for(i in remaining.edges){
      data.mod[,i ] <- lm(data.mod[,i]~ data.mod[,root])$resid
    }
  }
  ordering <- c(ordering, remaining.edges)
  
  A <- matrix(0, V, V)
  # pruning
  # for(i in V:2){
  #   A[i, 1:(i-1)] <- summary(lm(data[ ,ordering[i]] ~ data[ , ordering[1:(i-1)]]))$coefficients[-1, 4] < cutoff
  # }
  # A <- A + 2 * t(A)
  return(list(ordering = ordering, A = A))
}

tau.stat <- function(A, B, k) {
  mean(A^(k - 1) * B) * mean(A^2) - mean(A^k) * mean(A * B)
}


get_min_tau <- function(Y_i, Y_j, newest_node, ordered_nodes) {
  
}

tau.stat <- function(A, B, k) {
  mean(A^(k - 1) * B) * mean(A^2) - mean(A^k) * mean(A * B)
}


data <- rDAG_degree(p = 200, n = 5000, maxInDeg = 5)
# st <- proc.time()
# out <- ng_dag_search(t(data$Y))
# end <- proc.time()