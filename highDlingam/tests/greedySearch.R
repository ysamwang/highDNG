library(highDLingam)
library(doParallel)

ind <- 1

args <- (commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text = args[[i]]))
}
print(ind)


p.list <- c(100, 200, 500, 1000, 1500, 2000)
p <- p.list[ceiling(ind/20)]

getNextRoot <- function(ord, unord, J, cl){
  
  getTauGreedy <- function(v, ord, unord, J){
    
    testPar <- function(np,v, pa, unordered, k){
      v.res <- RcppArmadillo::fastLm(Y[, c(pa, np), drop = F], Y[,v])$res 
      max(sapply(setdiff(unordered, v), function(u){calcTau(k, v.res, Y[,u])}))
    }
    
    calcTau <- function(k, pa, ch) {
      abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
    }
    
    pa <- c()
    while(length(pa) < J & length(ord) > 0){
      upd <- ord[which.min(sapply(ord, testPar, v = 10, pa = pa, unordered = setdiff(unord,v), k = 4))]
      pa <- c(pa, upd)
      ord <- setdiff(ord, upd)
    }
    
    
    v.res <- RcppArmadillo::fastLm(Y[, c(pa), drop = F], Y[, v])$res 
    return(max(sapply(setdiff(unord, v), function(u){calcTau(k, v.res, Y[,u])})))
  }
  
  unord[which.min(parSapply(cl = cl, unord, getTauGreedy, ord, unord, J))]
}


getNextRootSubSet <- function(ord, unord, J, cl){
  
  calcTau <- function(k, pa, ch) {
    abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
  }
  
  getTauSubSet <- function(v, ord, unord, J){
    if(length(ord) > J){
      out <- leaps::regsubsets(x = Y[, ord, drop = F], y = Y[, v], nbest = 1, nvmax = J, really.big = T)
      pa <- ord[which(summary(out)$which[J,-1])]
      v.res <- RcppArmadillo::fastLm(Y[, pa, drop = F], Y[,v])$res
    } else if(length(ord) == 0){
      v.res <- Y[,v]
    } else {
      v.res <- RcppArmadillo::fastLm(Y[, ord, drop = F], Y[,v])$res
    }
    
    max(sapply(setdiff(unord, v), function(u){calcTau(k, v.res, Y[,u])}))
  }
  
  
  unord[which.min(parSapply(cl = cl, unord, getTauSubSet, ord, unord, J))]
}


#########






ncores <- detectCores() - 2
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)



distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}
maxInDegree <- 2
cs <- .8



rec <- matrix(0, nrow = 1, ncol = 2)
J <- 2
k <- 4
cor.rec <- timing.rec <- matrix(0, nrow = sim.size, ncol = 1)
for(i in 1:sim.size){
  out_dag <- rDAG(p = p, n = 3/4 * p, maxInDeg = J, 
                  dist = distro, lowScale = .8, highScale = 1,
                  lowEdge = .5, highEdge = 1)
  Y <- out_dag$Y
  
  # time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
  #                                                verbose = F))
  # 
  # cor.rec[i, 1] <- cor(output3$topOrder, 1:p, method = "kendall")
  # timing.rec[i, 1] <- time3[3]
  
  clusterExport(cl = cl, c("Y", "k"), envir = .GlobalEnv)
  
  

  
  ### Best SubSet ###
  st <- proc.time()
  ord <- c()
  unord <- c(1:p)
  while(length(unord) > 1){
    nextRoot <- getNextRootSubSet(ord, unord, J, cl)
    ord <- c(ord, nextRoot)
    unord <- setdiff(unord, nextRoot)
  }
  end <- proc.time()
  
  rec[1] <- cor(c(ord, unord), 1:p, method = "kendall")
  rec[2] <- (end-st)[3]
  
  write.csv(rec,paste("~/hdl/results/subsets_", ind, ".csv", sep = "")) 
  
}
stopCluster(cl)
