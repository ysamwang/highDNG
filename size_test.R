library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")
# source("findGraphSingle.R")

ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 50
n.list <- c(50, 100, 250, 500, 1000, 2000)
ncores <- 6
sim.size <- 20
args <- commandArgs(TRUE)

timing.rec <- cor.rec <- matrix(0, nrow = length(n.list), ncol = 2)

for(n.index in 1:length(n.list)){
  cat("=====")
  cat(n.list[n.index])
  cat("=====")
  timing.rec.n <- matrix(0, nrow = sim.size, ncol = 2)
  cor.rec.n <- matrix(0, nrow = sim.size, ncol = 2)
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_degree(p = p, n = n.list[n.index], maxInDeg = 3, dist = "unif")
    
    time2 <- system.time(output2 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .5, verbose =T))
    
    time3 <- system.time(output3 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, B = out_dag$B, verbose = T))
    
    
    cor.rec.n[i, ] <-c(#cor(output1$topOrder, 1:p, method = "kendall"),
      cor(output2$topOrder, 1:p, method = "kendall"),
      cor(output3$topOrder, 1:p, method = "kendall"))
    cor.rec.n[i, ] <- c(# time1[3],
      time2[3], time3[3])
  }
  cor.rec[n.index, ] <- colMeans(cor.rec.n)
  timing.rec[n.index, ] <- colMeans(timing.rec.n)
  
}
stopCluster(cl)


write.table(cor.rec, file = "cor_rec.csv", sep = ",")
write.table(timing.rec, file = "timing_rec.csv", sep = ",")
