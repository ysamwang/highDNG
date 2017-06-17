p <- 500
n <- 1000
ncores <- 6
sim.size <- 5


library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")

cl <- makeCluster(ncores)
registerDoParallel(cl)


timing.rec <- cor.rec <- dist.rec <- matrix(0, nrow = sim.size, ncol = 2)
for(i in 1:sim.size){
  cat(i)
  cat("\n")
  out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")

  time1 <- system.time(output1 <- findGraphMulti(out_dag$Y, subsets = F,
                                                 maxInDegree = 3, fun = max,
                                                 degree = 4, cutOffScaling = 1, verbose = F))
  
  time2 <- system.time(output2 <- findGraphMulti(out_dag$Y, subsets = F,
                                                 maxInDegree = 3, fun = max,
                                                 degree = 4, B = out_dag$B, verbose = F))
  
  cor.rec[i, ] <-c(cor(output1$topOrder, 1:p, method = "kendall"),
                   cor(output2$topOrder, 1:p, method = "kendall"))
  
  timing.rec[i, ] <- c(time1[3], time2[3])
}
stopCluster(cl)

write.table(cor.rec, file = "testOutput/cor_rec_highD.csv", sep = ",")
write.table(timing.rec, file = "testOutput/timing_rec_highD.csv", sep = ",")
            