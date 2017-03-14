library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")
source("findGraphSingle.R")

cl <- makeCluster(6)
registerDoParallel(cl)
p <- 500
n <- 1000

sim.size <- 1
timing.rec <- matrix(0, nrow = sim.size, ncol = 2)
cor.rec <- matrix(0, nrow = sim.size, ncol = 2)
for(i in 1:sim.size){
  cat(i)
  cat("\n")
  out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")
  # time1 <- system.time(output1 <- findGraphMulti(out_dag$Y, subsets = F,
  #                                                maxInDegree = 3, fun = max,
  #                                                degree = 4, pruningCut = 0, verbose = F))
  
  time2 <- system.time(output2 <- findGraphMulti(out_dag$Y, subsets = F,
                                                 maxInDegree = 3, fun = max,
                                                 degree = 4, cutOffScaling = .5, verbose =T))
  
  time3 <- system.time(output3 <- findGraphMulti(out_dag$Y, subsets = F,
                                                 maxInDegree = 3, fun = max,
                                                 degree = 4, B = out_dag$B, verbose = T))
  

  cor.rec[i, ] <-c(#cor(output1$topOrder, 1:p, method = "kendall"),
  cor(output2$topOrder, 1:p, method = "kendall"),
  cor(output3$topOrder, 1:p, method = "kendall"))
  timing.rec[i, ] <- c(# time1[3],
                       time2[3], time3[3])
}
stopCluster(cl)

colMeans(cor.rec)
colMeans(timing.rec)


######## Unif results #############
# p = 50, n = 100
# > colMeans(cor.rec)
# [1] 0.4969796 0.8958367
# > colMeans(timing.rec)
# [1] 6.58585 4.04510


#####################################################################################################
library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")
source("findGraphSingle.R")

cl <- makeCluster(6)
registerDoParallel(cl)
p <- 100
n <- 1000
out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")
apply(out_dag$Y, MAR = 2, FUN = var)
time2 <- system.time(output2 <- findGraphMulti(out_dag$Y, subsets = F,
                                               maxInDegree = 3, fun = max,
                                               degree = 4, cutOffScaling = .5))
stopCluster(cl)
cor(output2$topOrder, 1:p, method = "spearman")
