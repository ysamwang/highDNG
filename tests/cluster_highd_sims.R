p <- 1000
n <- 1000
ncores <- 6
sim.size <- 1
args <- commandArgs(TRUE)

for(k in 1:length(args)){
  eval(parse(text=args[[k]]))
}

library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")
# source("findGraphSingle.R")

cl <- makeCluster(ncores)
registerDoParallel(cl)


timing.rec <- matrix(0, nrow = sim.size, ncol = 2)
cor.rec <- matrix(0, nrow = sim.size, ncol = 2)
for(i in 1:sim.size){
  cat(i)
  cat("\n")
  out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")

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

write.table(cor.rec, file = "cor_rec.csv", sep = ",")
write.table(timing.rec, file = "timing_rec.csv", sep = ",")
            