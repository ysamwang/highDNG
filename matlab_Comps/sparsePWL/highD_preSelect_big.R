library(highDLingam)
library(doParallel)

ind <- 2

args <- (commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text = args[[i]]))
}
print(ind)

val.list <- c(100, 200, 500, 1000, 1500)
varParam <- 1500

detectCores()
ncores <- detectCores() - 1
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


sim.size <- 2
timing.rec <- cor.rec <- matrix(0, nrow = sim.size, ncol = 1)

for(i in 1:sim.size){
  cat(i)
  cat("\n")
  # out_dag <- rDAG(p = varParam, n = varParam * 3/4, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
  #                 lowEdge = .65, highEdge = 1)
  # Y <- out_dag$Y
  # 
  
  Y <- as.matrix(read.csv(paste("dat/data_", distro, "_", varParam, "_", i, ".csv", sep = "")))[, -1]
  skel <- as.matrix(read.csv(paste("dat/skeleton_", distro, "_", varParam, "_", i, ".csv", sep = "")))[, -1]
  time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, B = skel, degree = deg,
                                                 verbose = T))
  
  cor.rec[i, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
  timing.rec[i, ] <- time3[3]
  ### update table
  write.csv(cor.rec, paste("preSelect_", distro, "_cor_", varParam, "_",i,".csv", sep = ""))
  write.csv(timing.rec,paste("preSelect_", distro, "_timing_", varParam, "_", i ,".csv", sep = ""))
}
stopCluster(cl)

