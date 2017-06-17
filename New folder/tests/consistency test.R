library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")


args <- commandArgs(TRUE)
for(i in 1:length(args)){
  eval(parse(text=args[[i]]))
}

ncores <- 6
sim.size <- 10

n.list <- c(500)

cl <- makeCluster(ncores)
registerDoParallel(cl)

timing.rec <- cor.rec <- matrix(0, nrow = length(n.list) * sim.size, ncol = 3)
timing.rec[, 1] <- cor.rec[, 1] <- rep(n.list, each = sim.size)
count <- 1
for(n.index in 1:length(n.list)){
  cat("=====")
  cat(n.list[n.index])
  cat("=====")
  timing.rec.n <- matrix(0, nrow = sim.size, ncol = 2)
  cor.rec.n <- matrix(0, nrow = sim.size, ncol = 2)
  p <- n.list[n.index]
  n <- n.list[n.index]
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")
    
    time2 <- system.time(output2 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .5, verbose =T))
    
    time3 <- system.time(output3 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, B = out_dag$B, verbose = F))
    
    
    cor.rec[count, 2:3] <-c(cor(output2$topOrder, 1:p, method = "kendall"),
      cor(output3$topOrder, 1:p, method = "kendall"))
    timing.rec[count, 2:3] <- c(time2[3], time3[3])
    count <- count + 1
    
    ### update table
    write.table(cor.rec, file = "cor_rec_consist.csv", sep = ",")
    write.table(timing.rec, file = "timing_rec_consist.csv", sep = ",")
    
  }
}
stopCluster(cl)
