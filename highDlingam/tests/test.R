######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################

library(doParallel, calcTauCHelper)

ncores <- 3

cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 10
n <- 1000
distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

cs <- 0


sim.size <- 5
timing.rec <- cor.rec <- rep(0, sim.size)

for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = distro, varyScale = T)
    cov(out_dag$Y)
    cov(out_dag$errs)
    time1 <- system.time(output1 <- findGraphSingleFast(scale(out_dag$Y), maxInDegree = 3, cutOffScaling = cs,
                                                             fun = max, degree = deg, verbose = F))
    cor.rec[i] <- cor(output1$topOrder, 1:p, method = "kendall")
    timing.rec[i] <- time1[3]
    ### update table
    count <- count + 1
  }
stopCluster(cl)
cor.rec
