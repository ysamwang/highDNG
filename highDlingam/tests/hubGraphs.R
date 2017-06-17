######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################

library(doParallel)
library(highDLingam)
ncores <- 48
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)



cs <- .8


sim.size <- 10
val.list <- c(100, 200, 500, 1000, 1500)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 1)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_hub(p = varParam, n = varParam * 3/2 , lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1)
    Y <- out_dag$Y.unif
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = 2, cutOffScaling = cs, degree = deg,
                                                   verbose = F))
    
    cor.rec[count, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
    timing.rec[count, ] <- time3[3]
    ### update table
    count <- count + 1
    write.csv(cor.rec,"~/highDL/hub_cor.csv")
    write.csv(timing.rec,"~/highDL/hub_timing.csv")
  }
}


stopCluster(cl)
