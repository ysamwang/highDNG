######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################

library(doParallel)
library(highDLingam)
ncores <- 6
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)



cs <- .8


sim.size <- 40
val.list <- c(50)
timing.rec <- cor.rec4 <- matrix(0, nrow = sim.size * length(val.list), ncol = 1)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_hub(p = varParam, n = varParam, lowScale = .8, highScale = 1,
                    lowEdge = .7, highEdge = .95, numHubs = 2)
    Y <- out_dag$Y.unif
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = 2, cutOffScaling = cs, degree = 4,
                                                   verbose = F))
    
    cor.rec4[count, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
    timing.rec[count, ] <- time3[3]
    ### update table
    count <- count + 1
    # write.csv(cor.rec,"~/highDL/hub_cor.csv")
    # write.csv(timing.rec,"~/highDL/hub_timing.csv")
  }
}


timing.rec <- cor.rec2 <- matrix(0, nrow = sim.size * length(val.list), ncol = 1)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_hub(p = varParam, n = varParam, lowScale = .3, highScale = .5,
                        lowEdge = .6, highEdge = .95, numHubs = 2)
    Y <- out_dag$Y.unif
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = 2, cutOffScaling = cs, degree = 4,
                                                   verbose = F))
    
    cor.rec2[count, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
    timing.rec[count, ] <- time3[3]
    ### update table
    count <- count + 1
    # write.csv(cor.rec,"~/highDL/hub_cor.csv")
    # write.csv(timing.rec,"~/highDL/hub_timing.csv")
  }
}






mean(cor.rec)
mean(cor.rec1)
mean(cor.rec2)
mean(cor.rec3)
t.test(cor.rec, cor.rec1)
