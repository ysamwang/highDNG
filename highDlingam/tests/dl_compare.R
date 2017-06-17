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



distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

maxInDegree <- 3
cs <- .8


sim.size <- 500
val.list <- c(5, 10, 15, 20)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 2)
count <- 1
for(varParam in val.list){
  cat("\n===== ")
  cat(varParam)
  cat( "=====\n")
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG(p = varParam, n = varParam * 50, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1)
    Y <- out_dag$Y

    time1 <- system.time(output1 <- findGraphSingleFast(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                        verbose = F))
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                   verbose = F))

    cor.rec[count, ] <- c(cor(output1$topOrder, 1:varParam, method = "kendall"),
                          cor(output3$topOrder, 1:varParam, method = "kendall"))
    timing.rec[count, ] <- c(time1[3], time3[3])
    ### update table
    count <- count + 1
  }

}

write.csv(cor.rec,"./tests/testOutput/compare_cor50_a.csv")
write.csv(timing.rec,"./tests/testOutput/compare_timing50_a.csv")

stopCluster(cl)


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



distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

maxInDegree <- 3
cs <- .8


sim.size <- 500
val.list <- c(5, 10, 15, 20)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 2)
count <- 1
for(varParam in val.list){
  cat("\n===== ")
  cat(varParam)
  cat( "=====\n")
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG(p = varParam, n = varParam * 10, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1)
    Y <- out_dag$Y
    
    time1 <- system.time(output1 <- findGraphSingleFast(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                        verbose = F))
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                   verbose = F))
    
    cor.rec[count, ] <- c(cor(output1$topOrder, 1:varParam, method = "kendall"),
                          cor(output3$topOrder, 1:varParam, method = "kendall"))
    timing.rec[count, ] <- c(time1[3], time3[3])
    ### update table
    count <- count + 1
  }
  
}
write.csv(cor.rec,"./tests/testOutput/compare_cor10_a.csv")
write.csv(timing.rec,"./tests/testOutput/compare_timing10_a.csv")






stopCluster(cl)

