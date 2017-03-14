sim.size <- 100
p <- 20
n <- 1000
res.kendall <- matrix(0,nrow =  sim.size, ncol = 4)
res.time <- matrix(0, nrow = sim.size, ncol = 4)

for(i in 1:sim.size){
  cat(i)
  out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 4, dist = "gamma")
  Y <- out_dag$Y
  
  # min max
  res.time[i, 1] <- system.time(output1 <- findGraphMinMax(Y, maxInDeg = 4))[3]
  res.kendall[i, 1] <-cor(output1$topOrder, 1:length(output1$topOrder), method = "kendall")
  # 
  # # min over all sets
  res.time[i, 2] <- system.time(output2 <- findGraphMinAllSets(Y, maxInDeg = 4))[3]
  res.kendall[i, 2] <-cor(output2$topOrder, 1:length(output2$topOrder), method = "kendall")

  # rising Cut
  res.time[i, 3] <- system.time(output3 <- findGraphRisingCut(Y, maxInDeg = 4))[3]
  res.kendall[i, 3] <-cor(output3$topOrder, 1:length(output3$topOrder), method = "kendall")
}

colMeans(res.kendall)
colMeans(res.time)

