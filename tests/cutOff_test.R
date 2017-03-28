######################################################
#
# Build intuition for good cut off scaling value
#
######################################################
library(doParallel)
source("rDAG.R")
source("findGraphMulti.R")



ncores <- 6

cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 50
n <- 1000
sim.size <- 100 
timing.rec <- cor.rec <- matrix(0, nrow = sim.size, ncol = 5)
for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")
    
    time1 <- system.time(output1 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .3, verbose =F))
    
    time2 <- system.time(output2 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .5, verbose =F))
    
    time3 <- system.time(output3 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .8, verbose =F))
    
    time4 <- system.time(output4 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = 1, verbose =F))
  
    
    time5 <- system.time(output5 <- findGraphMulti(out_dag$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, B = out_dag$B, verbose = F))
    
    
    cor.rec[i, ] <-c(cor(output1$topOrder, 1:p, method = "kendall"),
                         cor(output2$topOrder, 1:p, method = "kendall"),
                         cor(output3$topOrder, 1:p, method = "kendall"),
                         cor(output4$topOrder, 1:p, method = "kendall"),
                         cor(output5$topOrder, 1:p, method = "kendall"))
    timing.rec[i, ] <- c(time1[3], time2[3], time3[3], time4[3], time5[3])

    ### update table
    write.table(cor.rec, file = "cor_rec_consist4.csv", sep = ",")
    write.table(timing.rec, file = "timing_rec_consist4.csv", sep = ",")
    
  }

stopCluster(cl)


boxplot(cor.rec, names = c(.3, .5, .8, 1, "oracle"), xlab = "Pruning Parameter", ylab = "Kendall Tau")
boxplot(timing.rec, names = c(.3, .5, .8, 1, "oracle"), xlab = "Pruning Parameter", ylab = "Seconds")
