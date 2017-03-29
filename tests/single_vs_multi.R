######################################################
#
# Uniform vs Gamma
#
######################################################

library(doParallel)
source("functions/rDAG.R")
source("functions/findGraphMulti.R")
source("functions/findGraphSingle.R")



ncores <- 6

cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 50
n.list <- c(500)
sim.size <- 50 
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(n.list), ncol = 3)
timing.rec[, 1] <- cor.rec[, 1] <- rep(n.list, each = sim.size)
colnames(timing.rec) <- colnames(cor.rec) <- c("n", "multi", "single")
count <- 1

for(n in n.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag_unif <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")

    time1 <- system.time(output1 <- findGraphMulti(out_dag_unif$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .8, verbose =F))
    
    time2 <- system.time(output2 <- findGraphSingle(out_dag_unif$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .8, verbose =F))
    
    
    
    cor.rec[count, 2:3 ] <-c(cor(output1$topOrder, 1:p, method = "kendall"),
                             cor(output2$topOrder, 1:p, method = "kendall"))
    timing.rec[count, 2:3 ] <- c(time1[3], time2[3])
    count <- count + 1
    ### update table
    write.table(cor.rec, file = "cor_singleMulti.csv", sep = ",")
    write.table(timing.rec, file = "timing_singleMulti.csv", sep = ",")
    
  }
}

stopCluster(cl)

png("singleMulti.png", width = 600, height = 350)
par(mfrow = c(1,2))
boxplot(cor.rec[, 2:3], names = c("Max-Min", "Min-Max"), ylab = "Kendall's Tau") 
boxplot(timing.rec[,2:3], names = c("Max-Min", "Min-Max"), ylab = "Time (s)") 
dev.off()
