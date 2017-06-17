######################################################
#
# Uniform vs Gamma
#
######################################################

library(doParallel)
ncores <- 6

cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 30
n.list <- c(100, 200, 500, 1000)
sim.size <- 10
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(n.list), ncol = 3)
colnames(timing.rec) <- colnames(cor.rec) <- c("n", "unif", "gamma", "gamma4")
count <- 1

for(n in n.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag_unif <- rDAG(p = p, n = n, maxInDeg = 3, dist = "unif", varyScale = T)
    out_dag_gamma <- rDAG(p = p, n = n, maxInDeg = 3, dist = "gamma", varyScale = T)
    
    time1 <- system.time(output1 <- findGraphSingleFast(scale(out_dag_unif$Y), maxInDegree = 3,
                                                   degree = 4, cutOffScaling = .3, verbose =F))
    
    time2 <- system.time(output2 <- findGraphSingleFast(scale(out_dag_gamma$Y), maxInDegree = 3,
                                                        degree = 3, cutOffScaling = .3, verbose =F))
    
    time3 <- system.time(output3 <- findGraphSingleFast(scale(out_dag_gamma$Y), maxInDegree = 3,
                                                        degree = 4, cutOffScaling = .3, verbose =F))
    
    
    
    cor.rec[count,] <-c(cor(output1$topOrder, 1:p, method = "kendall"),
                     cor(output2$topOrder, 1:p, method = "kendall"),
                     cor(output3$topOrder, 1:p, method = "kendall"))
    timing.rec[count,] <- c(time1[3], time2[3], time3[3])
    count <- count + 1
    ### update table
    # write.table(cor.rec, file = "cor_unifGamma.csv", sep = ",")
    # write.table(timing.rec, file = "timing_unifGamma.csv", sep = ",")
    
  }
}

stopCluster(cl)

png("tests/testOutput/unif_vs_gamma.png", width = 800, height = 600)
out <- boxplot(c(cor.rec) ~ rep(c("Unif", "Gamma-3", "Gamma-4"), each = sim.size * length(n.list)) + rep(rep(n.list, each = sim.size), times = 3),
        ylim = c(-1, 1), at = rep(c(1:length(n.list)* 2), each = 3) + rep(c(-.25, 0, .25), times = length(n.list)),
        xlab = "p", boxwex = .2, col = c("green", "blue", "red"), xaxt = "n", ylab = "Kendall's Tau")
axis(side = 1, at = c(1:length(val.list))*2, labels = val.list)
abline(h = 0, col = "red", lty = 2)
dev.off()