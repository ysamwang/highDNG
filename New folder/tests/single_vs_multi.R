######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################

library(doParallel)
ncores <- 6

cl <- makeCluster(ncores)
registerDoParallel(cl)



distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

cs <- .3


sim.size <- 100
val.list <- c(10, 30, 80)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 2)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG(p = varParam, n = varParam * 5, maxInDeg = 3, dist = distro, varyScale = T)
    Y <- scale(out_dag$Y)
    time1 <- system.time(output1 <- findGraphSingleFast(Y, maxInDegree = 3, cutOffScaling = cs, degree = deg,
                                                        verbose = F))
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = 3, cutOffScaling = cs, degree = deg,
                                                   verbose = F))
    
    cor.rec[count, ] <- c(cor(output1$topOrder, 1:varParam, method = "kendall"),
                          cor(output3$topOrder, 1:varParam, method = "kendall"))
    timing.rec[count, ] <- c(time1[3], time3[3])
    ### update table
    count <- count + 1
  }
  
}



stopCluster(cl)
png("tests/testOutput/single_vs_multi.png", width = 800, height = 600)
par(mfrow = c(1,2), oma = c(0, 1, 0, 0))
boxplot(c(cor.rec) ~ rep(c("Min-Max", "Max-Min"), each = sim.size * length(val.list)) + rep(rep(val.list, each = sim.size), times = 2),
        ylim = c(-1, 1), at = rep(c(1:length(val.list)* 2), each = 2) + rep(c(-.25, .25), times = length(val.list) ),
        xlab = "p", boxwex = .2, col = c("green", "blue"), xaxt = "n")
axis(side = 1, at = c(1:length(val.list))*2, labels = val.list)
abline(h = 0, col = "red", lty = 2)
mtext("Kendall's tau", side = 2, line = 2)
boxplot(c(timing.rec) ~ rep(c("Min-Max", "Max-Min"), each = sim.size * length(val.list)) + rep(rep(val.list, each = sim.size), times = 2),
        at = rep(c(1:length(val.list)* 2), each = 2) + rep(c(-.25, .25), times = length(val.list) ),
        xlab = "p", boxwex = .2, col = c("green", "blue"), xaxt = "n")
axis(side = 1, at = c(1:length(val.list))*2, labels = val.list)
axis(side = 1, at = c(1:length(val.list)* 2), labels = val.list)
mtext("Time (s)", side = 2, line = 2)
dev.off()



