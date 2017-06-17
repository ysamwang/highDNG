######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################

ncores <- 12
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)



distro <- "gamma"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

cs <- .5
maxInDegree <- 3
scaleData <- F
sim.size <- 500
val.list <- c(10, 25, 50, 100)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 2)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG(p = varParam, n = varParam * 5, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1)

    Y <- scale(out_dag$Y, center = scaleData, scale = scaleData)
    
    time1 <- system.time(output1 <- findGraphSingleFast(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                        verbose = F))
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                   verbose = F))
    
    cor.rec[count, ] <- c(cor(output1$topOrder, 1:varParam, method = "kendall"),
                          cor(output3$topOrder, 1:varParam, method = "kendall"))
    timing.rec[count, ] <- c(time1[3], time3[3])
    ### update table
    count <- count + 1
    write.csv(cor.rec,"~/udrive/testOutput/singMulti_cor.csv")
    write.csv(timing.rec,"~/udrive/testOutput/singMulti_timing.csv")
  }
  
}

stopCluster(cl)

cor.rec <- as.matrix(read.csv("tests/testOutput/singMulti_cor.csv"))[, - 1]
timing.rec <- as.matrix(read.csv("tests/testOutput/singMulti_timing.csv"))[, - 1]

png("tests/testOutput/single_vs_multi.png", width = 800, height = 600)
par(mfrow = c(1,2), oma = c(0, 1, 0, 0))
boxplot(c(cor.rec) ~ rep(c("Min-Max", "Max-Min"), each = sim.size * length(val.list)) + rep(rep(val.list, each = sim.size), times = 2),
        ylim = c(-1, 1), at = rep(c(1:length(val.list)* 2), each = 2) + rep(c(-.25, .25), times = length(val.list) ),
        xlab = "p", boxwex = .2, col = c("lightgray", "gray37"), xaxt = "n")
axis(side = 1, at = c(1:length(val.list))*2, labels = val.list)
abline(h = 0, col = "red", lty = 2)
mtext("Kendall's tau", side = 2, line = 2)
boxplot(c(timing.rec) ~ rep(c("Min-Max", "Max-Min"), each = sim.size * length(val.list)) + rep(rep(val.list, each = sim.size), times = 2),
        at = rep(c(1:length(val.list)* 2), each = 2) + rep(c(-.25, .25), times = length(val.list) ),
        xlab = "p", boxwex = .2, col = c("lightgray", "gray37"), xaxt = "n")
axis(side = 1, at = c(1:length(val.list))*2, labels = val.list)
axis(side = 1, at = c(1:length(val.list)* 2), labels = val.list)
mtext("Time (s)", side = 2, line = 2)
dev.off()


