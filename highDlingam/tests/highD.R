######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################



ncores <- 20
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)



distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

maxInDegree <- 2
cs <- .8


sim.size <- 10
val.list <- c(100, 200, 500, 1000, 1500)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 1)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG(p = varParam, n = varParam * 3/4, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1)
    Y <- out_dag$Y
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                   verbose = F))
    
    cor.rec[count, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
    timing.rec[count, ] <- time3[3]
    ### update table
    count <- count + 1
    write.csv(cor.rec,"~\udrive\testOutput\highD_cor.csv")
    write.csv(timing.rec,"~\udrive\testOutput\highD_timing.csv")
  }
  
}



stopCluster(cl)



png("~\udrive\testOutput\highDcons.png", width = 800, height = 500)
par(mfrow = c(1,2), oma = c(0, 1, 0, 0))
boxplot(c(cor.rec) ~ rep(val.list, each = sim.size),
        ylim = c(-1, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
abline(h = 0, col = "red", lty = 2)
mtext("Kendall's tau", side = 2, line = 2)
boxplot(c(timing.rec) ~ rep(val.list, each = sim.size), at = c(1:length(val.list)) , ylim = c(0, max(timing.rec)*1.2),
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (s)")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
dev.off()

