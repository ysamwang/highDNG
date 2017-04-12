######################################################
#
# Compare C++ inner function vs fastLM function
#
######################################################

library(doParallel)
source("functions/rDAG.R")
source("functions/findGraphMulti.R")
source("functions/findGraphSingle.R")
source("functions/findGraphSingleFast.R")
source("functions/findGraphSingleScaled.R")

ncores <- 3

cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 50
n <- 250
distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}

cs <- .7


sim.size <- 5
val.list <- c(T, F)
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(val.list), ncol = 4)
count <- 1
for(varParam in val.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = distro, varyScale = varParam)
    cov(out_dag$Y)
    cov(out_dag$errs)
    time1 <- system.time(output1 <- findGraphSingle(out_dag$Y, maxInDegree = 3, cutOffScaling = cs,
                                                             fun = max, degree = deg, verbose = F))
    time2 <- system.time(output2 <- findGraphSingle(scale(out_dag$Y), maxInDegree = 3, cutOffScaling = cs,
                                                    fun = max, degree = deg, verbose =F ))
    
    time3 <- system.time(output3 <- findGraphSingleScaled(out_dag$Y, maxInDegree = 3, cutOffScaling = cs,
                                                        fun = max, degree = deg, verbose = F))
    time4 <- system.time(output4 <- findGraphSingleScaled(scale(out_dag$Y), maxInDegree = 3, cutOffScaling = cs,
                                                          fun = max, degree = deg, verbose = F))

    cor.rec[count, ] <- c(cor(output1$topOrder, 1:p, method = "kendall"),
                      cor(output2$topOrder, 1:p, method = "kendall"),
                      cor(output3$topOrder, 1:p, method = "kendall"),
                      cor(output4$topOrder, 1:p, method = "kendall"))
    timing.rec[count, ] <- c(time1[3], time2[3],time3[3], time4[3])
    ### update table
    count <- count + 1
  }
  
}



stopCluster(cl)
par(mfrow = c(1,2))
boxplot(cor.rec[1:sim.size,], ylim = c(-1, 1),
        names = c("U/U", "U/S", "S/U", "S/S"))
abline(h = 0, col = "red")
boxplot(cor.rec[(sim.size +1):(2 * sim.size),], ylim = c(-1, 1),
        names = c("U/U", "U/S", "S/U", "S/S"))
abline(h = 0, col = "red")