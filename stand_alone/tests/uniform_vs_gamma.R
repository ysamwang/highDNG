######################################################
#
# Uniform vs Gamma
#
######################################################

library(doParallel)
source("functions/rDAG.R")
source("functions/findGraphMulti.R")



ncores <- 6

cl <- makeCluster(ncores)
registerDoParallel(cl)


p <- 20
n.list <- c(20, 100, 200, 500)
sim.size <- 50 
timing.rec <- cor.rec <- matrix(0, nrow = sim.size * length(n.list), ncol = 3)
timing.rec[, 1] <- cor.rec[, 1] <- rep(n.list, each = sim.size)
colnames(timing.rec) <- colnames(cor.rec) <- c("n", "unif", "gamma")
count <- 1

for(n in n.list){
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag_unif <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "unif")
    out_dag_gamma <- rDAG_degree(p = p, n = n, maxInDeg = 3, dist = "gamma")
    
    time1 <- system.time(output1 <- findGraphMulti(out_dag_unif$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 4, cutOffScaling = .5, verbose =F))
    
    time2 <- system.time(output2 <- findGraphMulti(out_dag_gamma$Y, subsets = F,
                                                   maxInDegree = 3, fun = max,
                                                   degree = 3, cutOffScaling = .5, verbose =F))
    
    
    
    cor.rec[count, 2:3 ] <-c(cor(output1$topOrder, 1:p, method = "kendall"),
                     cor(output2$topOrder, 1:p, method = "kendall"))
    timing.rec[count, 2:3 ] <- c(time1[3], time2[3])
    count <- count + 1
    ### update table
    write.table(cor.rec, file = "cor_unifGamma.csv", sep = ",")
    write.table(timing.rec, file = "timing_unifGamma.csv", sep = ",")
    
  }
}

stopCluster(cl)

cor.table <- read.table(file = "cor_unifGamma.csv", sep = ",")
time.table <- read.table(file = "timing_unifGamma.csv", sep = ",")

print(xtable::xtable(aggregate(unif ~ n, data = cor.table, FUN = mean)), include.rownames = FALSE)
print(xtable::xtable(aggregate(gamma ~ n, data = cor.table, FUN = mean)), include.rownames = FALSE)

print(xtable::xtable(aggregate(unif ~ n, data = time.table, FUN = mean)), include.rownames = FALSE)
print(xtable::xtable(aggregate(gamma ~ n, data = time.table, FUN = mean)), include.rownames = FALSE)

png("unif_vs_gamma.png", width= 800, height = 480)
par(mfrow = c(2, 1))
boxplot(cor.rec[, 2]~ cor.rec[, 1], names = n.list, xlab = "n", ylab = "Kendall Tau", ylim = c(0,1), main = "Uniform Errors")
boxplot(cor.rec[, 3]~ cor.rec[, 1], names = n.list, xlab = "n", ylab = "Kendall Tau", ylim = c(0,1), main = "Gamma Errors")
dev.off()