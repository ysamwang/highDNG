# Simulation: Chain Graph
# We simulate from a chain graph where the first node is
# non-Gaussian, but all others are Gaussian and show that
# the graph structure is still discoverable


library(doParallel)
source("functions/rDAG.R")
source("functions/findGraphMulti.R")
source("functions/findGraphSingle.R")
source("functions/findGraphSingleFast.R")
source("functions/findGraphSingleScaled.R")

ncores <- 3
cl <- makeCluster(ncores)
registerDoParallel(cl)

sim.size <- 50
p <- 3
n.list <- c(5000, 10000, 20000)
count <- 0
res <- matrix(0, nrow = length(n.list) * sim.size, ncol = 2)
res[, 1] <- rep(n.list, each = sim.size)

for(n in n.list){
  cat("\n")
  cat("N =")
  cat(n)
  cat("\n")
  for(i in 1:sim.size){
    cat(i)
    cat(" ")
    count <- count + 1
    # generate errors to be mean = 0 with variance as seleced in var.scales
    var.scales <- runif(p, .9, 1.1)
    errs <- sapply(var.scales, function(x){rnorm(n, sd = sqrt(x))})
    errs[, 1] <- rgamma(n, .1, sqrt(.1)) - (.1 / sqrt(.1))
    
    # Draw edge weights
    B <- matrix(0, nrow = p, ncol = p)
    B[lower.tri(B)] <- runif(p * (p-1)/2, .5, 1)
    Y <- t(solve(diag(rep(1, p)) - B, t(errs)))
    Y <- scale(Y)
    
    output.modDL <- findGraphSingle(Y, maxInDegree = p, degree = 3, cutOffScaling = 0, verbose = F)
    # output.lingam <- pcalg::lingam(X = scale(Y), verbose = F)
    apply(output.lingam$B, MAR = 1, function(x){max(which(x != 0),0)})
    res[count, 2] <- cor(output.modDL$topOrder, 1:p, method = "kendall")
  }
}
stopCluster(cl)
# par(mfrow = c(3,2))
# hist(Y[, 1])
# hist(Y[, 2])
# hist(Y[, 3])
# hist(Y[, 4])
# hist(Y[, 5])
boxplot(res[, 2] ~ res[, 1])

aggregate(res[, 2], by = list(res[, 1]), FUN = function(x){mean(x > .99)})

Y <- scale(Y, center = F)
.calcTau(3, Y[, 3], Y[, 1])
.calcTau(3, Y[, 1], Y[, 5])
