

sim.size <- 10000
res <- rep(0, sim.size)
n.list <- c(5, 10, 15, 20,30, 40, 50, 75, 100)
param.list <- data.frame(dist = c(rep("gamma", 4), rep("unif", 4)), k1 = c(3, 3, 3, 4, 3, 4, 4,4), k2 = c(3,4, 5, 4, 3, 4, 3,6))
rec <- matrix(0, nrow = length(n.list), ncol = dim(param.list)[1])




for(n.ind in 1:length(n.list)){
  n <- n.list[n.ind]
  for(z in 1:dim(param.list)[1]){
    dist = param.list$dist[z]
    K1 <- param.list$k1[z]
    K2 <- param.list$k2[z]
  for(s in 1:sim.size){
    coefs <- runif(1, .65, 1) * sample(c(-1, 1), 1, replace = T)
    scaleParams <- runif(2, .8, 1)  
    if(dist == "gamma"){
      A <- (rgamma(n, 2, sqrt(2)) - 2 / sqrt(2)) * scaleParams[1]
      B <-  coefs * A + (rgamma(n, 2, sqrt(2)) - 2 / sqrt(2)) * scaleParams[2]
    } else {
      A <- runif(n, -sqrt(3), sqrt(3)) * scaleParams[1]
      B <- coefs * A + runif(n, -sqrt(3), sqrt(3)) * scaleParams[2]
    }
    
    resA <- A #lm(A ~ X + Y - 1)$res
    resB <- B #lm(B ~ X + Y - 1)$res
    
    statAcauseB <- abs(mean(resA^(K1-1) * B) * mean(resA^2) - mean(resA^K1) * mean(resA * B)) +
      abs(mean(resA^(K2-1) * B) * mean(resA^2) - mean(resA^K2) * mean(resA * B)) 
    statBcauseA <- abs(mean(resB^(K1-1) * A) * mean(resB^2) - mean(resB^K1) * mean(A * resB)) + 
      abs(mean(resB^(K2-1) * A) * mean(resB^2) - mean(resB^K2) * mean(A * resB))
  res[s] <- statBcauseA > statAcauseB
  }
  rec[n.ind, z] <- mean(res)
  }
}
rec


matlabComps <- read.csv("lingRes.csv", header = F)


setEPS()


postscript("~/Dropbox/highDimDag/paper/v2/figures/gammaCompare.eps")
plot(matlabComps[, 1], xaxt = "n", ylim = c(.4, 1), type = "b", main = "Gamma Errors", ylab = "Correct Direction", xlab = "n", col = "red")
points(matlabComps[, 2], pch = 2, lty = 2, type = "b", col = "red")
points(rec[, 1], pch = 3, lty = 3, type = "b", col = "blue")
points(rec[, 4], pch = 4, lty = 4, type = "b", col = "blue")
points(rec[, 2], pch = 5, lty = 5, type = "b", col = "blue")
points(rec[, 3], pch = 6, lty = 6, type = "b", col = "blue")
axis(at = c(1:9), labels = n.list, side = 1)
legend("bottomright", pch = 1:6, lty = 1:6, legend = c("Pair", "Direct", "Tau-3", "Tau-4", "Tau-34", "Tau-35"), ncol = 2, col = c(rep("red", 2), rep("blue", 4)))
dev.off()

postscript("~/Dropbox/highDimDag/paper/v2/figures/unifCompare.eps")
plot(matlabComps[, 3], xaxt = "n", ylim = c(.4, 1), type = "b", main = "Uniform Errors", ylab = "Correct Direction", xlab = "n", col = "red")
points(matlabComps[, 4], pch = 2, lty = 2, type = "b", col = "red")
points(rec[, 5], pch = 3, lty = 3, type = "b", col = "blue")
points(rec[, 6], pch = 4, lty = 4, type = "b", col = "blue")
points(rec[, 7], pch = 5, lty = 5, type = "b", col = "blue")
points(rec[, 8], pch = 6, lty = 6, type = "b", col = "blue")
axis(at = c(1:9), labels = n.list, side = 1)
legend("bottomright", pch = 1:6, lty = 1:6, legend = c("Pair", "Direct", "Tau-3", "Tau-4", "Tau-34", "Tau-46"), col = c(rep("red", 2), rep("blue", 4)), ncol = 2)
dev.off()