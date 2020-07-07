rec <- matrix(0, nrow = 80, ncol = 3)
rec[, 1] <- rep(c(100, 200, 500, 1000), each = 20)
for(i in 1:80){
  dat <- read.csv(paste("subsets_", i, ".csv", sep = ""))
  rec[i, -1] <- unlist(dat[, -1])
}

postscript("bestSubset.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 8, height = 5)
par(mfrow = c(1,2))
boxplot(rec[,2] ~ rec[,1], xlab = "p", ylab = "Kendall's Tau", ylim = c(0, 1))
boxplot(rec[,3]/60 ~ rec[,1], log = "y", xlab = "p", ylab = "Time (min)", ylim = c(.1, 1000), yaxt = "n")
axis(side = 2, at = c(1, 5, 10, 100, 500, 1000))
dev.off()