##############################################
# High Dimensional results with unif errors  #
##############################################

unifRec <- matrix(0, ncol = 3, nrow = 6 * 20)
n.list <- c(100, 200, 500, 1000, 1500, 2000)
unifRec[, 1] <- rep(n.list, each = 20)


for(i in 1:4){
  if(i <4){dat <- read.csv(paste("simResults/highD_unif_cor_", n.list[i], ".csv", sep = ""))[, 2]
  if(i <4){dat_timing <- read.csv(paste("simResults/highD_unif_timing_", n.list[i], ".csv", sep = ""))[, 2]
  unifRec[((i-1) * 20 + 1):(i * 20), 2]  <- dat}
  unifRec[((i-1) * 20 + 1):(i * 20), 3]  <- dat_timing}
}

for(i in 1:20){
  dat10 <- read.csv(paste("simResults/highD_unif_cor_1000_", i, ".csv", sep = ""))[, 2]
  dat10_timing <- read.csv(paste("simResults/highD_unif_timing_1000_", i, ".csv", sep = ""))[, 2]
  unifRec[i + 60, 2]  <- dat10
  unifRec[i + 60, 3]  <- dat10_timing
  dat15 <- read.csv(paste("simResults/highD_unif_cor_1500_", i, ".csv", sep = ""))[, 2]
  dat15_timing <- read.csv(paste("simResults/highD_unif_timing_1500_", i, ".csv", sep = ""))[, 2]
  unifRec[i + 80, 2]  <- dat15
  unifRec[i + 80, 3]  <- dat15_timing
  dat2000 <- read.csv(paste("simResults/highD_unif_cor_2000_", i, ".csv", sep = ""))[, 2]
  unifRec[(i+100), 2]  <- dat2000
  dat2000_timing <- read.csv(paste("simResults/highD_unif_timing_2000_", i, ".csv", sep = ""))[, 2]
  unifRec[(i+100), 3]  <- dat2000_timing
}

unifRecHub <- matrix(0, ncol = 3, nrow = 6 * 20)
n.list <- c(100, 200, 500, 1000, 1500, 2000)
unifRecHub[, 1] <- rep(n.list, each = 20)


for(i in 1:4){
  if(i <4){dat <- read.csv(paste("simResults/highD_hub_unif_cor_", n.list[i], ".csv", sep = ""))[, 2]
  unifRecHub[((i-1) * 20 + 1):(i * 20), 2]  <- dat
  dat_timing <- read.csv(paste("simResults/highD_hubunif_timing_", n.list[i], ".csv", sep = ""))[, 2]
  unifRecHub[((i-1) * 20 + 1):(i * 20), 3]  <- dat_timing}
}

for(i in 1:20){
  dat10 <- read.csv(paste("simResults/highD_hub_unif_cor_1000_", i, ".csv", sep = ""))[, 2]
  unifRecHub[(i + 60), 2]  <- dat10
  dat10_timing <- read.csv(paste("simResults/highD_hub_unif_timing_1000_", i, ".csv", sep = ""))[, 2]
  unifRecHub[(i + 60), 3]  <- dat10_timing
  
  dat15 <- read.csv(paste("simResults/highD_hub_unif_cor_1500_", i, ".csv", sep = ""))[, 2]
  unifRecHub[i + 80, 2]  <- dat15
  dat15_timing <- read.csv(paste("simResults/highD_hub_unif_timing_1500_", i, ".csv", sep = ""))[, 2]
  unifRecHub[i + 80, 3]  <- dat15_timing
  
  dat2000 <- read.csv(paste("simResults/highD_hub_unif_cor_2000_", i, ".csv", sep = ""))[, 2]
  unifRecHub[(i+100), 2]  <- dat2000
  dat2000_timing <- read.csv(paste("simResults/highD_hub_unif_timing_2000_", i, ".csv", sep = ""))[, 2]
  unifRecHub[(i+100), 3]  <- dat2000_timing
}

postscript("highDcons_update.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 8, height = 5)
par(mfrow = c(2,2))
par(mar = c(3, 4, 2, 1))
boxplot(unifRec[,2] ~ unifRec[,1], ylim = c(0, 1), ylab = "Kendalls Tau")
mtext("Random Graphs", side = 3)
mtext("p", side = 1, line = 2)
boxplot(unifRec[,3] / 60 ~ unifRec[,1], log = "y", yaxt = "n", ylab = "minutes", xlab = "")
mtext("Random Graphs", side = 3)
mtext("p", side = 1, line = 2)
axis(2, at = c(1, 5, 10, 50, 100, 500, 1000), las = 2, cex.axis = .8)

boxplot(unifRecHub[,2] ~ unifRecHub[,1], ylim = c(0, 1), ylab = "Kendalls Tau", xlab = "")
mtext("Hub Graphs", side = 3)
mtext("p", side = 1, line = 2)
boxplot(unifRecHub[,3] / 60 ~ unifRecHub[,1], log = "y", yaxt = "n", ylab = "minutes", xlab = "")
mtext("Hub Graphs", side = 3)
mtext("p", side = 1, line = 2)
axis(2, at = c(1, 5, 10, 50, 100, 500, 1000), las = 2, cex.axis = .8)
dev.off()
system("cp highDcons_update.eps ~/Dropbox/highDimDag/paper/v2/figures")




##############################################
# High Dimensional results with other errors #
##############################################


gammaRec <- matrix(0, ncol = 2, nrow = 6* 20)
n.list <- c(100, 200, 500, 1000, 1500, 2000)
gammaRec[, 1] <- rep(n.list, each = 20)


for(i in 1:4){
  if(i < 4){dat <- read.csv(paste("simResults/highD_gamma_cor_", n.list[i], ".csv", sep = ""))[, 2]
  gammaRec[((i-1) * 20 + 1):(i * 20), 2]  <- dat}

  dat15 <- read.csv(paste("simResults/highD_gamma_cor_1500_", i, ".csv", sep = ""))[, 2]
  gammaRec[((i-1) * 5 + 81):(i * 5 + 80), 2]  <- dat15
}

for(i in 1:10){
  dat2000 <- read.csv(paste("simResults/highD_gamma_cor_2000_", i, ".csv", sep = ""))[, 2]
  gammaRec[((i-1)*2+ 101):(i * 2  + 100), 2]  <- dat2000
}


for(i in 1:20){
  dat10 <- read.csv(paste("simResults/highD_gamma_cor_1000_", i, ".csv", sep = ""))[, 2]
  gammaRec[(i + 60), 2]  <- dat10
}




gaussRec <- matrix(0, ncol = 2, nrow = 6* 20)
n.list <- c(100, 200, 500, 1000, 1500, 2000)
gaussRec[, 1] <- rep(n.list, each = 20)


for(i in 1:4){
  if(i <4){dat <- read.csv(paste("simResults/highD_gauss_cor_", n.list[i], ".csv", sep = ""))[, 2]
  gaussRec[((i-1) * 20 + 1):(i * 20), 2]  <- dat}
  dat15 <- read.csv(paste("simResults/highD_gauss_cor_1500_", i, ".csv", sep = ""))[, 2]
  gaussRec[((i-1) * 5 + 81):(i * 5 + 80), 2]  <- dat15
}

for(i in 1:20){
  dat10 <- read.csv(paste("simResults/highD_gamma_cor_1000_", i, ".csv", sep = ""))[, 2]
  gaussRec[(i + 60), 2]  <- dat10
}

for(i in 1:20){
  dat2000 <- read.csv(paste("highD_gauss_cor_2000_", i, ".csv", sep = ""))[, 2]
  gaussRec[i+ 100, 2]  <- dat2000
}




postscript("otherErrors.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 6, height = 3)
par(mfrow = c(1,2))
boxplot(gammaRec[,2]~ gammaRec[,1], ylim = c(-.5, 1), main = "Gamma Errors", ylab = "Kendalls Tau", xlab = "p")
abline(h = 0, col = "red")
boxplot(gaussRec[,2]~ gaussRec[,1], ylim = c(-.5, 1), main = "Gaussian Errors" , ylab = "Kendalls Tau", xlab = "p")
abline(h = 0, col = "red")
dev.off()
system("cp otherErrors.eps ~/Dropbox/highDimDag/paper/v2/figures")

##########################################
# High DimensionalDimensional Pre-select #
#########################################
unifRec <- matrix(0, ncol = 2, nrow = 6 * 20)
n.list <- c(100, 200, 500, 1000, 1500, 2000)
unifRec[, 1] <- rep(n.list, each = 20)



for(i in 1:4){
  if(i <4){dat <- read.csv(paste("simResults/highD_unif_cor_", n.list[i], ".csv", sep = ""))[, 2]
  unifRec[((i-1) * 20 + 1):(i * 20), 2]  <- dat}
  dat10 <- read.csv(paste("simResults/highD_unif_cor_1000_", i, ".csv", sep = ""))[, 2]
  unifRec[((i-1) * 5 + 61):(i * 5 + 60), 2]  <- dat10
  dat15 <- read.csv(paste("simResults/highD_unif_cor_1500_", i, ".csv", sep = ""))[, 2]
  unifRec[((i-1) * 5 + 81):(i * 5 + 80), 2]  <- dat15
}

for(i in 1:4){
  dat2000 <- read.csv(paste("simResults/highD_unif_cor_2000_", i, ".csv", sep = ""))[, 2]
  unifRec[((i-1)*5+ 101):(i * 5  + 100), 2]  <- dat2000
}



preselect.rec <- matrix(0, nrow = 100, ncol = 2)
preselect.rec[, 1] <- rep(c(100, 200, 500, 1000, 1500), each = 20)

preselect.rec[1:20, 2] <- read.csv("simResults/pwl_cor_100.csv")[, 2]
preselect.rec[21:40, 2] <- read.csv("simResults/pwl_cor_200.csv")[, 2]
preselect.rec[41:60, 2] <- read.csv("simResults/pwl_cor_500.csv")[, 2]

for(i in 1:20){
  preselect.rec[60+i, 2] <- read.csv(paste("simResults/pwl_cor_1000_",i, ".csv", sep = ""))[, 2]
  preselect.rec[80+i, 2] <- read.csv(paste("simResults//pwl_cor_1500_",i, ".csv", sep = ""))[, 2]
}

preselect.me.rec <- matrix(0, nrow = 100, ncol = 2)
preselect.me.rec[, 1] <- rep(c(100, 200, 500, 1000, 1500), each = 20)
preselect.timing.rec <- cbind(preselect.me.rec, rep(0, 100))

preselect.me.rec[1:20, 2] <- read.csv("simResults/preSelect_unif_cor_100.csv")[, 2]
preselect.me.rec[21:40, 2] <- read.csv("simResults/preSelect_unif_cor_200.csv")[, 2]
preselect.me.rec[41:60, 2] <- read.csv("simResults/preSelect_unif_cor_500.csv")[, 2]

for(i in 1:20){
  preselect.me.rec[60+i, 2] <- read.csv(paste("simResults/preSelect_unif_cor_1000_",i, ".csv", sep = ""))[, 2]
  preselect.me.rec[80+i, 2] <- read.csv(paste("simResults/preSelect_unif_cor_1500_",i, ".csv", sep = ""))[, 2]
}


### Timing ###
preselect.timing.rec[1:20, 2] <- read.csv("simResults/preSelect_unif_timing_100.csv")[, 2]
preselect.timing.rec[21:40, 2] <- read.csv("simResults/preSelect_unif_timing_200.csv")[, 2]
preselect.timing.rec[41:60, 2] <- read.csv("simResults/preSelect_unif_timing_500.csv")[, 2]

for(i in 1:20){
  preselect.timing.rec[60+i, 2] <- read.csv(paste("simResults/preSelect_unif_timing_1000_",i, ".csv", sep = ""))[, 2]
  preselect.timing.rec[80+i, 2] <- read.csv(paste("simResults/preSelect_unif_timing_1500_",i, ".csv", sep = ""))[, 2]
}

for(i in 1:20){
  preselect.timing.rec[i, 3] <- read.csv(paste("simResults/timingunif_100_",i, ".csv", sep = ""))[, 2]
  preselect.timing.rec[20+i, 3] <- read.csv(paste("simResults/timingunif_200_",i, ".csv", sep = ""))[, 2]
  preselect.timing.rec[40+i, 3] <- read.csv(paste("simResults/timingunif_500_",i, ".csv", sep = ""))[, 2]
  preselect.timing.rec[60+i, 3] <- read.csv(paste("simResults/timingunif_1000_",i, ".csv", sep = ""))[, 2]
  preselect.timing.rec[80+i, 3] <- read.csv(paste("simResults/timing_unif_1500_",i, ".csv", sep = ""))[, 2]
}

holder.timing <- data.frame(matrix(0, ncol = 3, nrow = 100 * 2))
holder.timing[1:100, ] <- cbind(rep("HDL+P", 100),preselect.timing.rec[,1],  rowSums(preselect.timing.rec[,2:3]))
holder.timing[101:200, ] <- cbind(rep("HDL", 100),unifRec[1:100,1],  unifRec[1:100,3])
bp <- boxplot(as.numeric(holder.timing[,3]) ~ holder.timing[, 1] + holder.timing[,2], log = "y")





holder <- data.frame(matrix(0, ncol = 3, nrow = 100 * 3))
holder[1:100, ] <- cbind(rep("PW+P", 100), preselect.rec)
holder[101:200, ] <- cbind(rep("HDL+P", 100), preselect.me.rec)
holder[201:300, ] <- cbind(rep("HDL", 100), unifRec[1:100,])
holder[,1] <- factor(as.character(holder[,1]), levels = c("HDL", "HDL+P", "PW+P"))
holder[,2] <- factor(as.character(holder[,2]), levels = c(100, 200, 500, 1000, 1500))


postscript("preSelectRandom.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 10, height = 4)
par(mar = c(3, 4, 2, 1))
bp <- boxplot(as.numeric(holder[, 3])~holder[, 1] + holder[, 2], ylim = c(-.5, 1), col = c("gray50", "gray75", "white"), xaxt = "n",
              at = rep(c(1:5) * 3 - 1, each = 3) + c(-.6, 0, .6), boxwex = .5, ylab = "Kendalls Tau", xlab = "p", main = "Random Graphs")
axis(side = 1, at = c(1:5) * 3 - 1,labels = c(100, 200, 500, 1000, 1500), lwd.ticks = F)
abline(v = c(1:4) * 3 + .55, lty = 3)
abline(h = 0, col = "red")
bp <- boxplot(as.numeric(holder[, 3])~holder[, 1] + holder[, 2], ylim = c(-.5, 1), col = c("gray50", "gray75", "white"), xaxt = "n",
              at = rep(c(1:5) * 3 - 1, each = 3) + c(-.6, 0, .6), boxwex = .5, ylab = "Kendalls Tau", xlab = "p", add = T)
#legend(x = "bottom", legend = c("HDL", "HDL+P", "PW+P"), col = c("gray50", "gray75", "white"), fill = c("gray50", "gray75", "white"),
#       ncol = 3, bg = "white")
mtext(side = 1, "p", line = 2)
dev.off()
system("cp preSelectRandom.eps ~/Dropbox/highDimDag/paper/v2/figures")

###########################################
# High DimensionalDimensional Hub results #
###########################################

unifRec <- matrix(0, ncol = 2, nrow = 6 * 20)
n.list <- c(100, 200, 500, 1000, 1500, 2000)
unifRec[, 1] <- rep(n.list, each = 20)


dat1 <- read.csv("simResults/highD_hub_unif_cor_100.csv")[, 2]
unifRec[1:20, 2]  <- dat1
dat2 <- read.csv("simResults/highD_hub_unif_cor_200.csv")[, 2]
unifRec[21:40, 2]  <- dat2
dat3 <- read.csv("simResults/highD_hub_unif_cor_500.csv")[, 2]
unifRec[41:60, 2]  <- dat3


for(i in 1:20){
  dat10 <- read.csv(paste("simResults/highD_hub_unif_cor_1000_", i, ".csv", sep = ""))[, 2]
  unifRec[i + 60, 2]  <- dat10
  dat15 <- read.csv(paste("simResults/highD_hub_unif_cor_1500_", i, ".csv", sep = ""))[, 2]
  unifRec[i + 80, 2]  <- dat15
  dat2000 <- read.csv(paste("simResults/highD_hub_unif_cor_2000_", i, ".csv", sep = ""))[, 2]
  unifRec[i + 100, 2]  <- dat2000
}



preselect.rec <- matrix(0, nrow = 100, ncol = 2)
preselect.rec[, 1] <- rep(c(100, 200, 500, 1000, 1500), each = 20)

preselect.rec[1:20, 2] <- read.csv("simResults/pwl_cor_hub_100.csv")[, 2]
preselect.rec[21:40, 2] <- read.csv("simResults/pwl_cor_hub_200.csv")[, 2]
preselect.rec[41:60, 2] <- read.csv("simResults/pwl_cor_hub_500.csv")[, 2]

for(i in 1:20){
  preselect.rec[60+i, 2] <- read.csv(paste("simResults/pwl_cor_hub_1000_",i, ".csv", sep = ""))[, 2]
  preselect.rec[80+i, 2] <- read.csv(paste("simResults/pwl_cor_hub_1500_",i, ".csv", sep = ""))[, 2]
}

preselect.me.rec <- matrix(0, nrow = 100, ncol = 2)
preselect.me.rec[, 1] <- rep(c(100, 200, 500, 1000, 1500), each = 20)

preselect.me.rec[1:20, 2] <- read.csv("simResults/preSelect_hub_unif_cor_100.csv")[, 2]
preselect.me.rec[21:40, 2] <- read.csv("simResults/preSelect_hub_unif_cor_200.csv")[, 2]
preselect.me.rec[41:60, 2] <- read.csv("simResults/preSelect_hub_unif_cor_500.csv")[, 2]

for(i in 1:20){
  preselect.me.rec[60+i, 2] <- read.csv(paste("simResults/preSelect_hub_unif_cor_1000_",i, ".csv", sep = ""))[, 2]
  preselect.me.rec[80+i, 2] <- read.csv(paste("simResults/preSelect_hub_unif_cor_1500_",i, ".csv", sep = ""))[, 2]
}



holder <- data.frame(matrix(0, ncol = 3, nrow = 100 * 3))
holder[1:100, ] <- cbind(rep("PW+P", 100), preselect.rec)
holder[101:200, ] <- cbind(rep("HDL+P", 100), preselect.me.rec)
holder[201:300, ] <- cbind(rep("HDL", 100), unifRec[1:100,])
holder[,1] <- factor(as.character(holder[,1]), levels = c("HDL", "HDL+P", "PW+P"))
holder[,2] <- factor(as.character(holder[,2]), levels = c(100, 200, 500, 1000, 1500))


postscript("preSelectHub.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 10, height = 4)
bp <- boxplot(as.numeric(holder[, 3])~holder[, 1] + holder[, 2], ylim = c(-.5, 1), col = c("gray50", "gray75", "white"), xaxt = "n",
              at = rep(c(1:5) * 3 - 1, each = 3) + c(-.6, 0, .6), boxwex = .5, ylab = "Kendalls Tau", xlab = "p", main = "Hub Graphs")
axis(side = 1, at = c(1:5) * 3 - 1,labels = c(100, 200, 500, 1000, 1500), lwd.ticks = F)
abline(v = c(1:4) * 3 + .55, lty = 3)
abline(h = 0, col = "red")
bp <- boxplot(as.numeric(holder[, 3])~holder[, 1] + holder[, 2], ylim = c(-.5, 1), col = c("gray50", "gray75", "white"), xaxt = "n",
              at = rep(c(1:5) * 3 - 1, each = 3) + c(-.6, 0, .6), boxwex = .5, ylab = "Kendalls Tau", xlab = "p", add = T)
#legend(x = "bottom", legend = c("HDL", "HDL+P", "PW+P"), col = c("gray50", "gray75", "white"), fill = c("gray50", "gray75", "white"),
#       ncol = 3)
dev.off()
system("cp preSelectHub.eps ~/Dropbox/highDimDag/paper/v2/figures")