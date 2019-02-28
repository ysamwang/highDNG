

png("./testOutput/highDcons.png", width = 600, height = 300)
cor.rec <- as.matrix(read.csv("./testOutput/highD_cor.csv"))
timing.rec <- as.matrix(read.csv("./testOutput/highD_timing.csv"))

val.list <- c(100, 200, 500, 1000, 1500)
sim.size <- 10
par(mfrow = c(2,2), mar = c(.5, 4, 1, .5), oma = c(3, 0, 1, 0))
boxplot(c(cor.rec[,2]) ~ rep(val.list, each = sim.size),
        ylim = c(0, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
mtext(side = 2, "Random DAG", line = 2)
mtext(expression(paste("Kendall's " ,tau)), side = 3, line = 0)

boxplot(c(timing.rec[, 2] / 60) ~ rep(val.list, each = sim.size), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (min)", log = "y", las = 1)
mtext("Time (min)", side = 3, line = 0)


cor.rec <- as.matrix(read.csv("./testOutput/hub_cor5.csv"))
timing.rec <- as.matrix(read.csv("./testOutput/hub_timing5.csv"))

val.list <- c(100, 200, 500, 1000, 1500)
sim.size <- 10
options(scipen=999)


boxplot(c(cor.rec[,2]) ~ rep(val.list, each = sim.size),
        ylim = c(0, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = c(1:length(val.list)), labels = val.list, las = 2)
mtext(side = 2, "Hub Graphs", line = 2)
# abline(h = 0, col = "red", lty = 2)
boxplot(c(timing.rec[, 2] / 60) ~ rep(val.list, each = sim.size), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (min)", log = "y",ylim = c(.1, 1000), las = 1)
axis(side = 1, at = c(1:length(val.list)), labels = val.list, las = 2)

dev.off()








# png("./testOutput/comparison_comb.png", width = 600, height= 225)

wid <- 8
hei <- 3
postscript("~/Dropbox/highDimDag/paper/bka/figures/comparison_comb.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)

cor.rec.me <- as.matrix(read.csv("./testOutput/compare_cor50_a.csv"))
cor.rec.dl <- as.matrix(read.csv("./testOutput/dlingam_res50.csv", header = F))

val.list <- c(5, 10, 15, 20)
sim.size <- 500

cor.total <- cbind(cor.rec.dl[, 1],
                   cor.rec.me[ -c(2001:3000), 2:3], cor.rec.dl[, 2:3])

colnames(cor.total) <- c("n","HD1", "HD2", "DL", "PW")
method <- rep(c("A1", "A2", "DL", "PW"), each = length(val.list) * sim.size)

par(mar = c(3, 0, 1, .5), mfrow = c(1,2), oma = c(0, 3.2, 0, 0))

out <- boxplot(c(cor.total[, -1]) ~  method + rep(rep(val.list, each = sim.size), times = 4), at = rep(1:length(val.list), each = 4) + c(-.225, -.075,  .075 ,.225),
        ylim = c(-1, 1), xlab = "", main = "n = 50p", xlim = c(.75, 4.25),
        boxwex = .1, col = c("white", "lightgray", "gray37", "gray20"), cex.axis=1, xaxt = "n", ylab = "", pch = 19, cex = .6)
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
mtext(side = 1, "p", line = 2)
mtext(side = 2, expression(paste("Kendall's ", tau)), line = 2)
abline(h = 0, col = "red", lty = 2)
abline(v = c(1:length(val.list)) + .5, col = "gray10", lty = 3)

cor.rec.me <- as.matrix(read.csv("./testOutput/compare_cor10_a.csv"))
cor.rec.dl <- as.matrix(read.csv("./testOutput/dlingam_res10.csv", header = F))

val.list <- c(5, 10, 15, 20)
sim.size <- 500

cor.total <- cbind(cor.rec.dl[, 1],
                   cor.rec.me[ -c(2001:3000), 2:3], cor.rec.dl[, 2:3])

colnames(cor.total) <- c("n","HD1", "HD2", "DL", "PW")
method <- rep(c("A1", "A2", "DL", "PW"), each = length(val.list) * sim.size)
out <- boxplot(c(cor.total[, -1]) ~  method + rep(rep(val.list, each = sim.size), times = 4), at = rep(1:length(val.list), each = 4) + c(-.225, -.075,  .075 ,.225),
               ylim = c(-1, 1), xlab = "", boxwex = .1, col = c("white", "lightgray", "gray37", "gray20"), cex.axis=.8, xaxt = "n", yaxt = "n",
               main = "n = 10p", xlim = c(.75, 4.25), pch = 19, cex = .6)
mtext(side = 1, "p", line = 2)
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
abline(h = 0, col = "red", lty = 2)
abline(v = c(1:length(val.list)) + .5, col = "gray10", lty = 3)

dev.off()






val.list <- c(5, 10, 15, 20, 40, 80)
sim.size <- 500
method <- rep(c("HD1", "HD2"), each = sim.size * length(val.list))
timing.rec.me <- as.matrix(read.csv("./testOutput/compare_timing50.csv"))
times.rec <- c(timing.rec.me[, 2:3])

wid <- 6
hei <- 3
postscript("~/Dropbox/highDimDag/paper/bka/figures/timing50.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)

par(mar = c(3, 0, 1, .5), oma = c(0, 3.2, 0, 0))
boxplot(times.rec ~ method + rep(rep(val.list, each = sim.size), times = 2),
        at = rep(c(1:length(val.list)), each = 2) + c(-.1, .1),
        main = "n = 50p", xaxt = "n", boxwex = .1, col = c("white", "lightgray"),
        ylab = "time (sec)", log = "y", xlab = "")
mtext(side = 1, "p", line = 2)
mtext(side = 2, "time (sec)", line = 2)
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
dev.off()



cor.rec <- as.matrix(read.csv("./testOutput/hub_cor5.csv"))
timing.rec <- as.matrix(read.csv("./testOutput/hub_timing5.csv"))

val.list <- c(100, 200, 500, 1000, 1500)
sim.size <- 10

png("./testOutput/highDHubcons.png", width = 500, height = 300)
par(mfrow = c(1,2), oma = c(0, 0, 0, 0), mar = c(4, 4, .5, .5))
boxplot(c(cor.rec[,2]) ~ rep(val.list, each = sim.size),
        ylim = c(0, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = c(1:length(val.list)), labels = val.list, las = 2)
# abline(h = 0, col = "red", lty = 2)
mtext("Kendall's tau", side = 2, line = 2)
boxplot(c(timing.rec[,2]) ~ rep(val.list, each = sim.size), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (sec)", log = "y", las = 1, ylim = c(5, 50000))
axis(side = 1, at = c(1:length(val.list)), labels = val.list, las = 2)
dev.off()







wid <- 9
hei <- 3.5
postscript("~/Dropbox/highDimDag/paper/bka/figures/highDcons_update.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)
# png("~/Dropbox/dissertation/figures/ch3/highDcons_update.png", width = 600, height = 300)
val.list <- c(100, 200, 500, 1000, 1500)
sim.size <- 20
i <- 1
p <- val.list[i]
cor.rec <- cbind(rep(p, sim.size), as.matrix(read.csv(paste("./testOutput/updatedRes/highD_cor_rerun_",p,".csv", sep = "")))[1:sim.size, 2])
timing.rec <- cbind(rep(p, sim.size), as.matrix(read.csv(paste("./testOutput/updatedRes/highD_timing_rerun_",p,".csv", sep = "")))[1:sim.size, 2])

for(i in 2:length(val.list)){
  p <- val.list[i]
  
  p <- val.list[i]
  if(p == 1500){
    sim.size <- 10
  } else {
    sim.size <- 20
  }
  cor.rec <- rbind(cor.rec, 
                   cbind(rep(p, sim.size),
                         as.matrix(read.csv(paste("./testOutput/updatedRes/highD_cor_rerun_",p,".csv", sep = "")))[1:sim.size, 2]))
  timing.rec <- rbind(timing.rec,
                      cbind(rep(p, sim.size),
                            as.matrix(read.csv(paste("./testOutput/updatedRes/highD_timing_rerun_",p,".csv", sep = "")))[1:sim.size, 2]))
  
}

par(mfrow = c(2,2), mar = c(.5, 4, 1, .5), oma = c(3, 0, 1, 0))
boxplot(c(cor.rec[which(timing.rec[,2] > 0),2]) ~ cor.rec[which(timing.rec[,2] > 0), 1],
        ylim = c(-.5, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
mtext(side = 2, "Random DAG", line = 2)
mtext(expression(paste("Kendall's " ,tau)), side = 3, line = 0)

boxplot(c(timing.rec[, 2] / 60) ~ timing.rec[, 1], at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (min)", log = "y", las = 1)
mtext("Time (min)", side = 3, line = 0)






val.list <- c(100, 200, 500, rep(rep(1000, 20)), rep(1500, 10))

cor.rec <- timing.rec <- matrix(0, 0, 2)
for(i in 1:length(val.list)){
  cor.read <- read.csv(paste("./testOutput/updatedRes/hub_cor_easy34_edge7_",val.list[i], "_", i,".csv", sep = ""))
  cor.rec <- rbind(cor.rec, cbind(rep(val.list[i], dim(cor.read)[1]), cor.read[, 2] ))
  
  timing.read <- read.csv(paste("./testOutput/updatedRes/hub_timing_easy34_edge7_",val.list[i], "_", i,".csv", sep = ""))
  timing.rec <- rbind(timing.rec, cbind(rep(val.list[i], dim(timing.read)[1]), timing.read[, 2] ))
}


boxplot(c(cor.rec[which(timing.rec[,2] > 0),2]) ~ cor.rec[which(timing.rec[,2] > 0), 1],
        ylim = c(0, 1), at = 1:length(unique(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = 1:length(unique(val.list)), labels = unique(val.list), las = 2)
mtext(side = 2, "Hub Graphs", line = 2)

options(scipen=999)


# abline(h = 0, col = "red", lty = 2)
boxplot(c(timing.rec[, 2] / 60) ~ timing.rec[, 1], at = 1:length(unique(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (min)", log = "y",ylim = c(.1, 1000), las = 1)
axis(side = 1, at = 1:length(unique(val.list)), labels = unique(val.list), las = 2)
dev.off()





wid <- 8
hei <- 3
postscript("~/Dropbox/highDimDag/paper/bka/figures/highDcons_update.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)
# png("~/Dropbox/presentations/final_exam/figures/highDcons_update.png", width = 400, height = 200)
val.list <- c(100, 200, 500, 1000, 1500)
sim.size <- 20
i <- 1
p <- val.list[i]
cor.rec <- cbind(rep(p, sim.size), as.matrix(read.csv(paste("./testOutput/updatedRes/highD_cor_rerun_",p,".csv", sep = "")))[1:sim.size, 2])
timing.rec <- cbind(rep(p, sim.size), as.matrix(read.csv(paste("./testOutput/updatedRes/highD_timing_rerun_",p,".csv", sep = "")))[1:sim.size, 2])

for(i in 2:length(val.list)){
  p <- val.list[i]
  
  p <- val.list[i]
  if(p == 1500){
    sim.size <- 10
  } else {
    sim.size <- 20
  }
  cor.rec <- rbind(cor.rec, 
                   cbind(rep(p, sim.size),
                         as.matrix(read.csv(paste("./testOutput/updatedRes/highD_cor_rerun_",p,".csv", sep = "")))[1:sim.size, 2]))
  timing.rec <- rbind(timing.rec,
                      cbind(rep(p, sim.size),
                            as.matrix(read.csv(paste("./testOutput/updatedRes/highD_timing_rerun_",p,".csv", sep = "")))[1:sim.size, 2]))
  
}

par(mfrow = c(1,2), mar = c(.5, 4, 1, .5), oma = c(3, 0, 1, 0))
boxplot(c(cor.rec[which(timing.rec[,2] > 0),2]) ~ cor.rec[which(timing.rec[,2] > 0), 1],
        ylim = c(0, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
mtext(side = 2, "Random DAG", line = 2)
mtext(expression(paste("Kendall's " ,tau)), side = 3, line = 0)
axis(side = 1, at = 1:length(unique(val.list)), labels = unique(val.list), las = 2)


boxplot(c(timing.rec[, 2] / 60) ~ timing.rec[, 1], at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (min)", log = "y", las = 1)
mtext("Time (min)", side = 3, line = 0)
axis(side = 1, at = 1:length(unique(val.list)), labels = unique(val.list), las = 2)


dev.off()


postscript("~/Dropbox/presentations/final_exam/figures/hubcons_update.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)
# png("~/Dropbox/presentations/final_exam/figures/hubcons_update.png", width = 400, height = 200)
val.list <- c(100, 200, 500, rep(rep(1000, 20)), rep(1500, 10))
par(mfrow = c(1,2), mar = c(.5, 4, 1, .5), oma = c(3, 0, 1, 0))
cor.rec <- timing.rec <- matrix(0, 0, 2)
for(i in 1:length(val.list)){
  cor.read <- read.csv(paste("./testOutput/updatedRes/hub_cor_easy34_edge7_",val.list[i], "_", i,".csv", sep = ""))
  cor.rec <- rbind(cor.rec, cbind(rep(val.list[i], dim(cor.read)[1]), cor.read[, 2] ))
  
  timing.read <- read.csv(paste("./testOutput/updatedRes/hub_timing_easy34_edge7_",val.list[i], "_", i,".csv", sep = ""))
  timing.rec <- rbind(timing.rec, cbind(rep(val.list[i], dim(timing.read)[1]), timing.read[, 2] ))
}


boxplot(c(cor.rec[which(timing.rec[,2] > 0),2]) ~ cor.rec[which(timing.rec[,2] > 0), 1],
        ylim = c(0, 1), at = 1:length(unique(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = 1:length(unique(val.list)), labels = unique(val.list), las = 2)
mtext(side = 3, "(.5, 1)", line = 2)
mtext(side = 2, "Hub Graphs", line = 2)

options(scipen=999)


# abline(h = 0, col = "red", lty = 2)
boxplot(c(timing.rec[, 2] / 60) ~ timing.rec[, 1], at = 1:length(unique(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (min)", log = "y",ylim = c(.1, 1000), las = 1)
axis(side = 1, at = 1:length(unique(val.list)), labels = unique(val.list), las = 2)
dev.off()

png("~/Dropbox/presentations/final_exam/figures/sim_var.png", width = 600, height = 400)
p <- 1500
out <- BCD::rDAG(p, 10, lowScale = .8, highScale = 1, lowEdge = .5, highEdge = 1)
sig <- solve(diag(rep(1, p)) - out$B, diag(out$scale.param^2)) %*% t( solve(diag(rep(1, p)) - out$B))

par(mfrow = c(1,2))
plot(sqrt(diag(sig)), main = "SD vs Ordering", xlab = "ordering", ylab = "sd(y)", pch = 19, ylim = c(.8, 2.5), cex = .8)
mtext("Random DAG")

out <- rDAG_hub(p, 10, lowScale = .8, highScale = 1, lowEdge = .65, highEdge = 1)
sig <- solve(diag(rep(1, p)) - out$B, diag(out$scale.param^2)) %*% t( solve(diag(rep(1, p)) - out$B))
plot(sqrt(diag(sig)), main = "SD vs Ordering", xlab = "ordering", ylab = "sd(y)", pch = 19, ylim = c(.8, 2.5), cex = .8)
mtext("Hubs")
dev.off()


cor(order(diag(sig)), 1:p)
