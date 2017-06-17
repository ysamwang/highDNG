cor.rec <- as.matrix(read.csv("./testOutput/highD_cor.csv"))
timing.rec <- as.matrix(read.csv("./testOutput/highD_timing.csv"))

val.list <- c(100, 250, 500, 1000, 1500)
sim.size <- 10

png("./testOutput/highDcons.png", width = 600, height = 350)
par(mfrow = c(1,2), oma = c(0, 1, 0, 0))
boxplot(c(cor.rec[,2]) ~ rep(val.list, each = sim.size),
        ylim = c(-1, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
abline(h = 0, col = "red", lty = 2)
mtext("Kendall's tau", side = 2, line = 2)
boxplot(c(timing.rec[,2]) ~ rep(val.list, each = sim.size), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n", ylab = "time (sec)", log = "y", las = 1)
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
dev.off()


cor.rec <- as.matrix(read.csv("./testOutput/hub_cor.csv"))
timing.rec <- as.matrix(read.csv("./testOutput/hub_timing.csv"))

val.list <- c(100, 250, 500, 1000, 1500)
sim.size <- 10

png("./testOutput/hubcons.png", width = 800, height = 500)
par(mfrow = c(1,2), oma = c(0, 1, 0, 0))
boxplot(c(cor.rec[,2]) ~ rep(val.list, each = sim.size),
        ylim = c(-1, 1), at = c(1:length(val.list)) ,
        xlab = "p", boxwex = .2, xaxt = "n")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
abline(h = 0, col = "red", lty = 2)
mtext("Kendall's tau", side = 2, line = 2)
boxplot(log(c(timing.rec[,2]), 10) ~ rep(val.list, each = sim.size), at = c(1:length(val.list)) , ylim = c(0, max(log(timing.rec, 10))*1.2),
        xlab = "p", boxwex = .2, xaxt = "n", ylab = expression(paste('log'[10], "(sec)")))
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
dev.off()

cor.rec.me <- as.matrix(read.csv("./testOutput/compare_cor50_a.csv"))
cor.rec.dl <- as.matrix(read.csv("./testOutput/dlingam_res50.csv", header = F))

val.list <- c(5, 10, 15, 20)
sim.size <- 500

cor.total <- cbind(cor.rec.dl[, 1],
                   cor.rec.me[ -c(2001:3000), 2:3], cor.rec.dl[, 2:3])

colnames(cor.total) <- c("n","HD1", "HD2", "DL", "PW")
method <- rep(c("A1", "A2", "DL", "PW"), each = length(val.list) * sim.size)





png("./testOutput/comparison50.png", width = 600, height= 225)
par(mar = c(4, 4, 1, 1))
out <- boxplot(c(cor.total[, -1]) ~  method + rep(rep(val.list, each = sim.size), times = 4), at = rep(1:length(val.list), each = 4) + c(-.225, -.075,  .075 ,.225),
        ylim = c(-1, 1), xlab = "p; n = 50p", boxwex = .1, col = c("white", "lightgray", "gray37", "gray20"), cex.axis=.8, xaxt = "n", ylab = "Kendall's Tau")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
abline(h = 0, col = "red", lty = 2)
dev.off()



cor.rec.me <- as.matrix(read.csv("./testOutput/compare_cor10_a.csv"))
cor.rec.dl <- as.matrix(read.csv("./testOutput/dlingam_res10.csv", header = F))

val.list <- c(5, 10, 15, 20)
sim.size <- 500

cor.total <- cbind(cor.rec.dl[, 1],
                   cor.rec.me[ -c(2001:3000), 2:3], cor.rec.dl[, 2:3])

colnames(cor.total) <- c("n","HD1", "HD2", "DL", "PW")
method <- rep(c("A1", "A2", "DL", "PW"), each = length(val.list) * sim.size)





png("./testOutput/comparison10.png", width = 600, height= 225)
par(mar = c(4, 4, 1, 1))
out <- boxplot(c(cor.total[, -1]) ~  method + rep(rep(val.list, each = sim.size), times = 4), at = rep(1:length(val.list), each = 4) + c(-.225, -.075,  .075 ,.225),
               ylim = c(-1, 1), xlab = "p; n = 10p", boxwex = .1, col = c("white", "lightgray", "gray37", "gray20"), cex.axis=.8, xaxt = "n", ylab = "Kendall's Tau")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
abline(h = 0, col = "red", lty = 2)
dev.off()


val.list <- c(5, 10, 15, 20, 40, 80)
method <- rep(c("HD1", "HD2"), each = sim.size * length(val.list))
timing.rec.me <- as.matrix(read.csv("./testOutput/compare_timing50.csv"))
times.rec <- c(timing.rec.me[, 2:3])
png("./testOutput/timing50.png", width = 600, height= 225)
boxplot(times.rec ~ method + rep(rep(val.list, each = sim.size), times = 2),
        at = rep(c(1:length(val.list)), each = 2) + c(-.1, .1),
        xlab = "p; n = 50p", xaxt = "n", boxwex = .1, col = c("white", "lightgray"),
        ylab = "time (sec)", log = "y")
axis(side = 1, at = c(1:length(val.list)), labels = val.list)
dev.off()
