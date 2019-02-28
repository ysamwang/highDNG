ret.final <- as.matrix(read.table("returnsSPY.csv", sep = ",", header = T))

sapply(sapply(as.character(2007:2017), grep, rownames(ret.final)), length)



rank.sector <- matrix(0, nrow = 11, ncol = 5)

# wid <- 12
# hei <- 8
# postscript("~/Dropbox/presentations/final_exam/figures/year_by_year.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)
# png("~/Dropbox/presentations/final_exam/figures/year_by_year.png", width = 500, height = 600)
par(mar = c(.5, 12, 1, 2), mfrow = c(5, 1))
outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_07_09.csv")
industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
rank.sector[, 1] <- rank(industry.ranks[, 2])

outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T, xaxt = "n")
mtext("2007-2009")

outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_10_11.csv")
industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
rank.sector[, 2] <- rank(industry.ranks[, 2])
outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T, xaxt = "n")
industry.ranks[order(industry.ranks[, 2]),]
mtext("2010-2011")

outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_12_13.csv")
industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
rank.sector[, 3] <- rank(industry.ranks[, 2])
outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T, xaxt = "n")
industry.ranks[order(industry.ranks[, 2]),]
mtext("2012-2013")

outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_14_15.csv")
industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
rank.sector[, 4] <- rank(industry.ranks[, 2])
outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T, xaxt = "n")
industry.ranks[order(industry.ranks[, 2]),]
mtext("2014-2015")

outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_comb.csv")
industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
rank.sector[, 5] <- rank(industry.ranks[, 2])
outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T, xaxt = "n")
industry.ranks[order(industry.ranks[, 2]),]
mtext("2016-2017")

dev.off()

sum(outS$Sector[1:441] == outS$Sector[2:442])


wid <- 8
hei <- 3
postscript("~/Dropbox/highDimDag/paper/bka/figures/time_ordering.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)
# png("~/Dropbox/presentations/final_exam/figures/time_ordering.png", width = 500, height = 300)
par(mar = c(3, 6, 2, 7))
v2 <- rainbow(11)
sec.name <- c("D", "S", "E", "F", "H", "N", "I", "M", "R", "T", "U")
plot(rank.sector[1, ], xlab = "", ylab = "", ylim = c(11.5, .5), main = "Sector Causality", type = "b", col = 1, lwd = 2, pch = NA,
     xaxt = "n")
for(i in 1:dim(rank.sector)[1]){
  lines(rank.sector[i, ], col = v2[i], lwd = 2, type = "b", pch = sec.name[i])
}
axis(at = c(1:5), labels = c("07-09", "10-11", "12-13", "14-15", "16-17"), side = 1)
axis(at = c(1,11), labels = c("Cause", "Effect"), side = 2, line = 2, lwd= 0)
mtext(side = 2, "Order of Median Rank", line = 4)
mtext(side = 1, "Year", line = 2)
legend("topright", pch = sec.name, col = v2, ncol = 1, legend = c("Cons Disc", "Cons Spl", "Energy", "Financials", "Healthcare",
       "Industrials", "Info Tech", "Materials", "Real Estate", "Telecomm", "Utilities"), xpd = NA, inset = c(-.2, 0), cex = .8)
dev.off()


outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_10_17.csv")
industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T)
industry.ranks[order(industry.ranks[, 2]),]




outS <- read.csv("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/total_ordering_highD_comb.csv")
outMod <- readRDS("~/Dropbox/highDimDag/highDCode/highDNG/highDlingam/tests/testOutput/updatedRes/highDSPY.RDS")


industry.ranks <- aggregate(outS$Rank ~ outS$Sector, FUN = median)
outS$Sector <- ordered(outS$Sector, levels =as.character(industry.ranks[order(industry.ranks[, 2], decreasing = T), 1]))

wid <- 8
hei <- 3
postscript("~/Dropbox/highDimDag/paper/bka/figures/spy500_order.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = wid, height = hei)
# png("~/Dropbox/presentations/final_exam/figures/spy500_order.png", width = 500, height = 300)
par(mar = c(3, 12, 3, 2))
boxplot(outS$Rank ~ outS$Sector, horizontal = T, las = T,
        main = "Ordering of SP 500 Constituents")
mtext(side = 1, "Causal Ordering", line = 2)
mtext("Jan 2016 - Sep 2017")
dev.off()

sum(outS$Sector[1:441] == outS$Sector[2:442])
test <- sort(outS$Sector)

sim.size <- 10000
rec <- rep(0, sim.size)
for(i in 1:sim.size){
  test <- sample(test)
  rec[i] <- sum(test[1:441] == test[2:442])
}

png("~/Dropbox/presentations/final_exam/figures/adj_null.eps", width = 600, height = 400)
hist(rec, main = "Null Distribution", freq = F, xlim = c(0, 200))
abline(v = 160, col = "red")
dev.off()





hist(ifelse(apply(ret.final, MAR = 2, function(x){mean((x - mean(x))^4) /mean((x - mean(x))^2)^2 - 3 }) < 50,apply(ret.final, MAR = 2, function(x){mean((x - mean(x))^4) /mean((x - mean(x))^2)^2 - 3 }),
            -1), xlim = c(0, 50), xlab = "")


hist(apply(ret.final, MAR = 2, function(x){mean((x - mean(x))^3) /sd(x)^3 }), xlim = c(-5, 5))


out <- pcalg::pc(suffStat = list(C = cor(ret.final[2266:2690,]), n = 501), indepTest = pcalg::gaussCItest, alpha = .05,
          labels = colnames(ret.final))
saveRDS(out, "pc_spy05.RDS")

adj_mat <- as(out, "amat")
sum(adj_mat == 1 & t(adj_mat) == 0)

edges <- adj_mat + t(adj_mat)
sum(edges[lower.tri(edges)] != 0)
edgeList <- which(edges != 0 & lower.tri(edges), arr.ind = T)

nat.order <- colnames(ret.final)
nat.order.sector <- outS$Sector[match(nat.order, outS$Ticker)]

sum(nat.order.sector[edgeList[, 1]] == nat.order.sector[edgeList[, 2]])



years <- sapply(as.character(2007:2017), grep, rownames(ret.final))
sum(p.adjust(apply(ret.final[c(years$'2016', years$'2017'), ], MAR = 2, function(x){cor.test(x[-1], x[-length(x)])$p.value}), method = "BH") < .05)
