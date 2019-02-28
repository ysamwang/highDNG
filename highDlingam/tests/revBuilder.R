# install.packages("quantmod")
library("quantmod")

spy <- read.csv("returnsSPY.csv", header = T)
spyTick <- names(spy)

skipInd <- c(19, 41, 81, 135, 146, 150, 153, 176, 214, 217, 224, 230, 232, 254, 273, 281, 293, 294, 309, 329,
             332,336, 357, 420, 435)
spyTick[skipInd]


annualRev <- rep(0, length(spyTick))
for(i in 1:length(spyTick)){
  print(i)
  if(!(i %in% skipInd)){
    report <- getFinancials(spyTick[i], auto.assign = F)$IS$A  
    years <- sapply(strsplit(colnames(report), "-"), function(x){x[1]})
    annualRev[i] <- report[3, which(years == "2015")]
  } 
}

fill <- c(12688000, 11661000, 3663851, 0,2725867, 101751800, 20855000, 10338000, 4585000, 2219762, 3007976,
  37179000, 70074000, 40536000, 20261000, 2722564, 9429000, 3183000,	5915700, 2104823, 4616000, 5015000, 1638474,
  103444000, 9649000)	/ 1000

annualRev[skipInd] <- fill
names(annualRev) <- spyTick

na.ind <- which(is.na(annualRev))
na.fills <- c(93514, 10346, 14951, 68024, 2819, 25038, 7031, 3153, 89716, 4502, 4995, 1421, 0, 346, 10666, 21494, 90033, 2190)

annualRev[na.ind] <- na.fills
write.csv(annualRev, "annualRev.csv")
