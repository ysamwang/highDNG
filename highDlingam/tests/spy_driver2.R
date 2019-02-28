library(doParallel)
library(highDLingam)

runNum <- 1
args <- commandArgs(TRUE)
eval(parse(text=args[[1]]))
print(runNum)

set.seed(100 + runNum)

ret.final <- as.matrix(read.table("returnsSPY.csv", sep = ",", header = T))

scaleOpt <- F

ind <- sample(1:dim(ret.final)[1], 252)

cs <- 1
ncores <- 20
cl <- makeCluster(ncores)
registerDoParallel(cl)
md <- 2

ret.training <- scale(ret.final[-ind, ] , scale = scaleOpt)
output3 <- highDLingam::findGraphMulti(ret.training, maxInDegree = md, cutOffScaling = cs, degree = 4,verbose = F)
ret.subs <- scale(ret.final[ind, output3$topOrder], scale = scaleOpt)
output4 <- highDLingam::findGraphMulti(ret.subs, maxInDegree = md, cutOffScaling = cs, degree = 4,
                                         verbose = F)
rec <-  cor(output4$topOrder, 1:dim(ret.final)[2], method = "kendall")
write.csv(rec, paste("highDL_spy_md_2_",runNum, ".csv", sep = ""))





