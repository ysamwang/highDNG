wb <- read.csv("./data/wille_buhlman.csv", header = T)
dim(wb)
head(wb)

dat <- (t(wb[, -c(1:5)]))


scaled.dat <- scale(dat[, which(wb$Pathwayname =="Non-Mevalonatepathway")])

library(doParallel)
library(highDLingam)

ncores <- 6
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)
time1 <- system.time(output1 <- findGraphSingleFast(scaled.dat, maxInDegree = 4,
                                                    cutOffScaling = .5, degree = 4,
                                                    verbose = F))

wb[which(wb$Pathwayname =="Non-Mevalonatepathway"), ]$Genename[output1$topOrder]



scaled.dat <- scale(dat[, which(wb$Pathwayname =="Mevalonatepathway")])

library(doParallel)
library(highDLingam)

ncores <- 6
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)
time1 <- system.time(output1 <- findGraphSingleFast(scaled.dat, maxInDegree = 4,
                                                    cutOffScaling = .5, degree = 4,
                                                    verbose = F))

wb[which(wb$Pathwayname =="Mevalonatepathway"), ]$Genename[output1$topOrder]


# library(BCD, mvtnorm)
sachs_data <- read.csv("./data/sachs_data1.csv") 
sachs_data <- scale(sachs_data)

no.out <- sachs_data[-unique(which(abs(sachs_data) > 5, arr.ind = T)[, 1]), ]

par(mfrow = c(3,3))
for(i in 1:11){
  hist(no.out[,i])
}

time1 <- system.time(output1 <- findGraphSingleFast(no.out, maxInDegree = 3,
                                                    cutOffScaling = .5, degree = 3,
                                                    verbose = F))
colnames(no.out)[output1$topOrder]

head(swiss)

par(mfrow = c(3,3))
for(i in 1:11){
  hist(swiss[,i])
}



output1 <- findGraphMulti(scale(swiss), maxInDegree = 3,
                               cutOffScaling = .5, degree = 4,
                               verbose = F)
colnames(swiss)[output1$topOrder]
output1$p


padhan <- read.csv("./data/padhan.csv") 
padhan <- scale(padhan)

dim(padhan)
dat <- na.omit(padhan[,1:27])
