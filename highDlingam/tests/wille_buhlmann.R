wb <- read.csv("highDlingam/data/wille_buhlman.csv", header = T)

# Raw data but remove first 5 columns
dat <- (t(wb[, -c(1:5)]))
# Scaled and centered data
scaled.dat <- scale(dat)

library(doParallel)
library(highDLingam)

ncores <- 6
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)
time1 <- system.time(output1 <- findGraphMulti(scaled.dat, maxInDegree = 4,
                                                    cutOffScaling = .5, degree = 4,
                                                    verbose = F))

### Get errors by regressing child onto parents ### 
err <- matrix(0, nrow = dim(scaled.dat)[1], ncol = dim(scaled.dat)[2])
for(v in 1:dim(scaled.dat)[2]){
    if(length(output1$parents[[which(output1$topOrder == v)]]) != 0){
      err[, v] <- lm(scaled.dat[, v] ~ scaled.dat[, output1$parents[[which(output1$topOrder == v)]], drop = F])$res
    } else {
      err[, v] <- scaled.dat[, v]
    }
}




### Visualize correlation between observed variables and correlation between estimated errors ###
par(mfrow = c(1,2))
corrplot::corrplot(cor(scaled.dat))
corrplot::corrplot(cor(err))

write.csv(err, "highDlingam/data/wille_lingam_errors.csv")
err
