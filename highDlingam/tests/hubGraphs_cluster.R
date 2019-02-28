library(highDLingam)
library(doParallel)

ind <- 1

args <- (commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text = args[[i]]))
}
print(ind)

val.list <- c(100, 200, 500, 1000, 1500)
varParam <- val.list[ind]

ncores <- 20
print(ncores)

cl <- makeCluster(ncores)
registerDoParallel(cl)



distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}
maxInDegree <- 2
cs <- .8


sim.size <- 20
timing.rec <- cor.rec <- matrix(0, nrow = sim.size, ncol = 1)

for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG_hub(p = varParam, n = 3/4 * varParam, lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1, numHubs = 3)
    Y <- out_dag$Y.unif
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = 2, cutOffScaling = cs, degree = 4,
                                                   verbose = F))
    
    cor.rec[i, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
    timing.rec[i, ] <- time3[3]
    ### update table
    
    write.csv(cor.rec, paste("hub_cor_rerun_", varParam,".csv", sep = ""))
    write.csv(timing.rec,paste("hub_timing_rerun_", varParam,".csv", sep = ""))
  }

