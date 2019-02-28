library(highDLingam)

ind <- 1

arg <- (commandArgs(TRUE))
for(i in 1:length(arg)){
  eval(parse(text = arg[[i]]))
}
print(ind)

val.list <- c(100, 200, 500, 1000, 1500)
varParam <- val.list[ind]

ncores <- detectCores() - 1
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
count <- 1
  for(i in 1:sim.size){
    cat(i)
    cat("\n")
    out_dag <- rDAG(p = varParam, n = varParam * 3/4, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                    lowEdge = .5, highEdge = 1)
    Y <- out_dag$Y
    time3 <- system.time(output3 <- findGraphMulti(Y, maxInDegree = maxInDegree, cutOffScaling = cs, degree = deg,
                                                   verbose = F))
    
    cor.rec[i, ] <- cor(output3$topOrder, 1:varParam, method = "kendall")
    timing.rec[i, ] <- time3[3]
    ### update table
    count <- count + 1
    write.csv(cor.rec,"highD_cor_rerun.csv")
    write.csv(timing.rec,"highD_timing_rerun.csv")
  }
  
}



stopCluster(cl)

