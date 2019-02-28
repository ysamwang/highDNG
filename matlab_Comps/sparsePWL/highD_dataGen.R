library(highDLingam)


ind <- 2

args <- (commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text = args[[i]]))
}
print(ind)

val.list <- c(100, 200, 500, 1000, 1500)
varParam <- val.list[ind]


distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}
maxInDegree <- 2


getParentSet <- function(i, Y){
  p <- dim(Y)[2]
  out <- glmnet::cv.glmnet(x = Y[, -i], y = Y[, i])
  parents <- c(1:p)[-i][which(coef(out, s = "lambda.min")[-1] != 0)]
  return(parents)
}

getParentMatrix <- function(Y){
  p <- dim(Y)[2]
  parentsMat <- matrix(0, p, p)
  parentEst <- sapply(1:p, FUN = getParentSet, Y)
  
  for(i in 1:p){
    parentsMat[i, parentEst[[i]]] <- 1
  }
  parentsMat <- ifelse(parentsMat + t(parentsMat) > 0, 1, 0)
  return(parentsMat)
}


sim.size <- 5
rec <- rep(0, sim.size)

for(i in 1:sim.size){
  cat(i)
  cat("\n")
  
  out_dag <- rDAG_hub(p = varParam, n = varParam * 3/4, nHubs = 3, lowScale = .8, highScale = 1, lowEdge = .65, highEdge = 1)
  
  Y <- out_dag$Y
  out <- getParentMatrix(Y)
  
  write.csv(Y, paste("dat/data_hub_",distro, "_", varParam, "_", i, ".csv", sep = ""))
  write.csv(out, paste("dat/skeleton_hub_",distro, "_", varParam, "_", i, ".csv", sep = ""))
  
  out_dag <- rDAG(p = varParam, n = varParam * 3/4, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                  lowEdge = .5, highEdge = 1)
  Y <- out_dag$Y
  out <- getParentMatrix(Y)
  write.csv(Y, paste("dat/data_",distro, "_",  varParam, "_", i, ".csv", sep = ""))
  write.csv(out, paste("dat/skeleton_",distro, "_",  varParam, "_", i, ".csv", sep = ""))
}



