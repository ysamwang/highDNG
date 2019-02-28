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


distro <- "unif"
if(distro == "gamma"){
  deg <- 3
} else {
  deg <- 4
}
maxInDegree <- 2


sim.size <- 20


for(i in 1:sim.size){
  cat(i)
  cat("\n")
  out_dag <- rDAG(p = varParam, n = varParam * 3/4, maxInDeg = maxInDegree, dist = distro, lowScale = .8, highScale = 1,
                  lowEdge = .65, highEdge = 1)
  Y <- out_dag$Y
  out <- pcalg::pc(suffStat = list(C = cor(Y), n = varParam * 3/4), indepTest = pcalg::gaussCItest, alpha = .05, labels = as.character(1:varParam))
  
  write.csv(paste("data", varParam, "_", i, ".csv", sep = ""), Y)
  write.csv(paste("skeleton_", varParam, "_", i, ".csv", sep = ""), as(out, "amat"))
}


