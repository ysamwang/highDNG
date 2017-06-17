
.calcTau <- function(k, pa, ch) {
  abs(mean(pa^(k - 1) * ch) * mean(pa^2) - mean(pa^k) * mean(pa * ch))
}

generate.errs <- function(n, distro = "unif"){
  if(distro == "unif"){
    return(runif(n, -sqrt(3), sqrt(3)))
  } else if (distro == "gamma"){
    return(rgamma(n, 1, 1) - 1)
  }
}

n.list <- c(100, 200, 500)
numZ.list <- c(1:4)
distro <- "unif"
sim.size <- 1000
count <- 0
rec <- matrix(0, nrow = sim.size, ncol = 9)
rec[, 1] <- rep(n.list, each = sim.size * length(numZ.list))
for(n in n.list){
  for(k in 1:sim.size){
    count <- count + 1
    Z <- matrix(generate.errs(n * numZ, distro), nrow = n, ncol = numZ)
    
    i <- generate.errs(n, distro = distro) + Z %*% runif(numZ, -2, 2)
    j <- i * runif(1, -2, 2)+ generate.errs(n, distro = distro) + Z %*% runif(numZ, -2, 2)
    
    i.z <- lm(i ~ Z - 1)$res
    j.z <- lm(j ~ Z - 1)$res
  
  
  rec[count, ] <- c(abs(mean(lm(i ~ j  + Z - 1)$res^3 * j)) > abs(mean(lm(j ~ i  + Z - 1)$res^3 * i)), 
    abs(mean(lm(i ~ j  + Z - 1)$res* j^3)) > abs(mean(lm(j ~ i  + Z - 1)$res * i^3)) ,
    abs(mean(lm(i ~ j.z - 1)$res^3 * j.z)) > abs(mean(lm(j ~ i.z - 1)$res^3 * i.z)),
    abs(mean(lm(i ~ j.z - 1)$res * j.z^3)) > abs(mean(lm(j ~ i.z)$res * i.z^3)),
    .calcTau(4, j.z, i) > .calcTau(4, i.z, j),
    .calcTau(4, j, i.z) > .calcTau(4, i, j.z),
    .calcTau(4, j.z, i.z) > .calcTau(4, i.z, j.z))
  }
}

write.table(rec,"statistics_compare.csv", sep = ",", row.names = F)





