ind <- 2

args <- (commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text = args[[i]]))
}
print(ind)

getOrderingPostProcessing <- function(skel, Y){
  p <- dim(Y)[2]
  B <- matrix(0, p, p)
  
  
  ## Get estimates of linear coefficients
  for(i in 1:p){
    if(sum(skel[i, ]) > 0){
      parents <- which(skel[i, ] != 0) 
      B[i, parents] <- lm(Y[, i] ~ ., data= Y[, parents, drop = F])$coef[-1]
    }
  }
  
  prunedB <- sltprune(B)
  return(list(ordering = prunedB$optperm, Bpruned = prunedB$Bopt, Braw = B,
              cutoff = prunedB$cutoff))
}



sltprune <- function(B) {
  ## Hack of JONAS PETERS 2013
  n <- nrow(B)
  ##[y,ind] = sort(abs(B(:)));
  ind <- sort.list(abs(B))
  
  for(i in ((n*(n+1)/2):(n*n))) {
    Bi <- B ## Bi := B, with the i smallest (in absolute value) coefficients to zero
    Bi[ind[1:i]] <- 0
    ## Try to do permutation
    p <- slttestperm( Bi )
    
    ## If we succeeded, then we're done!
    if(any(p != 0)) {
      Bopt <- B[p,p, drop=FALSE]
      break
    }
    ## ...else we continue, setting one more to zero!
  }
  ## return :
  list(Bopt = Bopt, optperm = p, cutoff = min(abs(Bi[Bi != 0])))
}

##' Measuring how close B is to SLT = Strict Lower Triangular
sltscore <- function (B) sum((B[upper.tri(B,diag=TRUE)])^2)

slttestperm <- function(B, rowS.tol = 1e-12)
{
  ## Hack of JONAS PETERS 2013;  tweaks: MM, 2015-07
  ##
  ## slttestperm - tests if we can permute B to strict lower triangularity
  ##
  ## If we can, then we return the permutation in p, otherwise p=0.
  ##
  
  ## Dimensionality of the problem
  stopifnot((n <- nrow(B)) >= 1, rowS.tol >= 0)
  
  ## This will hold the permutation
  p <- integer(0)
  
  ## Remaining nodes
  remnodes <- 1:n
  
  ## Remaining B, take absolute value now for convenience
  Brem <- abs(B)
  ## Select nodes one-by-one
  for(ii in 1:n)
  {
    ## Find the row with all zeros
    ## therow = find(sum(Brem,2)<1e-12)
    rowS <- if(length(Brem) > 1) rowSums(Brem) else Brem
    therow <- which(rowS < rowS.tol)
    
    ## If empty, return 0
    if(length(therow) == 0L)
      return(0L)
    ## If we made it to the end, then great!
    if(ii == n)
      return(c(p, remnodes))
    ## If more than one, arbitrarily select the first
    # therow <- therow[1]
    therow <- therow[length(therow)]
    # therow <- ifelse(length(therow) == 1, therow, sample(therow, size = 1))
    
    ## Take out that row and that column
    Brem <- Brem[-therow, -therow, drop=FALSE]
    ### CHECK!!!!
    
    ## Update remaining nodes
    p <- c(p,remnodes[therow])
    remnodes <- remnodes[-therow]
  }
  stop("the program flow should never get here [please report!]")
}






sim.size <- 20

varList <- c(100, 200, 500, 1000, 1500)
varParam <- varList[ind]
cor.rec <- rep(0, sim.size)
pw.rec <- rep(0, sim.size)

for(i in 1:sim.size){
  A <- read.csv(paste("res/adj_unif_", varParam, "_",i,".csv", sep = ""), header = F)
  Y <- read.csv(paste("dat/data_unif_", varParam, "_",i,".csv", sep = ""), header = T)[, -1]
  output <- getOrderingPostProcessing(A, Y)
  B.est <- ifelse(abs(output$Braw) >= output$cutoff, output$Braw, 0) 
  
  print(output$ordering)
  pw.rec[i] <- sum(A[upper.tri(A)]) /sum(A) 
  cor.rec[i] <- cor(output$ordering, 1:varParam, method = "kendall")
  write.csv(cor.rec, paste("res/pwl_cor_", varParam,".csv", sep = ""))
  write.csv(pw.rec, paste("res/pw_pctCorrect_", varParam,".csv", sep = ""))
}


