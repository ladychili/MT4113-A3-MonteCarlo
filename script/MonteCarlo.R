library(snow)
MonteCarlo <- function(n = 1000, spSize, dat0, dat1, rndto = NULL, param = TRUE, seed = NULL, core = 3) {
  # Purpose: conduct Monte Carlo simulation, calculate power/size
  # Inputs:
  #   n - integer, number of iteration, 1000 by defualt
  #   spSize - integer, sample size for two groups of samples
  #   dat0 - numeric vector of length 2, first element as mean, second element as sd
  #   dat1 - numeric vector of length 2, first element as mean, second element as sd
  #   rndto - integer, round observation in to how many decimal place, NULL by default
  #   param - logical, TRUE conduct t test, FALSE conduct M-W test, TRUE by default
  #   seed - numeric, for reproducible purpose, NULL by default
  #   core - integer, how many cores to be used to run in parallel, 3 by default
  # Output: 
  #   a number between 0 and 1
  
  mycl <- makeSOCKcluster(rep('localhost',core))
  if (hasArg(seed)) set.seed(seed)
  seedindex <- sample(1e4, size = n)
  datasets <- parLapply(mycl, seedindex, simulating, spSize=spSize, dat0=dat0, dat1=dat1)
  p.vals <- vector()
  stopCluster(mycl)
  
  if (param) {
    for (i in 1:n) {
      x <- datasets[[i]]$brexitRate[1:spSize]
      y <- datasets[[i]]$brexitRate[-(1:spSize)]
      if (hasArg(rndto)) {
        x <- round(x, rndto)
        y <- round(y, rndto)}
      p.vals[i] <- t.test(x,y,alternative = 'g')$p.value
    }
  } else {
    for (i in 1:n) {
      x <- datasets[[i]]$brexitRate[1:spSize]
      y <- datasets[[i]]$brexitRate[-(1:spSize)]
      if (hasArg(rndto)) {
        x <- round(x, rndto)
        y <- round(y, rndto)}
      p.vals[i] <- wilcox.test(x,y,alternative = 'g')$p.value
    }
  }
  return(sum(p.vals<0.05)/n)
}