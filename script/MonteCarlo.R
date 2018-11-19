library(snow)
MonteCarlo <- function(n = 1000, spSize, dat0, dat1, rndto = NULL, param = TRUE, seed = NULL) {
  
  mycl <- makeSOCKcluster(rep('localhost',3))
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