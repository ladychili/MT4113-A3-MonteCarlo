simulating <- function(seed, spSize, dat0, dat1) {
  set.seed(seed)
  Eng <- data.frame(Region = c('England'), brexitRate = rnorm(spSize, dat0[1], dat0[2]))
  set.seed(seed)
  nEng <- data.frame(Region = c('NonEngland'), brexitRate = rnorm(spSize, dat1[1], dat1[2]))
  data <- rbind(Eng,nEng)
  return(data)
}