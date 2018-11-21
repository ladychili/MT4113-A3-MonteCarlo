simulating <- function(seed, spSize, dat0, dat1) {
  # Purpose: simulating dataset for scenarios
  # Inputs:
  #   seed - numeric, for reproducible purpose, NULL by default
  #   spSize - integer, sample size for two groups of samples
  #   dat0 - numeric vector of length 2, first element as mean, second element as sd
  #   dat1 - numeric vector of length 2, first element as mean, second element as sd

  # Output: 
  #   2n by 2 dataset, n is sample size
  
  set.seed(seed)
  Eng <- data.frame(Region = c('England'), brexitRate = rnorm(spSize, dat0[1], dat0[2]))
  set.seed(seed)
  nEng <- data.frame(Region = c('NonEngland'), brexitRate = rnorm(spSize, dat1[1], dat1[2]))
  data <- rbind(Eng,nEng)
  return(data)
}