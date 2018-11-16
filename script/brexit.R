library(tidyverse)

brexit <- read_csv("data/referendum.csv")


hist(brexit[brexit$Region %in% c('Scotland','Wales','Northern Ireland'),]$`Percent Leave`,breaks = 20)
hist(brexit[!(brexit$Region %in% c('Scotland','Wales','Northern Ireland')),]$`Percent Leave`,breaks = 20)
table(brexit$Region)

brxData <- brexit %>% select(Region, 'brexitRate' = `Percent Leave`)
brxData$Region[!(brxData$Region %in% c('Scotland','Wales','Northern Ireland'))] <- 'England'
brxData$Region[brxData$Region %in% c('Scotland','Wales','Northern Ireland')] <- 'NonEngland'

hist(brxData$brexitRate,breaks = 20)


# summ --------------------------------------------------------------------

truesum <- brxData %>% group_by(Region) %>% 
  summarise(n = n(),
            mean = mean(brexitRate),
            sd = sd(brexitRate))

kableExtra::kable(truesum, format = 'markdown', digits = 2)



# MCfunc ------------------------------------------------------------------

  
simulating <- function(seed, spSize, dat0, dat1) {
  set.seed(seed)
  Eng <- data.frame(Region = c('England'), brexitRate = rnorm(spSize, dat0[1], dat0[2]))
  set.seed(seed)
  nEng <- data.frame(Region = c('NonEngland'), brexitRate = rnorm(spSize, dat1[1], dat1[2]))
  data <- rbind(Eng,nEng)
  return(data)
}

# simulating a dataset
tmp <- simulating(seed = 4113, 100, dat0 = c(50.4,3), dat1 = c(50,3))
tmpsum <- tmp %>% group_by(Region) %>% 
  summarise(n = n(),
            mean = mean(brexitRate),
            sd = sd(brexitRate))

kableExtra::kable(tmpsum, format = 'markdown', digits = 2)

Eng <- tmp$brexitRate[1:(nrow(tmp)/2)]
nonEng <- tmp$brexitRate[-(1:(nrow(tmp)/2))]
p.val <- t.test(Eng, nonEng,alternative = 'g')$p.value


library(snow)
MonteCarlo <- function(n = 1000, spSize, dat0, dat1, param = TRUE, seed = NULL) {
  
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
      p.vals[i] <- t.test(x,y,alternative = 'g')$p.value
    }
  } else {
    for (i in 1:n) {
      x <- datasets[[i]]$brexitRate[1:spSize]
      y <- datasets[[i]]$brexitRate[-(1:spSize)]
      p.vals[i] <- wilcox.test(x,y,alternative = 'g')$p.value
    }
  }
  return(sum(p.vals<0.05)/n)
}




# forloop-scenario --------------------------------------------------------
samplesize <- seq(10, 300, 30)
effectsize <- seq(10)

pwr <- matrix(NA, length(samplesize), length(effectsize))
for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwr[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), seed = 4113)
  }
}
