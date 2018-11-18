library(tidyverse)

brexit <- read_csv("data/referendum.csv")


qplot(brexit[brexit$Region %in% c('Scotland','Wales','Northern Ireland'),]$`Percent Leave`, bins=10, 
      main = "Non-England Regions Vote for Brexit",
      xlab = "Percentage of Leave Votes",
      ylab = "Numer of Regions") +
  scale_y_continuous(breaks=seq(0,12,2))

qplot(brexit[!(brexit$Region %in% c('Scotland','Wales','Northern Ireland')),]$`Percent Leave`, bins =10,
      main = "England Regions Vote for Brexit",
      xlab = "Percentage of Leave Votes",
      ylab = "Numer of Regions") +
scale_y_continuous(breaks=seq(0,120,20))
table(brexit$Region)

brxData <- brexit %>% select(Region, 'brexitRate' = `Percent Leave`)
brxData$Region[!(brxData$Region %in% c('Scotland','Wales','Northern Ireland'))] <- 'England'
brxData$Region[brxData$Region %in% c('Scotland','Wales','Northern Ireland')] <- 'NonEngland'

hist(brxData$brexitRate,breaks = 20)
ggplot(brxData, aes(brexitRate, fill = Region)) +
  geom_histogram(bins = 30, alpha = 0.7) + 
  geom_vline(xintercept=50,lwd = 1) +
  scale_x_continuous(breaks=seq(0,100,10))

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
p.val <- t.test(round((Eng),2), round(nonEng,2),alternative = 'g')$p.value


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


MonteCarlo(1000, spSize = 150, dat0 = c(50,10), dat1 = c(50,1), rndto = 1,seed = 4113)

samplesize <- c(10, 50, 100, 150, 200, 300, 400, 500, 700, 1000)

# Power-table-parametric --------------------------------------------------

effectsize <- seq(10)

pwr <- matrix(NA, length(samplesize), length(effectsize), dimnames = list(samplesize,effectsize))

for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwr[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), 
                           seed = 4113)
  }
}
kableExtra::kable(pwr[c(1,8,10),c(1,5,10)],format = "markdown")

# Power-table-NonParametric -----------------------------------------------

pwrNonPar <- matrix(NA, length(samplesize), length(effectsize), dimnames = list(samplesize,effectsize))

for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwrNonPar[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), 
                                 param = FALSE, seed = 4113)
  }
}
kableExtra::kable(pwrNonPar[c(1,8,10),c(1,5,10)],format = "markdown")

# Size-table-parametric ---------------------------------------------------

SDdiff <- rep(c(0,3,5,8,10),2)

siz <- matrix(NA, length(samplesize), length(SDdiff), dimnames = list(samplesize, SDdiff))

for (i in 1:length(samplesize)) {
  for (j in 1:5) {
    siz[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(50,1+SDdiff[j]), dat1 = c(50,1),
                           seed = 4113)
  }
  for (j in 6:10) {
    siz[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(50,1+SDdiff[j]), dat1 = c(50,1),
                           rndto = 0, seed = 4113)
  }
}

kableExtra::kable(siz[c(1,8,10),c(1,5,10)],format = "markdown")


# Size-table-NonParametric ------------------------------------------------

sizNonPar <- matrix(NA, length(samplesize), length(SDdiff), dimnames = list(samplesize, SDdiff))

for (i in 1:length(samplesize)) {
  for (j in 1:5) {
    sizNonPar[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(50,1+SDdiff[j]), dat1 = c(50,1),
                                 param = FALSE, seed = 4113)
  }
  for (j in 6:10) {
    sizNonPar[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(50,1+SDdiff[j]), dat1 = c(50,1),
                                 rndto = 0, param = FALSE, seed = 4113)
  }
}

kableExtra::kable(sizNonPar[c(1,8,10),c(1,5,10)],format = "markdown")
