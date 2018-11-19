library(tidyverse)
library(reshape2)
library(viridis)
source("script/simulating.R")
source("script/MonteCarlo.R")


# data-wranggling ---------------------------------------------------------

brexit <- read_csv("data/referendum.csv")

# qplot(brexit[brexit$Region %in% c('Scotland','Wales','Northern Ireland'),]$`Percent Leave`, bins=10, 
#       main = "Non-England Regions Vote for Brexit",
#       xlab = "Percentage of Leave Votes",
#       ylab = "Numer of Regions") +
#   scale_y_continuous(breaks=seq(0,12,2))
# 
# qplot(brexit[!(brexit$Region %in% c('Scotland','Wales','Northern Ireland')),]$`Percent Leave`, bins =10,
#       main = "England Regions Vote for Brexit",
#       xlab = "Percentage of Leave Votes",
#       ylab = "Numer of Regions") +
# scale_y_continuous(breaks=seq(0,120,20))
# table(brexit$Region)

brxData <- brexit %>% select(Region, 'brexitRate' = `Percent Leave`)
brxData$Region[!(brxData$Region %in% c('Scotland','Wales','Northern Ireland'))] <- 'England'
brxData$Region[brxData$Region %in% c('Scotland','Wales','Northern Ireland')] <- 'NonEngland'

pdf("figure/datasetHist.pdf", height = 4)
ggplot(brxData, aes(brexitRate, fill = Region)) +
  geom_histogram(bins = 30, alpha = 0.7) + 
  geom_vline(xintercept=50,lwd = 1) +
  xlab("Percentage of Leave Votes") + xlim(c(0,100)) + 
  ylab("Count of Regions") + 
  ggtitle('Vote Outcome in England and Non-England Regions') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = c(25,75), y = 50, label = c("bold(Remain)","bold(Leave)"), parse = TRUE)
dev.off()

# summary -----------------------------------------------------------------

truesum <- brxData %>% group_by(Region) %>% 
  summarise(n = n(),
            mean = mean(brexitRate),
            sd = sd(brexitRate))

# Power-table-parametric --------------------------------------------------

samplesize <- c(10, 50, 100, 150, 200, 300, 400, 500, 700, 1000)
effectsize <- seq(10)

pwr <- matrix(NA, length(samplesize), length(effectsize), dimnames = list(samplesize,effectsize))

for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwr[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), 
                           seed = 4113)
  }
}


# Power-table-NonParametric -----------------------------------------------

pwrNonPar <- matrix(NA, length(samplesize), length(effectsize), dimnames = list(samplesize,effectsize))

for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwrNonPar[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), 
                                 param = FALSE, seed = 4113)
  }
}


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


# power-plots -------------------------------------------------------------
meltpwr <- melt(pwr,c("spSize","efSize"))
meltpwr$efSize <- as.factor(meltpwr$efSize)
meltpwr$spSize <- as.factor(meltpwr$spSize)

ggplot(meltpwr, mapping = aes(spSize,value, color = efSize, group = efSize)) +
  geom_line() +
  geom_point(size =2) + 
  scale_color_viridis(direction = -1,breaks = c(1,4,7,10), name = 'Effect Size') +
  labs(x = 'Sample Size', y = 'Power', title = "Power of Student's t Test under Different Scenarios") +
  theme(plot.title = element_text(hjust = 0.5))
 
