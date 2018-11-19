library(tidyverse)
library(reshape2) # for plots
library(viridis)  # for plots
library(ggpubr)
source("script/simulating.R")
source("script/MonteCarlo.R")


# data-wranggling ---------------------------------------------------------

brexit <- read_csv("data/referendum.csv")

brxData <- brexit %>% select(Region, 'brexitRate' = `Percent Leave`)
brxData$Region[!(brxData$Region %in% c('Scotland','Wales','Northern Ireland'))] <- 'England'
brxData$Region[brxData$Region %in% c('Scotland','Wales','Northern Ireland')] <- 'NonEngland'

# histogram of true data
pdf("figure/datasetHist.pdf", width = 5.5, height = 2.5)
ggplot(brxData, aes(brexitRate, fill = Region)) +
  geom_histogram(bins = 30, alpha = 0.7) + 
  geom_vline(xintercept=50,lwd = 1) +
  xlab("Percentage of Leave Votes") + xlim(c(0,100)) + 
  ylab("Count of Regions") + 
  ggtitle('Vote Outcome in England and Non-England Regions') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = c(25,75), y = 50, label = c("bold(Remain)","bold(Leave)"), parse = TRUE)
dev.off()

# summary
truesum <- brxData %>% group_by(Region) %>% 
  summarise(n = n(),
            mean = mean(brexitRate),
            sd = sd(brexitRate))
truesum


# Power-tables ------------------------------------------------------------

samplesize <- c(10, 50, 100, 150, 200, 300, 400, 500, 700, 1000)
effectsize <- seq(10) # mean difference between two groups

# calc power of t test using 1000 simulated datasets

pwr <- matrix(NA, length(samplesize), length(effectsize), dimnames = list(samplesize,effectsize))

for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwr[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), 
                           seed = 4113)
  }
}


# calc power of MannW test using 1000 simulated datasets

pwrNonPar <- matrix(NA, length(samplesize), length(effectsize), dimnames = list(samplesize,effectsize))

for (i in 1:length(samplesize)) {
  for (j in 1:length(effectsize)) {
    pwrNonPar[i,j] <- MonteCarlo(1000, spSize = samplesize[i], dat0 = c(45+effectsize[j],10), dat1 = c(45,8), 
                                 param = FALSE, seed = 4113)
  }
}



# Size-tables -------------------------------------------------------------

SDdiff <- rep(c(0,3,5,8,10),2) # Standard Deviation difference between two groups

# calc size of t test using 1000 simulated datasets

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


# calc size of MannW test using 1000 simulated datasets

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



# Power-plot --------------------------------------------------------------

# power of t test

meltpwr <- melt(pwr,c("spSize","efSize"))
meltpwr$spSize <- as.factor(meltpwr$spSize)

p1 <- ggplot(meltpwr, mapping = aes(spSize,value, color = efSize, group = efSize)) +
  geom_line() +
  geom_point(size =2) + 
  scale_color_viridis(direction = -1,breaks = c(1,4,7,10), name = 'Effect Size') +
  labs(x = 'Sample Size', y = 'Power', title = "Power of Student's t Test") +
  theme(plot.title = element_text(hjust = 0.5))


# power of MW test

meltpwrNonPar <- melt(pwrNonPar,c("spSize","efSize"))
meltpwrNonPar$spSize <- as.factor(meltpwrNonPar$spSize)

p2 <- ggplot(meltpwrNonPar, mapping = aes(spSize,value, color = efSize, group = efSize)) +
  geom_line() +
  geom_point(size =2) + 
  scale_color_viridis(direction = -1,breaks = c(1,4,7,10), name = 'Effect Size') +
  labs(x = 'Sample Size', y = 'Power', title = "Power of Mann-Whitney U Test") +
  theme(plot.title = element_text(hjust = 0.5))

# combine and save
p <- ggarrange(p1,p2, ncol = 2, common.legend = TRUE, legend ="bottom")
pdf("figure/power.pdf", width = 10, height = 4)
p
dev.off()


# Size-plot ---------------------------------------------------------------

# size of t test
meltsiz <- melt(siz, c("spSize","SDdiff"))
meltsiz$rnd <- c(rep("No",50),rep("Yes",50))
meltsiz$spSize <- as.factor(meltsiz$spSize)
meltsiz$SDdiff <- as.factor(meltsiz$SDdiff)

p1 <- ggplot(meltsiz,aes(spSize,value, color = SDdiff, shape = rnd, group=interaction(SDdiff,rnd))) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_brewer(limits=c(-1,0,3,5,8,10), palette=3, breaks=c(10,8,5,3,0), name='Difference of SD')+
  labs(x = 'Sample Size', y = 'Size', title = "Size of Student's t Test", shape = "Round to 0") +
  theme(plot.title = element_text(hjust = 0.5))


# size of MW test

meltsizNon <- melt(sizNonPar, c("spSize","SDdiff"))
meltsizNon$rnd <- c(rep("No",50),rep("Yes",50))
meltsizNon$spSize <- as.factor(meltsiz$spSize)
meltsizNon$SDdiff <- as.factor(meltsiz$SDdiff)

p2 <- ggplot(meltsizNon,aes(spSize,value, color = SDdiff, shape = rnd, group=interaction(SDdiff,rnd))) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_brewer(limits=c(-1,0,3,5,8,10),palette=3, breaks=c(10,8,5,3,0), name='Difference of SD') +
  labs(x = 'Sample Size', y = 'Size', title = "Size of Mann-Whitney U Test", shape = "Round to 0") +
  theme(plot.title = element_text(hjust = 0.5))

# combine and save
p <- ggarrange(p1,p2, ncol = 2, common.legend = TRUE, legend ="right")
pdf("figure/size.pdf", width = 10, height = 4)
p
dev.off()
