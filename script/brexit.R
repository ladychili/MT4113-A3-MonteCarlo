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
diff(truesum$mean)


# MCfunc ------------------------------------------------------------------

spSize = 100
efSize = -10
mean0 = 55
sd0 = 10
sd1 = 8
  
MC.simulate <- function(spSize = 100, efSize, mean0, sd0, sd1, seed = 4113) {
  set.seed(seed)
  Eng <- data.frame(Region = c('England'), brexitRate = rnorm(spSize, mean0, sd0))
  set.seed(seed)
  nEng <- data.frame(Region = c('NonEngland'), brexitRate = rnorm(spSize, mean0 + efSize, sd1))
  data <- rbind(Eng,nEng)
  return(data)
}


tmp <- MC.simulate(100,-5,mean0 = 60,sd0 = 10,sd1 = 7, seed = 4113)
tmpsum <- tmp %>% group_by(Region) %>% 
  summarise(n = n(),
            mean = mean(brexitRate),
            sd = sd(brexitRate))

kableExtra::kable(tmpsum, format = 'markdown', digits = 2)
diff(truesum$mean)


