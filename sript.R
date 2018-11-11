library(dslabs)
library(tidyverse)

# Import and first glance
data("us_contagious_diseases")
usdat <- us_contagious_diseases
rm(us_contagious_diseases)


# cleaning
usdat <- usdat %>% mutate(century = ceiling(year/100),
                          rate = count/population)

# true-polio --------------------------------------------------------------

polio <- usdat %>% 
  na.omit() %>% 
  filter(disease == "Polio", weeks_reporting != 0) %>% 
  select(century,rate)

truesum <- polio %>% group_by(century) %>% 
  summarise(n = n(),
            mean = mean(rate),
            sd = sd(rate))

kableExtra::kable(truesum, format = 'markdown', digits = 10)

set.seed(4113)
polio500 <- polio %>% group_by(century) %>% sample_n(500)

sum500 <- polio500 %>% group_by(century) %>% 
  summarise(n = n(),
            mean = mean(rate),
            median = median(rate),
            sd = sd(rate),
            max = max(rate),
            min = min(rate))

kableExtra::kable(sum500, format = 'markdown', digits = 10)


# scenarios ---------------------------------------------------------

set.seed(4113)
cent20 <- data.frame(century = 20, rate = rnorm(1000, truesum$mean[1], truesum$sd[1]))
set.seed(4113)
cent21 <- data.frame(century = 21, rate = rnorm(1000, truesum$mean[2], truesum$sd[2]))
sceno1 <- bind_rows(cent20,cent21)

sceno1[sample(2000,10),]

# tests -------------------------------------------------------------------

# Parametric - Paired t-test

spSize = 100
efSize = -3.5e-04
mean0 = 4e-04
sd0 = 

MC.simulate <- function(spSise = 100, efSize, mean0, sd0, sd1, seed = 4113) {
  cent20 <- data.frame(century = 20, rate = rnorm(spSise))
}






