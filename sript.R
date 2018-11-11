library(dslabs)
library()

# Import and first glance
data("us_contagious_diseases")
usdat <- us_contagious_diseases
usdat[sample(10),]
table(usdat$disease)

# cleaning
usdat$century[usdat$year < 2000] <- 20
usdat$century[usdat$year >= 2000] <- 21
