library(GGally)
library(dplyr)
library(ggplot2)
library(MASS)


load('DebTrivedi')
df <- DebTrivedi
remove(DebTrivedi)

head(df)
str(df)
hist(df$ofp)

fitdistr(df$ofp, densfun = 'poisson')
fitdistr(df$ofp, 'geometric')
fitdistr(df$ofp, 'negative binomial')

a <- rpois(10000, lambda = 5)
fitdistr(a, densfun = 'poisson')
