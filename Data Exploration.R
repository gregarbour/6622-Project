library(dplyr)
library(ggplot2)
library(MASS)


load('DebTrivedi')
df <- DebTrivedi
remove(DebTrivedi)

######################################
############ Data Exploration ########
######################################
head(df)
str(df)

plot(table(df$ofp))
plot(table(rpois(n=1000, lambda = 5.8))) 
#Big difference beween our dataset at a fitted poisson distributino with lambda = 5.8
# Much more variance and large number of zeroes



fitdistr(df$ofp, densfun = 'poisson')
fitdistr(df$ofp, 'geometric')
fitdistr(df$ofp, 'negative binomial')


fitdistr(rpois(10000, lambda = 5), densfun = 'poisson')
fitdistr(rpois(100, lambda = 5), densfun = 'poisson')
fitdistr(rnorm(n=10, mean = 10), densfun = 'normal')



#log1p is used because ofp has some zero counts
ggplot(df, aes(x = factor(numchron), y = log1p(ofp))) + geom_boxplot()

#Variables included in example
# ofp: Number of physician office visits (Response)
# hosp: number of hospital stays
# health: self perceived health status
# numchron: Number of chronic conditions
# gender: (male(level 2)), female(level 1))
# school: Number of years of education
 
# Other variables we might include
# adldiff
# region: factor with 4 levels("midwest", "noreast"..)
# age: also on a weird scale. 6.6 - 10.9 by increments of 0.1. 36 unique values in total 
# black: yes/no
# married: yes/no
# faminc: family income, but on a weird scale
# employed: yes/no
# privins: Private insurance yes/no
# medicard: Medicaid yes/no

var1 <- c('hosp', 'health', 'numchron', 'gender', 'school')
f1 <- as.formula(paste('ofp ~', paste(var1, collapse = ' + ')))

var2 <- c('adldiff', 'region', 'age', 'black', 'married', 'faminc', 'employed', 'privins', 'medicaid')
f2 <- as.formula(paste('ofp ~', paste(c(var1, var2),  collapse = ' + ')))

######################################
############ Models ##################
######################################


#Regular Poisson
m_pois1 <- glm(f1, data = df, family = poisson)
m_pois2 <- glm(f2, data = df, family = poisson)

#Quasi Poisson
m_qpois1 <- glm(f1, data = df, family = quasipoisson)
m_qpois2 <- glm(f2, data = df, family = quasipoisson)

#Negative Binomial
m_nb1 <- glm.nb(f1, data = df)
m_nb2 <- glm.nb(f2, data = df)

#Zero Inflated Negative Binomial


#Zero Inflated Quasi Poisson

