library(ggplot2)
library(GGally)
library(MASS)
library(pscl)
library(dplyr)

load('DebTrivedi')
df <- DebTrivedi
remove(DebTrivedi)

names(df)

set.seed(1337)

######################################
############ Data Exploration ########
######################################
head(df)
str(df)

mean(df$ofp) # mean about 5.8
var(df$ofp)  # var about 45.68
## overdispersion is a problem with this response


## any other **count** columns without overdispersion?
sapply(df[, c(1:6, 8)], mean)  ## Count Variable Means
sapply(df[, c(1:6, 8)], var)   ## Count Variable Variances


## NUMCHRON seems to be a lot better.. maybe we should predict that instead
## seems to match the distribution well.. not zero inflated and very little overdispersion
dataplot <- function(){
barplot(table(df$numchron), xlab = "Number of Chronic Diseases", ylab = "Frequency")
poigen <- rpois(length(df$numchron), lambda = mean(df$numchron))
barplot(table(poigen), width=0.5, space=c(0.9, rep(1.4, length(unique(poigen))-1)), 
        col="red", add=TRUE, names.arg = "")
legend(5, 1400, legend=c("Data", expression(paste("Poisson Dist., ", lambda, "= 1.54"))),
       col=c("grey", "red"), pch= 15, cex=0.8)
}
dataplot()


#This is a sweet plot. Nice job Tino
#x is a vector of data. Can supply labda or use default = mean(x)
dataplot2 <- function(x, lambda = mean(x)){
  barplot(table(x), xlab = "Number of Chronic Diseases", ylab = "Frequency")
  poigen <- rpois(length(df$numchron), lambda = lambda)
  barplot(table(poigen), width=0.5, space=c(0.9, rep(1.4, length(unique(poigen))-1)), 
          col="red", add=TRUE, names.arg = "")
  legend(5, 1400, legend=c("Data", expression(paste("Poisson Dist., ", lambda, "= 1.54"))),
         col=c("grey", "red"), pch= 15, cex=0.8)
}
dataplot(df$numchron)
dataplot2(df$numchron, lambda = 5)



## too much overdispersion, we haven't looked at this in class much
## so we might not be able to do any of the goodness of fit tests -
## I'm not sure if they work with zero-inflated models
plot(table(df$ofp))
plot(table(rpois(n=1000, lambda = 5.77))) 
dataplot2(df$ofp)

mean(df$ofp)
var(df$ofp)
# Big difference beween our dataset at a fitted poisson distribution with lambda = 5.77
# Much more variance and large number of zeroes
# Quasi poisson or zero-inflated poisson would fit much better

####### OFP PARAMETER MLE ########
#I don't think we need to include these, but leaving them in just in case. 
# The test doesn't actually work for our purposes of showing standard Poisson is a bad fit
fitdistr(df$ofp, densfun = 'poisson') 
fitdistr(df$ofp, 'geometric')
fitdistr(df$ofp, 'negative binomial')






####### VARIABLE EXPLANATIONS ########

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




################################################
############ Exploratory Stuff ################
###############################################

var1 <- c('hosp', 'health', 'numchron', 'gender', 'school')
#var2 <- c('adldiff', 'region', 'age', 'black', 'married', 'faminc', 'employed', 'privins', 'medicaid')
# I propose adding only the three variables below, instead of all the ones I initially listed above
var2 <- c('age', 'privins', 'medicaid') #These three variables are the most significant in the 3 models below

#Create dataframes with only the variables listed above
df1 <- dplyr::select(df, ofp, one_of(var1))
df2 <- dplyr::select(df, ofp, one_of(var1), one_of(var2))

ggplot(df, aes(x = ofp, y = hosp)) + geom_point() + geom_smooth(method = 'lm')
ggplot(df, aes(x = ofp, y = numchron)) + geom_point() + geom_smooth(method = 'lm')
ggplot(df, aes(x = ofp, y = school)) + geom_point() + geom_smooth(method = 'lm')
ggplot(df, aes(x = ofp, y = age)) + geom_point() + geom_smooth(method = 'lm')

ggplot(df, aes(x = ofp)) + geom_histogram() + facet_wrap(~health)
ggplot(df, aes(x = ofp)) + geom_histogram() + facet_wrap(~gender)
ggplot(df, aes(x = ofp)) + geom_histogram() + facet_wrap(~privins)
ggplot(df, aes(x = ofp)) + geom_histogram() + facet_wrap(~medicaid)


#log1p is used because ofp has some zero counts
ggplot(df, aes(x = factor(numchron), y = log1p(ofp))) + geom_boxplot()

#Table 1 for all variables (I'm manually copying the output to Excel for formatting)
numeric_vars <- c('ofp', 'hosp', 'numchron', 'school', 'age')
numeric_sum <- t(sapply(dplyr::select(df, one_of(numeric_vars)), FUN = summary))

cat_sum <- df %>% group_by(health) %>% 
  summarise(Count = n(),
            Percent_total = n()/nrow(df),
            Mean = mean(ofp),
            Median = median(ofp),
            Std_dev = sd(ofp)) %>% 
  ungroup()
cat_sum$Variable = 'Health'
names(cat_sum)[1] <- 'Levels'

gender_sum <- df %>% group_by(gender) %>% 
  summarise(Count = n(),
            Percent_total = n()/nrow(df),
            Mean = mean(ofp),
            Median = median(ofp),
            Std_dev = sd(ofp)) %>% 
  ungroup()
gender_sum$Variable = 'Gender'
names(gender_sum)[1] <- 'Levels'

privins_sum <- df %>% group_by(privins) %>% 
  summarise(Count = n(),
            Percent_total = n()/nrow(df),
            Mean = mean(ofp),
            Median = median(ofp),
            Std_dev = sd(ofp)) %>% 
  ungroup()
privins_sum$Variable = 'Private Insurance'
names(privins_sum)[1] <- 'Levels'

medicaid_sum <- df %>% group_by(medicaid) %>% 
  summarise(Count = n(),
            Percent_total = n()/nrow(df),
            Mean = mean(ofp),
            Median = median(ofp),
            Std_dev = sd(ofp)) %>% 
  ungroup()
medicaid_sum$Variable = 'Has Medicaid'
names(medicaid_sum)[1] <- 'Levels'

cat_sum <- bind_rows(cat_sum, gender_sum, privins_sum, medicaid_sum)




######################################
############ Models ##################
######################################

######### OFP FORMULAS ###########
f1 <- as.formula(paste('ofp ~', paste(var1, collapse = ' + ')))
f2 <- as.formula(paste('ofp ~', paste(c(var1, var2),  collapse = ' + ')))

######### OFP MODELS ##########

#Regular Poisson
# m_pois1 <- glm(f1, data = df, family = poisson)
# m_pois2 <- glm(f2, data = df, family = poisson)

#Quasi Poisson
m_qp1 <- glm(f1, data = df, family = quasipoisson)
m_qp2 <- glm(f2, data = df, family = quasipoisson)

## Check to see if this if quasi is neccessary by looking at the dispersion parameters
s_qp1 <- summary(m_qp1)
s_qp2 <- summary(m_qp2)

## dispersion parameters
c("Model 1" = s_qp1$dispersion, "Model 2" = s_qp2$dispersion)
# Dispersion parameters are large!

#Test if the use of a dispersion parameter is warranted.
#TINO - I can't get this package to install properly. Can you?
library(AER)
dispersiontest(m_qp1, trafo = 1) #trafo parameter specifies that the null hypothesis is that dispersion param = 1
dispersiontest(m_qp2, trafo = 1)

#Negative Binomial
m_nb1 <- glm.nb(f1, data = df)
m_nb2 <- glm.nb(f2, data = df)
summary(m_nb1)
summary(m_nb2)

#Zero Inflated Negative Binomial
m_zero_nb0 <- zeroinfl(ofp ~ 1, data = df, dist = 'negbin') #Null model with just an intercept
m_zero_nb1 <- zeroinfl(f1, data = df, dist = 'negbin')
m_zero_nb2 <- zeroinfl(f2, data = df, dist = 'negbin')
summary(m_zero_nb1)
summary(m_zero_nb2)




################################################
############# Model Diagnostics ################
################################################

#Deviance test of additional parameters
(deviance(m_qp1) - deviance(m_qp2)) > qchisq(0.95, 3)
(deviance(m_nb1) - deviance(m_nb2)) > qchisq(0.95, 3)
# deviance() function doesn't work for zero_inflated models


#Comparing adding the three additional parameters
anova(m_nb1, m_nb2)
anova(m_qp1, m_qp2)
anova(m_zero_nb1, m_zero_nb2) #ANOVA doesn't work for this type of model. See LR-test below for a suitable replacement


# Likelihood ratio test (I think)
# https://stats.idre.ucla.edu/r/dae/zip/
pchisq(2 * (logLik(m_zero_nb2) - logLik(m_zero_nb1)), df = 3, lower.tail = FALSE) 
pchisq(2 * (logLik(m_zero_nb1) - logLik(m_zero_nb0)), df = 5, lower.tail = FALSE)


#There are a few options for comparing models to pick the "best" one
# We can use AIC and BIC (or QAIC in MuMin packge for overdispersed count data)

# We can also use glmmulti to find the best subset of predictors based on some criteria (e.g. AIC)

# The approach we use might depend on the goal of modeling. If it's inference, then we should include all variables that we have
# a reason to suspect belong in the causal model of the process
# If it is just prediction, then a parsimonious model that still explains a large proportion of the variation is our best 
# chance for prediction without overfitting. In this case AIC & BIC are likely the best guides for this.







########### NUMCHRON FORMULAS ###########

var1n <- c('hosp', 'health', 'ofp', 'gender', 'school')
f1n <- as.formula(paste('numchron ~', paste(var1n, collapse = ' + ')))

var2n <- c('adldiff', 'region', 'age', 'black', 'married', 'faminc', 'employed', 'privins', 'medicaid')
f2n <- as.formula(paste('numchron ~', paste(c(var1n, var2n),  collapse = ' + ')))

var3n <- c("ofp", "ofnp", "opp", "opnp", "emer") 
## why not add in these extra counts for the saturated model??
f3n <- as.formula(paste('numchron ~', paste(c(var3n, var1n, var2n), collapse = ' + ')))


########### NUMCHRON MODELS #############
### check to make sure negative binomial, zero inflated not necessary
### Zero inflated is fine from the graph - but we need to make sure dispersion parameter is close to 1

#Quasi Poisson
m_qpois1n <- glm(f1n, data = df, family = quasipoisson)
m_qpois2n <- glm(f2n, data = df, family = quasipoisson)
m_qpois3n <- glm(f3n, data = df, family = quasipoisson)

## Check to see if this if quasi is neccessary by looking at the dispersion parameters
s_qp1n <- summary(m_qpois1n)
s_qp2n <- summary(m_qpois2n)
s_qp3n <- summary(m_qpois3n)

## dispersion parameters
c("Model 1" = s_qp1n$dispersion, "Model 2" = s_qp2n$dispersion, "Model 3" = s_qp3n$dispersion)
## CLOSE ENOUGH TO 1 NOT TO MATTER!!!


### We need to come up with a better solution for feature selection,
### but just to check out the model this is okay

### I think there are model selection methods in the textbook that we should use

## Regular Poisson
m_pois1n <- glm(f1n, data = df, family = poisson)
m_pois2n <- glm(f2n, data = df, family = poisson)
m_pois3n <- glm(f3n, data = df, family = poisson)

summary(m_pois1n)
summary(m_pois2n)
summary(m_pois3n)

### Let's test the difference in deviance of model 3n and model 2n
(deviance(m_pois2n) - deviance(m_pois3n)) > qchisq(0.95, 5)
## TRUE means reject -> I believe we keep more complex model?

## test sequential adding of all predictors
anova(m_pois3n, test="Chisq")

###### OFFSET
## I think we should consider checking whether or not we should include an offset in our
## model, considering our mean should be thought of as a rate - as in number of chronic
## diseases over the course of a number of years of age. If we don't include an offset it
## may cause bias in our estimates. So we rethink of the mean as #diseases/#years of age
## which will add "complexity" to our model and possibly get more valid results - let me 
## know what you think.

####################### VARIABLE TRANSFORMATION ########################
##### Change the two-factor variables to boolean
## Factors can be releveled and cause confusion if baseline factor is not intuitive

## adldiff
table(df$adldiff)
table(as.numeric(df$adldiff)) #no is level 1, yes is level 2
df$adldiff <- as.logical(as.numeric(df$adldiff) - 1)

## black
table(df$black)
table(as.numeric(df$black)) # no is level 1, yes is level 2
df$black <- as.logical(as.numeric(df$black) - 1)

## gender (true = male, name change to male instead of gender)
table(df$gender)
table(as.numeric(df$gender)) # female is level 1, male is level 2
df$gender <- as.logical(as.numeric(df$gender) - 1)

## married
table(df$married)
table(as.numeric(df$married)) # no is level 1, yes is level 2
df$married <- as.logical(as.numeric(df$married) - 1)

## employed 
table(df$employed)
table(as.numeric(df$employed)) # no is level 1, yes is level 2
df$employed <- as.logical(as.numeric(df$employed) - 1)

## privins 
table(df$privins)
table(as.numeric(df$privins)) # no is level 1, yes is level 2
df$privins <- as.logical(as.numeric(df$privins) - 1)

## medicaid 
table(df$medicaid)
table(as.numeric(df$medicaid)) # no is level 1, yes is level 2
df$medicaid <- as.logical(as.numeric(df$medicaid) - 1)


