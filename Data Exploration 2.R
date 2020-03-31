library(ggplot2)
library(MASS)
library(pscl)
library(dplyr)
library(bbmle)

load('DebTrivedi')
df <- DebTrivedi
remove(DebTrivedi)

set.seed(1337)

######################################
############ Data Exploration ########
######################################
head(df)
str(df)

mean(df$ofp) # mean about 5.8
var(df$ofp)  # var about 45.68
## overdispersion is a problem with this response


####################### VARIABLE TRANSFORMATION ########################
##### Change the two-factor variables to boolean
## Factors can be releveled and cause confusion if baseline factor is not intuitive

## adldiff
# table(df$adldiff)
# table(as.numeric(df$adldiff)) #no is level 1, yes is level 2
df$adldiff <- as.logical(as.numeric(df$adldiff) - 1)

## black
# table(df$black)
# table(as.numeric(df$black)) # no is level 1, yes is level 2
df$black <- as.logical(as.numeric(df$black) - 1)

## gender (true = male, name change to male instead of gender)
# table(df$gender)
# table(as.numeric(df$gender)) # female is level 1, male is level 2
df$gender <- as.logical(as.numeric(df$gender) - 1)
# change name of gender to male
names(df)[names(df)=='gender'] <- 'male'

## married
# table(df$married)
# table(as.numeric(df$married)) # no is level 1, yes is level 2
df$married <- as.logical(as.numeric(df$married) - 1)

## employed 
# table(df$employed)
# table(as.numeric(df$employed)) # no is level 1, yes is level 2
df$employed <- as.logical(as.numeric(df$employed) - 1)

## privins 
# table(df$privins)
# table(as.numeric(df$privins)) # no is level 1, yes is level 2
df$privins <- as.logical(as.numeric(df$privins) - 1)

## medicaid 
# table(df$medicaid)
# table(as.numeric(df$medicaid)) # no is level 1, yes is level 2
df$medicaid <- as.logical(as.numeric(df$medicaid) - 1)






################################################
############ Exploratory Stuff ################
###############################################

# x is a vector of data. Can supply labda or use default = mean(x)
dataplot2 <- function(x, lambda = mean(x)){
  poigen <- rpois(length(df$numchron), lambda = lambda)
  barplot(table(x), xlab = "Number of Chronic Diseases", 
          ylab = "Frequency", xlim = c(0, ifelse(length(unique(poigen))>=length(unique(x)), 
                                                 length(unique(poigen))+2, min(length(unique(x))+2, 25))))
  if (length(unique(x))>=length(unique(poigen))){
    barplot(table(poigen), width=0.5, space=c(0.9, rep(1.4, length(unique(poigen))-1)), 
            col="red", add=TRUE, names.arg = "")
  } else {
    barplot(table(poigen), width=0.5, space=c(0.9, rep(1.4, length(unique(poigen))-1)), 
            col="red", add=TRUE, names.arg = c(rep("", length(unique(x))), (length(unique(x)) + 1):length(unique(poigen))))
  }
  legend(13, max(max(table(x)), max(table(poigen))) - 150, legend=c("Data", expression(paste("Poisson Dist., mean = ", lambda))),
         col=c("grey", "red"), pch= 15, cex = 0.8)
}

dataplot2(df$ofp)




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

var1 <- c('hosp', 'health', 'numchron', 'male', 'school')
var2 <- c('age', 'privins', 'medicaid') #These three variables are the most significant in the 3 models below

f1 <- as.formula(paste('ofp ~', paste(var1, collapse = ' + ')))
f2 <- as.formula(paste('ofp ~', paste(c(var1, var2),  collapse = ' + ')))


######## Step 1: Compare NB to Zero-Inflated NB using the base predictor variables #########

nb1 <- glm.nb(f1, data = df)
summary(nb1)

zero1 <- zeroinfl(f1, data = df, dist = 'negbin')
summary(zero1)

#Compare the zero-inflated to regular NB model
AIC(nb1)
AIC(zero1)



# It looks like the Zero-Inflated model is slightly better, although AIC is not recommended for over dispersed count data (and is probably kind
# of a crappy measure in general)
# I want to find some justification for exploring adding variables to the NB model, since diagnostics are easier on it (A lot of tests we
# used in class don't work on Zero-inflated models')
# Currently I'm looking into different measures for comparing the two models, and finding justification for only exploring the NB

######## Experiment with adding parameters to the NB model #######
#### Add Age ####
nb2 <- update(nb1, . ~ . + age)

#Wald Test
summary(nb2)
abs(coef(nb2)[8] / sqrt(summary(nb2)$cov.unscaled[8,8])) > 1.96 #Std Error is diff than in the summary. Is that due to Wald test using Normal dist?

#LR Test
(deviance(nb2) - deviance(nb1)) > qchisq(0.95, 1)

# ANOVA
anova(nb2, nb1) #How to interpret this?

#Age fails to be significant in both LR and Wald Tests


#### Add Privins ####
nb3 <- update(nb1, . ~ . + privins)

#Wald Test
summary(nb3)
abs(coef(nb3)[8] / sqrt(summary(nb3)$cov.unscaled[8,8])) > 1.96 #Std Error is diff than in the summary. Is that due to Wald test using Normal dist?

#LR Test
(deviance(nb2) - deviance(nb1)) > qchisq(0.95, 1)
#There are three tests 
#Deviance test of additional parameters
(deviance(m_nb1) - deviance(m_nb2)) > qchisq(0.95, 3)



#Comparing adding the three additional parameters
anova(m_nb1, m_nb2)
anova(m_zero_nb1, m_zero_nb2) #ANOVA doesn't work for this type of model. See LR-test below for a suitable replacement


# Likelihood ratio test (I think)
# https://stats.idre.ucla.edu/r/dae/zip/
pchisq(2 * (logLik(m_zero2) - logLik(m_zero1)), df = 3, lower.tail = FALSE) 
pchisq(2 * (logLik(m_zero1) - logLik(m_zero0)), df = 5, lower.tail = FALSE)









