

# CHOICE EXPERIMENT R CODE: Models derived from the Native Woodland Creation Preferences Survey (NWCPS)


## Install necessary packages:

install.packages("mlogit")
install.packages("gmnl")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages ("stats")
install.packages("MASS")
install.packages("psych")
install.packages ("plotrix")


## To clear memory if needed

rm(list = ls())


## Load necessary packages

library(mlogit)
library(gmnl)
library(tidyverse)
library(ggpubr)
library(stats)
library(MASS)
library(psych)
library(plotrix)


#################################################################################################################################################################################################################################################################################################################################################################################

# DATA PREPARATIONS AND CHECKS


## Set working directory

setwd("C:/InsertPathToYourDirectoryHere")


## Load database

data_nwcps_raw<- read.table("C:/InsertPathToYourDatabaseHere") 

data<-data_nwcps_raw

data_full_obshetnum<-data%>%
  mutate(pbc_opportunity = case_when(pbc_opportunity %in% c('1') ~ "5",#For PBC opportunity I invert the scale since the question has a negative framing:  I feel that circumstances complicate the creation of woodland on the landholding I represent. Basically negative answers here mean a stronger perception of behavioural control.
                                     pbc_opportunity %in% c('2') ~ "4",
                                     pbc_opportunity %in% c('3') ~ "3",
                                     pbc_opportunity %in% c('4') ~ "2",
                                     pbc_opportunity %in% c('5') ~ "1"))%>% 
  mutate(id=as.numeric(id),
         choiceset=as.factor(choiceset),
         choiceID=as.factor(choiceID),
         choice=as.factor(choice),
         alternative=as.numeric(alternative),
         avi=as.numeric(avi),
         tc=as.numeric(tc),
         help=as.factor(help),
         lg=as.numeric(lg),
         optout=as.factor(optout),
         landhold_type=as.factor(landhold_type),
         land_size=as.ordered(land_size),
         orgtype=as.factor(orgtype),
         financeaims=as.factor(financeaims),
         orgsize=as.factor(orgsize),
         mgmtlength=as.factor(mgmtlength),
         newwood=as.factor(newwood),
         oldwood_combined=as.factor(oldwood_combined),
         socialnorminj1=as.numeric(socialnorminj1),
         persnormsoilwater=as.ordered(persnormsoilwater),
         pbc_opportunity=as.numeric(pbc_opportunity),
         persnormwildlife=as.numeric(persnormwildlife),
         socialnormdes1=as.ordered(socialnormdes1),
         persnormexpansion=as.ordered(persnormexpansion),
         socialnorminj2=as.ordered(socialnorminj2),
         persnormcc=as.ordered(persnormcc),
         intentions=as.numeric(intentions),
         pbc_ability=as.numeric(pbc_ability),
         socialnormdes2=as.ordered(socialnormdes2),
         natwoodhectares=as.numeric(natwoodhectares),
         education=as.factor(education),
         protest=as.factor(protest),
         adoption=as.factor(adoption))


##Inspecting data

str(data_full) #To inspect data is coded properly (i.e. factors are factors...)

summary(data_full)


##The four lines below give choice counts (i.e. cross tabulations of how many times levels were chosen, if the variables have a 0 level then subtract 255 to avoid counting times when optout was selected). Choice needs to be numeric for this to work.

xtabs(choice ~ avi , data = data_full_obshetnum) 
xtabs(choice ~ tc , data = data_full_obshetnum)
xtabs(choice ~ help , data_full_obshetnum)
xtabs(choice ~ lg , data_full_obshetnum)
xtabs(choice ~ optout , data_full_obshetnum)


##Transform data using the mlogit package so it can be used by the gmnl package

database_full_obshetnum<-mlogit.data(data_full_obshetnum, choice = "choice", shape = "long", alt.var = "alternative", id.var = "responseID", chid.var = "choiceset") 


##############################################################################################################################################################################################################################################################################################################################################################################


#RUNNING THE CHOICE MODELS 

## Run the Multinomial Logit model (AKA Conditional Logit or MNL) using the mlogit R package

nwcps_mnl_mlogit_final <- mlogit (choice ~ avi + tc + help + lg + optout | 0 | 0 | 0 | 0,
                                  data = database_full_obshetnum)

summary(nwcps_mnl_mlogit_final)

AIC(nwcps_mnl_mlogit_final)
BIC(nwcps_mnl_mlogit_final)


## Willingness To Accept (WTA) estimations for the Multinomial Logit model (AKA Conditional Logit or MNL. There are two ways of calculating this:

###One method writing the full WTA formula using the "coef" function: 

- coef(nwcps_mnl_mlogit_final)["tc"]/coef(nwcps_mnl_mlogit_final)["avi"]  # Willingness to Accept (WTA) for the "Minimum time to complete the woodland creation administrative requirements" attribute.
- coef(nwcps_mnl_mlogit_final)["help1"]/coef(nwcps_mnl_mlogit_final)["avi"]  # WTA for the "Availability of free advice on native woodland creation" attribute.
- coef(nwcps_mnl_mlogit_final)["lg"]/coef(nwcps_mnl_mlogit_final)["avi"]  # WTA for the "Percentage of all initial costs received as an interest free loan" attribute.


###The other is using the "wtp.gmnl" for automatic estimation. The "wtp.gmnl" method provides Willingness to Pay (WTP) coefficients, these must be multiplied by -1 to get WTA estimates, which are the opposite to WTP. An advantage of this automatic method is that it also produces standard errors for the estimates:

wtp.gmnl(nwcps_mnl_mlogit_final, wrt="avi") #Remember to multiply results by -1 to get WTA estimates instead of WTP estimates


## This line can be used to save the model as an rds file:

saveRDS(nwcps_mnl_mlogit_final, file="NWCPS_MNLmlogit_Final.rds")

## This line can be used to load the rds file, using the same as the original fit so code can be used without changes:

nwcps_mnl_mlogit_final<-readRDS(file="NWCPS_MNLmlogit_Final.rds")



###########################################################################################################################


## Run the Mixed Multinomial Logit models (AKA MIXL or MMNL) using the mlogit R package

nwcps_mixl_mlogit_final<-mlogit(choice ~ avi + tc + help + lg + optout  | 0 | 0 | 0 | 0, 
       database_full_obshetnum, 
       rpar = c(avi= 'ln', tc = 'n', lg= 'n', help1 = 'n', optout1='n'), 
       panel=TRUE, 
       halton = NULL, #We use pseudorandom draws instead of Halton draws. The model is more stable this way as confirmed when running it at 1000 or 2000 draws.
       R = 500)


summary(nwcps_mixl_mlogit_final)

AIC(nwcps_mixl_mlogit_final)
BIC(nwcps_mixl_mlogit_final)



## Willingness To Accept (WTA) estimations Mixed Multinomial Logit models (AKA MIXL or MMNL) 

###The estimation of WTA is a bit trickier in this case, requiring simulation since the parameters are distributions. Additionally, when distributions other than normal, such as log-normal or truncated (at zero) normal, are used for random parameters in mlogit MIXL models, the resulting parameter estimates are not the mean and sd of the distributions used. For normal related distributions two parameters are estimated: the mean and the standard-deviation of the underlying normal distribution. While for the uniform and triangular distribution, the mean and the half range of the distribution is estimated (Source: https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html). This is something to account for in WTP and WTA estimations. We produced our WTA estimates adapting the approach outlined here: 

set.seed(123456)

N = 10^6#This defines the number of random draws taken from a particular distribution (normal, lognormal...)
b_avi = exp(rnorm(N, mean=coef(nwcps_mixl_mlogit_final)["avi"], sd=coef(nwcps_mixl_mlogit_final)["sd.avi"]))#This simulates the lognormal distribution of the compensation attribute, in our case our "Expected average net income" attribute. Our lognormal distribution is positive, thus why we removed the negative sign preceding "exp" in the source material.

#b_avi can also be created as follows: b_avi = rlnorm(N,meanlog=coef(nwcps_mixl_mlogit_final)["avi"], sdlog=coef(nwcps_mixl_mlogit_final)["sd.avi"])


b_tc = rnorm(N, mean=coef(nwcps_mixl_mlogit_final)["tc"], sd=coef(nwcps_mixl_mlogit_final)["sd.tc"]) #This simulates the normal distribution of the parameter of interest.
wta_tc = -1 *(b_tc/b_avi)  #This is the WTA formula, which produces the distribution of WTA.
mean(wta_tc) # This is the mean of the WTA distribution.
sd(wta_tc) # This is the standard deviation of the WTA distribution.
describe(wta_tc) # This describes the WTA distribution (mean, sd, median, range...)


b_help1 = rnorm(N, mean=coef(nwcps_mixl_mlogit_final)["help1"], sd=-coef(nwcps_mixl_mlogit_final)["sd.help1"]) #This simulates the normal distribution of the parameter of interest. We make the negative of the standard deviation (sd) coefficient to address a bug in the R package's algorithm that sometimes results in negative sd estimates(Source: https://stats.stackexchange.com/questions/158260/mlogit-negative-value-for-sd).
wta_help1 = -1 *(b_help1/b_avi)  #This is the WTA formula, which produces the distribution of WTAs.
mean(wta_help1) # This is the mean of the WTA distribution.
sd(wta_help1) # This is the standard deviation of the WTA distribution.
describe(wta_help1) # This describes the WTA distribution (mean, sd, median, range...)


b_lg = rnorm(N, mean=coef(nwcps_mixl_mlogit_final)["lg"], sd=coef(nwcps_mixl_mlogit_final)["sd.lg"]) #This simulates the normal distribution of the parameter of interest.
wta_lg = -1 *(b_lg/b_avi)  #This is the WTA formula, which produces the distribution of WTAs.
mean(wta_lg) # This is the mean of the WTA distribution.
sd(wta_lg) # This is the standard deviation of the WTA distribution.
describe(wta_lg) # This describes the WTA distribution (mean, sd, median, range...)


b_optout1 = rnorm(N, mean=coef(nwcps_mixl_mlogit_final)["optout1"], sd=coef(nwcps_mixl_mlogit_final)["sd.optout1"]) #This simulates the normal distribution of the parameter of interest.
wta_optout1 = -1 *(b_optout1/b_avi)  #This is the WTA formula, which produces the distribution of WTAs.
mean(wta_optout1) # This is the mean of the WTA distribution.
sd(wta_optout1) # This is the standard deviation of the WTA distribution.
describe(wta_optout1) # This describes the WTA distribution (mean, sd, median, range...)


###SE formula for the WTA distributions can be estimated using the following formula if needed:

sd(wta_lg)/sqrt(length(wta_lg)) 

                
                
## This line can be used to save the model as an rds file:

saveRDS(nwcps_mixl_mlogit_final, file="NWCPS_MIXLmlogit_Final.rds")

## This line can be used to load the rds file, using the same as the original fit so code can be used without changes:

nwcps_mixl_mlogit_final<-readRDS(file="NWCPS_MIXLmlogit_Final.rds")


## If needed the modeled attribute distributions can be plotted as follows:

plot(rpar(nwcps_mixl_mlogit_final, 'avi'))
plot(rpar(nwcps_mixl_mlogit_final, 'tc'))
plot(rpar(nwcps_mixl_mlogit_final, 'help1'))
plot(rpar(nwcps_mixl_mlogit_final, 'lg'))



###########################################################################################################################


##Run the Latent Class Multinomial Logit models (AKA LCMNL) using the gmnl R package


##LCMNL with two classes and no observed heterogeneity

nwcps_lcm2_final <- gmnl(choice ~ avi + tc + help + lg + optout | 0 | 0 | 0 | 1,
                          data = database_full_obshetnum,
                          model = 'lc',
                          Q = 2,
                          panel = TRUE,           
                          method = "bfgs")        

summary(nwcps_lcm2_final) 

AIC(nwcps_lcm2_final)
BIC(nwcps_lcm2_final)



## Willingness To Accept (WTA) estimations for the Latent Class Multinomial Logit models (AKA LCMNL). There are two ways of calculating this:

###One method writing the full WTA formula using the "coef" function: 


- coef(nwcps_lcm2_final)["class.1.tc"]/coef(nwcps_lcm2_final)["class.1.avi"]
- coef(nwcps_lcm2_final)["class.1.help1"]/coef(nwcps_lcm2_final)["class.1.avi"]
- coef(nwcps_lcm2_final)["class.1.lg"]/coef(nwcps_lcm2_final)["class.1.avi"]

- coef(nwcps_lcm2_final)["class.2.tc"]/coef(nwcps_lcm2_final)["class.2.avi"]
- coef(nwcps_lcm2_final)["class.2.help1"]/coef(nwcps_lcm2_final)["class.2.avi"]
- coef(nwcps_lcm2_final)["class.2.lg"]/coef(nwcps_lcm2_final)["class.2.avi"]


###The other is using the "wtp.gmnl" for automatic estimation. The "wtp.gmnl" method provides Willingness to Pay (WTP) coefficients, these must be multiplied by -1 to get WTA estimates, which are the opposite to WTP. An advantage of this automatic method is that it also produces standard errors for the estimates:

wtp.gmnl(nwcps_lcm2_final, wrt="class.1.avi") # This code produces the same WTA estimates for class 1. Remember to multiply results by -1 to get WTA estimates instead of WTP estimates

wtp.gmnl(nwcps_lcm2_final, wrt="class.2.avi") # This code produces the same WTA estimates for class 2.Remember to multiply results by -1 to get WTA estimates instead of WTP estimates



## Class shares estimations (Source: https://rpubs.com/msarrias1986/335556):

1 - exp(coef(nwcps_lcm2_final)["(class)2"]) / (exp(0) + exp(coef(nwcps_lcm2_final)["(class)2"])) # This is the formula for the class share of class 1. It is basically one minus the share of class 2.

exp(coef(nwcps_lcm2_final)["(class)2"]) / (exp(0) + exp(coef(nwcps_lcm2_final)["(class)2"])) # This is the formula for the class share of class 2.


## This line can be used to save the model as an rds file:

saveRDS(nwcps_lcm2_final, file="NWCPS_LCM2gmnl_Final.rds")

## This line can be used to load the rds file, using the same as the original fit so code can be used without changes:

nwcps_lcm2_final<-readRDS(file="NWCPS_LCM2gmnl_Final.rds")





## LCMNL with two classes and observed heterogeneity
 
nwcps_lcm2_observedheterogeneity_final <- gmnl(choice ~ avi + tc + help + lg + optout | 0 | 0 | 0 | intentions + pbc_opportunity + pbc_ability,  
                                 data = database_full_obshetnum,
                                 model = 'lc',
                                 Q = 2,
                                 panel = TRUE,           
                                 method = "bfgs")        


summary(nwcps_lcm2_observedheterogeneity_final) 

AIC(nwcps_lcm2_observedheterogeneity_final)
BIC(nwcps_lcm2_observedheterogeneity_final)


## Willingness To Accept (WTA) estimations for the Latent Class Multinomial Logit models (AKA LCMNL). There are two ways of calculating this:

###One method writing the full WTA formula using the "coef" function: 

- coef(nwcps_lcm2_observedheterogeneity_final)["class.1.tc"]/coef(nwcps_lcm2_observedheterogeneity_final)["class.1.avi"]
- coef(nwcps_lcm2_observedheterogeneity_final)["class.1.help1"]/coef(nwcps_lcm2_observedheterogeneity_final)["class.1.avi"]
- coef(nwcps_lcm2_observedheterogeneity_final)["class.1.lg"]/coef(nwcps_lcm2_observedheterogeneity_final)["class.1.avi"]

- coef(nwcps_lcm2_observedheterogeneity_final)["class.2.tc"]/coef(nwcps_lcm2_observedheterogeneity_final)["class.2.avi"]
- coef(nwcps_lcm2_observedheterogeneity_final)["class.2.help1"]/coef(nwcps_lcm2_observedheterogeneity_final)["class.2.avi"]
- coef(nwcps_lcm2_observedheterogeneity_final)["class.2.lg"]/coef(nwcps_lcm2_observedheterogeneity_final)["class.2.avi"]


###The other is using the "wtp.gmnl" for automatic estimation. The "wtp.gmnl" method provides Willingness to Pay (WTP) coefficients, these must be multiplied by -1 to get WTA estimates, which are the opposite to WTP. An advantage of this automatic method is that it also produces standard errors for the estimates:

wtp.gmnl(nwcps_lcm2_final, wrt="class.1.avi") # This code produces WTA estimates for class 1.Remember to multiply results by -1 to get WTA estimates instead of WTP estimates

wtp.gmnl(nwcps_lcm2_final, wrt="class.2.avi") # This code produces WTA estimates for class 2.Remember to multiply results by -1 to get WTA estimates instead of WTP estimates



## Class shares estimations (Source: https://rpubs.com/msarrias1986/335556):

1- exp(coef(nwcps_lcm2_observedheterogeneity_final)["(class)2"]) / (exp(0) + exp(coef(nwcps_lcm2_observedheterogeneity_final)["(class)2"])) # This is the formula for the class share of class 1. It is basically one minus the share of class 2.

exp(coef(nwcps_lcm2_observedheterogeneity_final)["(class)2"]) / (exp(0) + exp(coef(nwcps_lcm2_observedheterogeneity_final)["(class)2"]))# This is the formula for the class share of class 2.


#This line can be used to save the model as an rds file:

saveRDS(nwcps_lcm2_observedheterogeneity_final, file="NWCPS_LCM2observedheterogeneity_gmnl_Final.rds")

#This line can be used to load the rds file, using the same as the original fit so code can be used without changes:

nwcps_lcm2_observedheterogeneity_final<-readRDS(file="NWCPS_LCM2observedheterogeneity_gmnl_Final.rds")


#THE END