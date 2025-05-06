
# ELASTIC NET REGRESSION R CODE: Analysis of the Drivers of Native Woodland Creation Survey (DNWCS) using Elastic Net Regression to select variables and a Binomial Logit Regressions.


##Installations
install.packages("glmnet")
install.packages("tidyverse")
install.packages("sjPlot")
install.packages("caret")
install.packages("corrr")
install.packages("car")
install.packages("correlation")




#Load packages
library(glmnet)  
library(tidyverse)
library(sjPlot)
library(caret)
library(corrr) 
library(car) 
library(correlation)

##Set working directory

setwd("C:/InsertPathToYourDirectoryHere")


##Load database

###data_dnwcs_raw is the original database, we don't wrangle on this, instead we create a copy of it data_dnwcs, where we wrangle.

data_dnwcs_raw <- read_csv("C:/InsertPathToYourDirectoryHere")

data_dnwcs<-data_dnwcs_raw

doitvars<-data_dnwcs%>%
  dplyr::select(col160, 
                #Innovation characteristics
                
                ##Relative advantage
                col92, #doit_inicost_money (ORDINAL  - Blanks 0) 
                col93, #doit_inicost_time (ORDINAL  - Blanks 0)
                col113 , #doit_envben_carbon (ORDINAL - Blanks 5)
                col114 , #doit_envben_habitat (ORDINAL - Blanks 2)
                col115 , #doit_envben_biodiversity (ORDINAL - Blanks 1)
                col116 , #doit_envben_waterquality (ORDINAL - Blanks 3)
                col117 , #doit_envben_flooding (ORDINAL - Blanks 2)
                col118 , #doit_envben_soilqual (ORDINAL - Blanks 4)
                col119 , #doit_econbenefits (ORDINAL - Blanks 1) 
                col120 , #doit_socialben_commwellbeing (ORDINAL - Blanks 1)  
                col121 , #doit_socrecognition (ORDINAL - Blanks 1)
                col122 , #doit_envben_jobs (ORDINAL - Blanks 1) 
                col97 , #doit_perceivedrisk (ORDINAL - Blanks 0)
                
                ##Compatibility
                col137 , #doit_valuesandbeliefs (ORDINAL - Blanks 0)
                col138 , #doit_needs (ORDINAL - Blanks 0)
                col139 , #doit_currentpractices (ORDINAL - Blanks 1)
                
                ##Complexity
                col105 , #doit_understandgroundprep (ORDINAL - Blanks 4)
                col106 , #doit_understandtreeprotect (ORDINAL - Blanks 4)
                col107 , #doit_understandplanting (ORDINAL - Blanks 4)
                col110 , #doit_dogroundprep (ORDINAL - Blanks 4)
                col111 , #doit_dotreeprotect (ORDINAL - Blanks 4)
                col112 , #doit_doplanting (ORDINAL - Blanks 5)
                
                ##Trialability
                col91 , #doit_reversibility (ORDINAL - Blanks 0)
                
                ##Observability
                col135 , #doit_vispract (ORDINAL - Blanks 0)
                col136 , #doit_visresults (ORDINAL - Blanks 0)
                
                ##Flexibility
                col90 , #doit_modification (ORDINAL - Blanks 0)
                
                
                #Adopter characteristics
                
                ##Social-economic
                col55 , #doit_economicwellbeing (ORDINAL - Blanks 1)
                col154 , #doit_education (ORDINAL - Blanks 3) 
                col94 , #doit_reslack_money (ORDINAL - Blanks 0) 
                col95 , #doit_reslack_time (ORDINAL - Blanks 0) 
                col12 , #land_size (ORDINAL - Other 14, Blanks 1) 
                col17 , #orgsize (ORDINAL - Blanks 0) 
                
                ##Personality
                col99 , #doit_riskorientation (ORDINAL - Blanks 0) 
                col150 , #doit_competition (ORDINAL - Blanks 0) 
                
                ##Knowledge
                col98 , #doit_familiarity (ORDINAL - Blanks 0) 
                col142 , #doit_interconnect (BINARY - Blanks 0)
                col96 , #doit_complexity (ORDINAL - Blanks 0) 
                
                ##Decision-making
                col53 , #doit_innovdecision (FACTOR 3 LEVELS - Blanks 4) 
                col101 , #doit_formalization (ORDINAL - Blanks 0) 
                
                
                #Context characteristics
                
                ##Geographical settings
                col86 , #doit_ecoconds_temp (ORDINAL - Blanks 0)
                col87 , #doit_ecoconds_soil (ORDINAL - Blanks 0)
                col88 , #doit_ecoconds_wind (ORDINAL - Blanks 0)
                col89 , #doit_ecoconds_herb (ORDINAL - Blanks 0)
                
                ##Culture
                col144 , #doit_tradition (ORDINAL - Blanks 0) 
                
                ##Political conditions - Not addressed in the survey
                
                ##Extension support
                col145 , #doit_champs (ORDINAL - Blanks 0) 
                col146)%>% #doit_support (ORDINAL - Blanks 0)
  
                ##Global discourse- Not addressed in the survey
  
  dplyr::mutate(col53 = na_if(col53, 4),#Removes "Do not know" from analysis
                col53 = na_if(col53, 5), #Removes "Other, please specify" from analysis
                col154= na_if(col154,7), #Removes "Refuse to answer" from analysis. 
                singledecision= col53==1, #New column for decisions made by a single individual
                alldecision= col53==2, #New column for decisions made by all members of an organisation
                groupdecision= col53==3, #New column for decisions made by a select group within an organisation
                singledecision= if_else(singledecision == TRUE, 1, 0), #Recode column so if value is "TRUE" it becomes 1 if not 0.
                alldecision= if_else(alldecision == TRUE, 1, 0),#Recode column so if value is "TRUE" it becomes 1 if not 0.
                groupdecision= if_else(groupdecision == TRUE, 1, 0),#Recode column so if value is "TRUE" it becomes 1 if not 0.
                col160=as.factor(col160), #Recode column as a factor
                col142=as.factor(col142), #Recode column as a factor
                singledecision=as.factor(singledecision), #Recode column as a factor
                alldecision=as.factor(alldecision), #Recode column as a factor
                groupdecision=as.factor(groupdecision))%>% #Recode column as a factor
  
  dplyr::rename (adoption=col160, 
                 
                 #Innovation characteristics
                 
                 ##Relative advantage
                 inicost_money=col92 , #(ORDINAL  - Blanks 0) 
                 inicost_time=col93 , #(ORDINAL  - Blanks 0)
                 envben_carbon=col113, #(ORDINAL - Blanks 5)
                 envben_habitat=col114, #(ORDINAL - Blanks 2)
                 envben_biodiversity=col115, #(ORDINAL - Blanks 1)
                 envben_waterquality=col116,#(ORDINAL - Blanks 3)
                 envben_flooding=col117, #(ORDINAL - Blanks 2)
                 envben_soilqual=col118, #(ORDINAL - Blanks 4)
                 econbenefits=col119, #(ORDINAL - Blanks 1)
                 socialben_commwellbeing=col120, #(ORDINAL - Blanks 1)
                 socrecognition=col121, # (ORDINAL - Blanks 1)
                 socben_jobs=col122, #(ORDINAL - Blanks 1)
                 perceivedrisk=col97, #(ORDINAL - Blanks 0)
                 
                 ##Compatibility
                 comp_vb=col137, #(ORDINAL - Blanks 0)
                 comp_needs=col138, #(ORDINAL - Blanks 0)
                 comp_pract=col139, #(ORDINAL - Blanks 1)
                 
                 ##Complexity
                 easeundgrndprep=col105, #(ORDINAL - Blanks 4)
                 easeundtreeprtct=col106, #(ORDINAL - Blanks 4)
                 easeundplant=col107, #(ORDINAL - Blanks 4)
                 easedogroundprep=col110, #(ORDINAL - Blanks 4)
                 easedotreeprotect=col111, #(ORDINAL - Blanks 4)
                 easedoplanting=col112, #(ORDINAL - Blanks 5)
                 
                 ##Trialability
                 revers=col91, #(ORDINAL - Blanks 0)
                 
                 ##Observability
                 vispract=col135, #(ORDINAL - Blanks 0)
                 visresults=col136, #(ORDINAL - Blanks 0)
                 
                 ##Flexibility
                 modif=col90, #(ORDINAL - Blanks 0)
                 
                 
                 #Adopter characteristics
                 
                 ##Social-economic
                 econwellb=col55, #(ORDINAL - Blanks 1)
                 edu=col154, #(ORDINAL - Blanks 3) - Needs to be inverted, since the lowest level is now doctoral. This is done below.
                 reslack_money=col94, #(ORDINAL - Blanks 0)
                 reslack_time=col95, #(ORDINAL - Blanks 0)
                 landsize=col12, #(ORDINAL - Other 14= Blanks 1)
                 orgsize=col17, #(ORDINAL - Blanks 0)
                 
                 ##Personality
                 riskavers=col99, #(ORDINAL - Blanks 0) - Check this, either need to speak in terms of risk aversion “We will wait until we are comfortable with the risks posed by woodland creation before implementing it” or invert it. Relabeled it as risk aversion from the original risk orientation.
                 percvcomp=col150, #(ORDINAL - Blanks 0)
                 
                 ##Knowledge
                 fam=col98, #(ORDINAL - Blanks 0) - Needs to be inverted before analysis. The statement here is " We will wait until we have enough information about woodland creation and its consequences before implementing it." hence a "Strongly agree" (i.e. 5) indicates less familiarity. The inversion is done below.
                 interconnect=col142, #(BINARY - Blanks 0)
                 relvntknowskills=col96, #(ORDINAL - Blanks 0) - Needs to be inverted or spoken about differently. The statement is "We have the knowledge and skills needed to create new woodlands" so basically more agreement shows the innovation is perceived as less complex. Relabeled it as perception of having relevant skills and knowledge.
                 
                 ##Decision-making
                 innovdecision=col53, #(FACTOR 3 LEVELS - Blanks 4) - Split this into 3 binary columns. This is done above, by creating the three binary columns: singledecision, alldecision, and groupdecision.
                 formalization=col101, #(ORDINAL - Blanks 0) 
                 
                 
                 #Context characteristics
                 
                 ##Geographical settings
                 ecoconds_temp=col86, #(ORDINAL - Blanks 0) 
                 ecoconds_soil=col87, #(ORDINAL - Blanks 0)
                 ecoconds_wind=col88, #(ORDINAL - Blanks 0)
                 ecoconds_herb=col89, #(ORDINAL - Blanks 0)
                 
                 ##Culture
                 opennewideas=col144, #(ORDINAL - Blanks 0) - Need to invert or talk about it differently. The statement is "The people in the landholding you represent (including you) are open to new ideas and practices about land management." so higher scores means less traditional.  Relabeled it as openness to new ideas from the original traditionalism.
                 
                 ##Political conditions - Not addressed in the survey
                 
                 ##Extension support
                 
                 champs=col145, #(ORDINAL - Blanks 0)
                 support=col146)%>% #(ORDINAL - bLANKS 0)
  dplyr::mutate(fam= case_when(fam %in% c('1') ~ "5", #This performs the necessary inversion described above.
                               fam %in% c('2') ~ "4",
                               fam %in% c('3') ~ "3",
                               fam %in% c('4') ~ "2",
                               fam %in% c('5') ~ "1"),
                fam =as.numeric(fam))%>% 
  dplyr::mutate(edu= case_when(edu %in% c('1') ~ "6", #This performs the necessary inversion described above.
                               edu %in% c('2') ~ "5",
                               edu %in% c('3') ~ "4",
                               edu %in% c('4') ~ "3",
                               edu %in% c('5') ~ "2",
                               edu %in% c('6') ~ "1"),
                edu =as.numeric(edu))




####################################################################################################################################################################################################################################################

#Variable selection through Elastic Net Regression code


##First create the needed databases

data_nonas<-doitvars%>%      #Load database
  select(-c(innovdecision))%>% # Remove two variables that cause problems with the variables selection functions.
  na.omit()                 #Remove rows with missing values


y<- data_nonas$adoption%>%  #Isolate the outcome variable
  as.matrix()               #Transform the resulting database into a matrix

X<-subset(data_nonas, select=-c(adoption))%>%  #Create database containing DOIT predictors by removing the outcome variable
  as.matrix() 


## Elastic net regression modulates the strength of the lasso and ridge penalties through the elastic net penalty parameter (α), which ranges from 0 (ridge regression) to 1 (lasso regression). The strength of the applied penalization is further controlled by a tuning parameter (λ).

##The code below tests a set of αs ranging between 0 and 1 in 0.1 increments, to determine our optimal model. We use cross-validation to find the λ that minimises binomial deviance – a measure of model fit for logistic regressions – for each α. We then compare the results for each α, considering the model with the smallest overall binomial deviance as the “best”, and extracting its variables. 


##The code below runs the cross-validation using αs ranging between 0 and 1 in 0.1 increments.

set.seed(12345)
cv0_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0)
cv0.1_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.1)
cv0.2_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.2)
cv0.3_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.3)
cv0.4_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.4)
cv0.5_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
cv0.6_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.6)
cv0.7_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.7)
cv0.8_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.8)
cv0.9_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 0.9)
cv1_seed1<- cv.glmnet(X, y, family = "binomial", alpha = 1)


##The code below plots the cross-validation results.

plot(log(cv1_seed1$lambda), cv1_seed1$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1_seed1$name, ylim=c(1,2), xlim=c(-4,-1), main="Seed = 12345")
points(log(cv0.9_seed1$lambda), cv0.9_seed1$cvm, pch = 19, col = "grey")
points(log(cv0.8_seed1$lambda), cv0.8_seed1$cvm, pch = 19, col = "green")
points(log(cv0.7_seed1$lambda), cv0.7_seed1$cvm, pch = 19, col = "violet")
points(log(cv0.6_seed1$lambda), cv0.6_seed1$cvm, pch = 19, col = "darkgreen")
points(log(cv0.5_seed1$lambda), cv0.5_seed1$cvm, pch = 19, col = "yellow")
points(log(cv0.4_seed1$lambda), cv0.4_seed1$cvm, pch = 19, col = "orange")
points(log(cv0.3_seed1$lambda), cv0.3_seed1$cvm, pch = 19, col = "pink")
points(log(cv0.2_seed1$lambda), cv0.2_seed1$cvm, pch = 19, col = "black")
points(log(cv0.1_seed1$lambda), cv0.1_seed1$cvm, pch = 19, col = "brown")
points(log(cv0_seed1$lambda) , cv0_seed1$cvm , pch = 19, col = "blue")
legend("topright", legend = c("a1","a0.9",  "a0.8", "a0.7", "a0.6", "a0.5","a0.4","a0.3","a0.2","a0.1", "a0"),
       pch = 19, col = c("red","grey", "green","violet","darkgreen","yellow", "orange","pink","black","brown", "blue"))


##The code below allows us to identify the α producing the smallest binomial deviance.

sort(cv0_seed1$cvm, decreasing=FALSE)
sort(cv0.1_seed1$cvm, decreasing=FALSE)
sort(cv0.2_seed1$cvm, decreasing=FALSE)
sort(cv0.3_seed1$cvm, decreasing=FALSE)
sort(cv0.4_seed1$cvm, decreasing=FALSE)
sort(cv0.5_seed1$cvm, decreasing=FALSE)
sort(cv0.6_seed1$cvm, decreasing=FALSE)
sort(cv0.7_seed1$cvm, decreasing=FALSE)
sort(cv0.8_seed1$cvm, decreasing=FALSE)
sort(cv0.9_seed1$cvm, decreasing=FALSE)
sort(cv1_seed1$cvm, decreasing=FALSE)


##The code below identifies the λ producing model with the smallest binomial deviance for a particular  α.

cv0_seed1$lambda.min
cv0.1_seed1$lambda.min
cv0.2_seed1$lambda.min
cv0.3_seed1$lambda.min
cv0.4_seed1$lambda.min
cv0.5_seed1$lambda.min
cv0.6_seed1$lambda.min
cv0.7_seed1$lambda.min
cv0.8_seed1$lambda.min
cv0.9_seed1$lambda.min
cv1_seed1$lambda.min


#The code below repeats the process but giving the algorithm a different seed.

set.seed(56789)
cv0_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0)
cv0.1_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.1)
cv0.2_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.2)
cv0.3_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.3)
cv0.4_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.4)
cv0.5_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
cv0.6_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.6)
cv0.7_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.7)
cv0.8_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.8)
cv0.9_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 0.9)
cv1_seed2<- cv.glmnet(X, y, family = "binomial", alpha = 1)


plot(log(cv1_seed2$lambda), cv1_seed2$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1_seed2$name, ylim=c(1,2), xlim=c(-4,-1), main="Seed = 56789")
points(log(cv0.9_seed2$lambda), cv0.9_seed2$cvm, pch = 19, col = "grey")
points(log(cv0.8_seed2$lambda), cv0.8_seed2$cvm, pch = 19, col = "green")
points(log(cv0.7_seed2$lambda), cv0.7_seed2$cvm, pch = 19, col = "violet")
points(log(cv0.6_seed2$lambda), cv0.6_seed2$cvm, pch = 19, col = "darkgreen")
points(log(cv0.5_seed2$lambda), cv0.5_seed2$cvm, pch = 19, col = "yellow")
points(log(cv0.4_seed2$lambda), cv0.4_seed2$cvm, pch = 19, col = "orange")
points(log(cv0.3_seed2$lambda), cv0.3_seed2$cvm, pch = 19, col = "pink")
points(log(cv0.2_seed2$lambda), cv0.2_seed2$cvm, pch = 19, col = "black")
points(log(cv0.1_seed2$lambda), cv0.1_seed2$cvm, pch = 19, col = "brown")
points(log(cv0_seed2$lambda) , cv0_seed2$cvm , pch = 19, col = "blue")
legend("topright", legend = c("a1","a0.9",  "a0.8", "a0.7", "a0.6", "a0.5","a0.4","a0.3","a0.2","a0.1", "a0"),
       pch = 19, col = c("red","grey", "green","violet","darkgreen","yellow", "orange","pink","black","brown", "blue"))



sort(cv0_seed2$cvm, decreasing=FALSE)
sort(cv0.1_seed2$cvm, decreasing=FALSE)
sort(cv0.2_seed2$cvm, decreasing=FALSE)
sort(cv0.3_seed2$cvm, decreasing=FALSE)
sort(cv0.4_seed2$cvm, decreasing=FALSE)
sort(cv0.5_seed2$cvm, decreasing=FALSE)
sort(cv0.6_seed2$cvm, decreasing=FALSE)
sort(cv0.7_seed2$cvm, decreasing=FALSE)
sort(cv0.8_seed2$cvm, decreasing=FALSE)
sort(cv0.9_seed2$cvm, decreasing=FALSE)
sort(cv1_seed2$cvm, decreasing=FALSE)


cv0_seed2$lambda.min
cv0.1_seed2$lambda.min
cv0.2_seed2$lambda.min
cv0.3_seed2$lambda.min
cv0.4_seed2$lambda.min
cv0.5_seed2$lambda.min
cv0.6_seed2$lambda.min
cv0.7_seed2$lambda.min
cv0.8_seed2$lambda.min
cv0.9_seed2$lambda.min
cv1_seed2$lambda.min


#The code below repeats the process but giving the algorithm a different seed.

set.seed(101112)
cv0_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0)
cv0.1_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.1)
cv0.2_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.2)
cv0.3_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.3)
cv0.4_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.4)
cv0.5_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
cv0.6_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.6)
cv0.7_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.7)
cv0.8_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.8)
cv0.9_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 0.9)
cv1_seed3<- cv.glmnet(X, y, family = "binomial", alpha = 1)


plot(log(cv1_seed3$lambda), cv1_seed3$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1_seed3$name, ylim=c(1,2), xlim=c(-4,-1), main="Seed = 101112")
points(log(cv0.9_seed3$lambda), cv0.9_seed3$cvm, pch = 19, col = "grey")
points(log(cv0.8_seed3$lambda), cv0.8_seed3$cvm, pch = 19, col = "green")
points(log(cv0.7_seed3$lambda), cv0.7_seed3$cvm, pch = 19, col = "violet")
points(log(cv0.6_seed3$lambda), cv0.6_seed3$cvm, pch = 19, col = "darkgreen")
points(log(cv0.5_seed3$lambda), cv0.5_seed3$cvm, pch = 19, col = "yellow")
points(log(cv0.4_seed3$lambda), cv0.4_seed3$cvm, pch = 19, col = "orange")
points(log(cv0.3_seed3$lambda), cv0.3_seed3$cvm, pch = 19, col = "pink")
points(log(cv0.2_seed3$lambda), cv0.2_seed3$cvm, pch = 19, col = "black")
points(log(cv0.1_seed3$lambda), cv0.1_seed3$cvm, pch = 19, col = "brown")
points(log(cv0_seed3$lambda) , cv0_seed3$cvm , pch = 19, col = "blue")
legend("topright", legend = c("a1","a0.9",  "a0.8", "a0.7", "a0.6", "a0.5","a0.4","a0.3","a0.2","a0.1", "a0"),
       pch = 19, col = c("red","grey", "green","violet","darkgreen","yellow", "orange","pink","black","brown", "blue"))

sort(cv0_seed3$cvm, decreasing=FALSE)
sort(cv0.1_seed3$cvm, decreasing=FALSE)
sort(cv0.2_seed3$cvm, decreasing=FALSE)
sort(cv0.3_seed3$cvm, decreasing=FALSE)
sort(cv0.4_seed3$cvm, decreasing=FALSE)
sort(cv0.5_seed3$cvm, decreasing=FALSE)
sort(cv0.6_seed3$cvm, decreasing=FALSE)
sort(cv0.7_seed3$cvm, decreasing=FALSE)
sort(cv0.8_seed3$cvm, decreasing=FALSE)
sort(cv0.9_seed3$cvm, decreasing=FALSE)
sort(cv1_seed3$cvm, decreasing=FALSE)


cv0_seed3$lambda.min
cv0.1_seed3$lambda.min
cv0.2_seed3$lambda.min
cv0.3_seed3$lambda.min
cv0.4_seed3$lambda.min
cv0.5_seed3$lambda.min
cv0.6_seed3$lambda.min
cv0.7_seed3$lambda.min
cv0.8_seed3$lambda.min
cv0.9_seed3$lambda.min
cv1_seed3$lambda.min


#The code below repeats the process but giving the algorithm a different seed.

set.seed(131415)
cv0_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0)
cv0.1_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.1)
cv0.2_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.2)
cv0.3_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.3)
cv0.4_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.4)
cv0.5_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
cv0.6_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.6)
cv0.7_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.7)
cv0.8_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.8)
cv0.9_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 0.9)
cv1_seed4<- cv.glmnet(X, y, family = "binomial", alpha = 1)


plot(log(cv1_seed4$lambda), cv1_seed4$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1_seed4$name, ylim=c(1,2), xlim=c(-4,-1), main="Seed = 131415")
points(log(cv0.9_seed4$lambda), cv0.9_seed4$cvm, pch = 19, col = "grey")
points(log(cv0.8_seed4$lambda), cv0.8_seed4$cvm, pch = 19, col = "green")
points(log(cv0.7_seed4$lambda), cv0.7_seed4$cvm, pch = 19, col = "violet")
points(log(cv0.6_seed4$lambda), cv0.6_seed4$cvm, pch = 19, col = "darkgreen")
points(log(cv0.5_seed4$lambda), cv0.5_seed4$cvm, pch = 19, col = "yellow")
points(log(cv0.4_seed4$lambda), cv0.4_seed4$cvm, pch = 19, col = "orange")
points(log(cv0.3_seed4$lambda), cv0.3_seed4$cvm, pch = 19, col = "pink")
points(log(cv0.2_seed4$lambda), cv0.2_seed4$cvm, pch = 19, col = "black")
points(log(cv0.1_seed4$lambda), cv0.1_seed4$cvm, pch = 19, col = "brown")
points(log(cv0_seed4$lambda) , cv0_seed4$cvm , pch = 19, col = "blue")
legend("topright", legend = c("a1","a0.9",  "a0.8", "a0.7", "a0.6", "a0.5","a0.4","a0.3","a0.2","a0.1", "a0"),
       pch = 19, col = c("red","grey", "green","violet","darkgreen","yellow", "orange","pink","black","brown", "blue"))


sort(cv0_seed4$cvm, decreasing=FALSE)
sort(cv0.1_seed4$cvm, decreasing=FALSE)
sort(cv0.2_seed4$cvm, decreasing=FALSE)
sort(cv0.3_seed4$cvm, decreasing=FALSE)
sort(cv0.4_seed4$cvm, decreasing=FALSE)
sort(cv0.5_seed4$cvm, decreasing=FALSE)
sort(cv0.6_seed4$cvm, decreasing=FALSE)
sort(cv0.7_seed4$cvm, decreasing=FALSE)
sort(cv0.8_seed4$cvm, decreasing=FALSE)
sort(cv0.9_seed4$cvm, decreasing=FALSE)
sort(cv1_seed4$cvm, decreasing=FALSE)


cv0_seed4$lambda.min
cv0.1_seed4$lambda.min
cv0.2_seed4$lambda.min
cv0.3_seed4$lambda.min
cv0.4_seed4$lambda.min
cv0.5_seed4$lambda.min
cv0.6_seed4$lambda.min
cv0.7_seed4$lambda.min
cv0.8_seed4$lambda.min
cv0.9_seed4$lambda.min
cv1_seed4$lambda.min


#The code below repeats the process but giving the algorithm a different seed.

set.seed(161718)
cv0_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0)
cv0.1_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.1)
cv0.2_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.2)
cv0.3_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.3)
cv0.4_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.4)
cv0.5_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
cv0.6_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.6)
cv0.7_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.7)
cv0.8_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.8)
cv0.9_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 0.9)
cv1_seed5<- cv.glmnet(X, y, family = "binomial", alpha = 1)


plot(log(cv1_seed5$lambda), cv1_seed5$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1_seed5$name, ylim=c(1,2), xlim=c(-4,-1), main="Seed = 161718")
points(log(cv0.9_seed5$lambda), cv0.9_seed5$cvm, pch = 19, col = "grey")
points(log(cv0.8_seed5$lambda), cv0.8_seed5$cvm, pch = 19, col = "green")
points(log(cv0.7_seed5$lambda), cv0.7_seed5$cvm, pch = 19, col = "violet")
points(log(cv0.6_seed5$lambda), cv0.6_seed5$cvm, pch = 19, col = "darkgreen")
points(log(cv0.5_seed5$lambda), cv0.5_seed5$cvm, pch = 19, col = "yellow")
points(log(cv0.4_seed5$lambda), cv0.4_seed5$cvm, pch = 19, col = "orange")
points(log(cv0.3_seed5$lambda), cv0.3_seed5$cvm, pch = 19, col = "pink")
points(log(cv0.2_seed5$lambda), cv0.2_seed5$cvm, pch = 19, col = "black")
points(log(cv0.1_seed5$lambda), cv0.1_seed5$cvm, pch = 19, col = "brown")
points(log(cv0_seed5$lambda) , cv0_seed5$cvm , pch = 19, col = "blue")
legend("topright", legend = c("a1","a0.9",  "a0.8", "a0.7", "a0.6", "a0.5","a0.4","a0.3","a0.2","a0.1", "a0"),
       pch = 19, col = c("red","grey", "green","violet","darkgreen","yellow", "orange","pink","black","brown", "blue"))


sort(cv0_seed5$cvm, decreasing=FALSE)
sort(cv0.1_seed5$cvm, decreasing=FALSE)
sort(cv0.2_seed5$cvm, decreasing=FALSE)
sort(cv0.3_seed5$cvm, decreasing=FALSE)
sort(cv0.4_seed5$cvm, decreasing=FALSE)
sort(cv0.5_seed5$cvm, decreasing=FALSE)
sort(cv0.6_seed5$cvm, decreasing=FALSE)
sort(cv0.7_seed5$cvm, decreasing=FALSE)
sort(cv0.8_seed5$cvm, decreasing=FALSE)
sort(cv0.9_seed5$cvm, decreasing=FALSE)
sort(cv1_seed5$cvm, decreasing=FALSE)


cv0_seed5$lambda.min
cv0.1_seed5$lambda.min
cv0.2_seed5$lambda.min
cv0.3_seed5$lambda.min
cv0.4_seed5$lambda.min
cv0.5_seed5$lambda.min
cv0.6_seed5$lambda.min
cv0.7_seed5$lambda.min
cv0.8_seed5$lambda.min
cv0.9_seed5$lambda.min
cv1_seed5$lambda.min


####################################################################################################################################################################################################################################################

#Comparing the "best" models suggested by the Elastic Net Regression Results


##########################################################################################################################

## "Best" models suggested for the Enet run with seed 1: 12345.

coef(cv1_seed1, s = "lambda.min") #This line gives variables that should go into the model. It list all the variables that the Enet considered. Some have coefficient next to them, those are the ones Enet selected.

modelcv1_seed1<-glm(adoption~             #This runs the model recommended by the line above
                      inicost_money +           
                    envben_habitat +             
                    envben_soilqual +            
                    comp_pract +            
                    econwellb +                      
                    landsize +                   
                    riskavers +         
                    percvcomp +                
                    relvntknowskills +             
                    ecoconds_wind +             
                    singledecision,      
                  data = doitvars, 
                  family= binomial (link= logit)) 


plot_model(modelcv1_seed1, vline.color = "black", transform= NULL) #This plots the results

AIC(modelcv1_seed1)
BIC(modelcv1_seed1)
with(summary(modelcv1_seed1), 1 - deviance/null.deviance) #This estimates the pseudo-R-squared for the model. Pseudo-R-squared evaluates model fit for GLMs where traditional R-squared is not applicable like our logistic regression. 
summary(modelcv1_seed1)



##########################################################################################################################

## "Best" models suggested for the Enet run with seed 2: 56789.

coef(cv0.9_seed2, s = "lambda.min") #This line gives variables that should go into the model. It list all the variables that the Enet considered. Some have coefficient next to them, those are the ones Enet selected.

modelcv0.9_seed2<-glm(adoption~          #This runs the model recommended by the line above
                      inicost_money +                 
                      envben_habitat +             
                      envben_soilqual +            
                      comp_pract +             
                      econwellb +            
                      reslack_time +                     
                      landsize +      
                      riskavers +         
                      percvcomp +                
                      relvntknowskills +             
                      ecoconds_wind +            
                      singledecision,     
                    data = doitvars, 
                    family= binomial (link= logit)) 


plot_model(modelcv0.9_seed2, vline.color = "black", transform= NULL) #This plots the results

AIC(modelcv0.9_seed2)
BIC(modelcv0.9_seed2)
with(summary(modelcv0.9_seed2), 1 - deviance/null.deviance) #This estimates the pseudo-R-squared for the model. Pseudo-R-squared evaluates model fit for GLMs where traditional R-squared is not applicable like our logistic regression. 
summary(modelcv0.9_seed2)



########################################################################################################################

## "Best" models suggested for the Enet run with seed 3: 101112.

coef(cv1_seed3, s = "lambda.min") #This line gives variables that should go into the model. It list all the variables that the Enet considered. Some have coefficient next to them, those are the ones Enet selected.

modelcv1_seed3<-glm(adoption~             #This runs the model recommended by the line above
                        inicost_money +                
                        envben_habitat +            
                        envben_soilqual +           
                        comp_pract +             
                        econwellb +                    
                        landsize +                   
                        riskavers +         
                        percvcomp +               
                        relvntknowskills +           
                        ecoconds_wind +            
                        singledecision,     
                      data = doitvars, 
                      family= binomial (link= logit)) 


plot_model(modelcv1_seed3, vline.color = "black", transform= NULL) #This plots the results

AIC(modelcv1_seed3)
BIC(modelcv1_seed3)
with(summary(modelcv1_seed3), 1 - deviance/null.deviance) #This estimates the pseudo-R-squared for the model. Pseudo-R-squared evaluates model fit for GLMs where traditional R-squared is not applicable like our logistic regression.
summary(modelcv1_seed3)



#######################################################################################################################

## "Best" models suggested for the Enet run with seed 4: 131415. 

###Alpha 0.2 produces “best” model with this seed, but that model does not converge. Alpha 0.6 produces the second “best” model, this model converged, thus is reported in the thesis.


coef(cv0.2_seed4, s = "lambda.min") #This line gives variables that should go into the model. It list all the variables that the Enet considered. Some have coefficient next to them, those are the ones Enet selected.

modelcv0.2_seed4<-glm(adoption~         #This runs the model recommended by the line above
                        inicost_money +                 
                        envben_habitat +              
                        envben_flooding +             
                        envben_soilqual +  
                        econbenefits +            
                        comp_pract +           
                        easeundtreeprtct +             
                        revers +            
                        econwellb +           
                        reslack_money +            
                        reslack_time +                     
                        landsize +            
                        orgsize +                   
                        riskavers +         
                        percvcomp +         
                        fam +                   
                        relvntknowskills +            
                        ecoconds_wind +         
                        ecoconds_herb +                
                        support +             
                        singledecision +     
                        alldecision +             
                        groupdecision,     
                      data = doitvars, 
                      family= binomial (link= logit)) 


plot_model(modelcv0.2_seed4, vline.color = "black", transform= NULL) #This plots the results

AIC(modelcv0.2_seed4)
BIC(modelcv0.2_seed4)
with(summary(modelcv0.2_seed4), 1 - deviance/null.deviance) #This estimates the pseudo-R-squared for the model. Pseudo-R-squared evaluates model fit for GLMs where traditional R-squared is not applicable like our logistic regression.
summary(modelcv0.2_seed4)



coef(cv0.6_seed4, s = "lambda.min") #This line gives variables that should go into the model. It list all the variables that the Enet considered. Some have coefficient next to them, those are the ones Enet selected.

modelcv0.6_seed4<-glm(adoption~         #This runs the model recommended by the line above
                        inicost_money +                 
                        envben_habitat +             
                        envben_soilqual +             
                        comp_pract +              
                        econwellb +             
                        reslack_time +                     
                        landsize +                   
                        riskavers +         
                        percvcomp +                
                        relvntknowskills +            
                        ecoconds_wind +            
                        singledecision,     
                      data = doitvars, 
                      family= binomial (link= logit)) 


plot_model(modelcv0.6_seed4, vline.color = "black", transform= NULL) #This plots the results

AIC(modelcv0.6_seed4)
BIC(modelcv0.6_seed4)
with(summary(modelcv0.6_seed4), 1 - deviance/null.deviance) #This estimates the pseudo-R-squared for the model. Pseudo-R-squared evaluates model fit for GLMs where traditional R-squared is not applicable like our logistic regression.
summary(modelcv0.6_seed4)



#####################################################################################################################

## "Best" models suggested for the Enet run with seed 5: 161718.

coef(cv0.5_seed5, s = "lambda.min") #This line gives variables that should go into the model. It list all the variables that the Enet considered. Some have coefficient next to them, those are the ones Enet selected.

modelcv0.5_seed5<-glm(adoption~          #This runs the model recommended by the line above
                        inicost_money +                 
                        envben_habitat +              
                        envben_soilqual +             
                        comp_pract +            
                        econwellb +             
                        reslack_time +                     
                        landsize +                   
                        riskavers +         
                        percvcomp +         
                        fam +                   
                        relvntknowskills +             
                        ecoconds_wind +           
                        singledecision,     
                      data = doitvars, 
                      family= binomial (link= logit)) 


plot_model(modelcv0.5_seed5, vline.color = "black", transform= NULL) #This plots the results

AIC(modelcv0.5_seed5)
BIC(modelcv0.5_seed5)
with(summary(modelcv0.5_seed5), 1 - deviance/null.deviance) #This estimates the pseudo-R-squared for the model. Pseudo-R-squared evaluates model fit for GLMs where traditional R-squared is not applicable like our logistic regression.
summary(modelcv0.5_seed5)



###################################################################################################################################################################################################################################################

#Final model based on the above selection process, which indicated based on our approach were those suggested by seed 1 and 3, and their respective α=1 and lambda minimum.

##The code below runs the model.

finalmodel<-glm(adoption~  
                      inicost_money +                 
                      envben_habitat +              
                      envben_soilqual +             
                      comp_pract +             
                      econwellb +                     
                      landsize +                    
                      riskavers +         
                      percvcomp +               
                      relvntknowskills +             
                      ecoconds_wind +            
                      singledecision,     
                    data = doitvars, 
                    family= binomial (link= logit)) 

summary (finalmodel) #This summarizes the model estimates.

confint(finalmodel) #This produce the confidence intervals for the model estimates.



##The code below produces the plot included in the thesis.

set_theme(base = theme_classic())

plot_model(finalmodel, 
           type="est",
           vline.color = "black", 
           sort.est = TRUE, 
           transform= NULL,
           axis.labels = c(
             "Decisions made by a single individual*",
             "Windiness in the landholding**",
             "Risk aversion*",
             "Economic wellbeing*",
             "Expected changes to soil quality",
             "Perceived knowledge and skills to create woodland*",
             "Landholding size*",
             "Perceived competition for woodland creation resources*",
             "Compatibility with land use practices",
             "Initial costs - Money",
             "Expected changes to habitat for wildlife"), 
           title="",
           colors="bw", 
           auto.label=FALSE)+
  labs(y="
Coefficient estimates (Log-odds)")+
  theme(axis.text.x = element_text(family="sans", colour = "black", size=12,face="plain")) +
  theme(axis.text.y = element_text(family="sans", colour = "black", size=12,face="plain")) +
  theme(axis.title.x = element_text(family="sans", colour = "black", hjust=0.51, size=12,face="plain"))+
  scale_y_continuous(breaks = seq(-8, 8, by =2), limits=c(-6.2,6.2))


#THE END


#####################################################################################################################################################################################################################################################
  

#PS: This is the code we used to run our auxiliary analysis to complement our statistical model’s findings.

##We ran correlation tests for our variables to assess potential links between them.

install.packages("rcompanion")  # Install the rcompanion package
library(rcompanion)   # Load the rcompanion package


df_data <- as.data.frame(doitvars) #Transform the database containing the Diffusion of Innovation variables into a dataframe.
str(df_data) #Check data after change

df_data1<-df_data%>%mutate_if(is.numeric, as.ordered) #When modelling ordered variables were coded as numeric, to estimate the correlation test we transformed them as ordered variables
str(df_data1) #Check data after change


CorrTable<-rcompanion::correlation(df_data1, printClasses=TRUE)


#PS ENDS
