#INSTALLATIONS

##Install following packages (if needed)

install.packages("rstan")
install.packages("tidyverse") 
install.packages("brms") #brms is a wrapper for stan that makes it easy to use
install.packages("tidybayes")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("ggdist")
install.packages("bayesplot")
install.packages("bayestestR")
install.packages("marginaleffects")
install.packages("collapse")
install.packages("ggpubr")
install.packages("ggthemes")


########################################################################################################################################################################################

#LOADING PACKAGES

#Load following packages (if needed):

library(rstan)
library(tidyverse) 
library(brms) 
library(marginaleffects)
library(collapse)
library(tidybayes)
library(ggridges)
library(ggpubr)
library(ggplot2)
library(bayesplot)
library(ggthemes)
library(ggdist)
library(bayestestR) 


########################################################################################################################################################################################

#SET WORKING DIRECTORY AND CREATE DATABASE

##Set working directory. This is where you store the database you downloaded from our Github Repository into your computer. The command to do this is: 

setwd("C:/InsertPathToYourDirectoryHere")


##Load database

###data_dnwcs_raw is the original database, we don't wrangle on this, instead we create a copy of it called data_dnwcs, where we wrangle.

data_dnwcs_raw <- read_csv("C:/InsertPathToYourDatabaseHere")

data_dnwcs<-data_dnwcs_raw

data_dnwcs_ordered<-data_dnwcs%>%
  mutate(col12 = na_if(col12, 13),
         col12 = na_if(col12, 14),
         col17 = na_if(col17, 6),
         col17 = na_if(col17, 7),
         col53 = na_if(col53, 4),
         col53 = na_if(col53, 5),
         col55 = na_if(col55, 0),
         col55 = na_if(col55, 6),
         col55 = na_if(col55, 7),
         col103 = na_if(col103, 6),
         col104 = na_if(col104, 6),
         col105 = na_if(col105, 6),
         col106 = na_if(col106, 6),
         col107 = na_if(col107, 6),
         col108 = na_if(col108, 6),
         col109 = na_if(col109, 6),
         col110 = na_if(col110, 6),
         col111 = na_if(col111, 6),
         col112 = na_if(col112, 6),
         col140 = na_if(col140, 7),
         col140 = na_if(col140, 8), 
         col154 = na_if(col154, 7),
         col154 = na_if(col154, 8))%>%
  mutate(col55 = case_when(col55 %in% c('1') ~ '5', col55 %in% c('2') ~ '4', col55 %in% c('3')~'3', col55 %in% c('4')~'2', col55 %in% c('5')~'1'))%>%
  mutate(col140 = case_when(col140 %in% c('1') ~ '1', col140 %in% c('2') ~ '6', col140 %in% c('3')~'5', col140 %in% c('4')~'4', col140 %in% c('5')~'3', col140 %in% c('6')~'2'))%>%
  mutate(col154 = case_when(col154 %in% c('1') ~ '6', col154 %in% c('2') ~ '5', col154 %in% c('3')~'4', col154 %in% c('4')~'3', col154 %in% c('5')~'2', col154 %in% c('6')~'1'))%>%
  mutate(
    col12=as.ordered(col12), 
    col17=as.ordered(col17),
    col53=as.factor(col53),
    col55=as.ordered(col55), 
    col86=as.ordered(col86),
    col87=as.ordered(col87),
    col88=as.ordered(col88),
    col89=as.ordered(col89),
    col90=as.ordered(col90),
    col91=as.ordered(col91),
    col92=as.ordered(col92), 
    col93=as.ordered(col93), 
    col94=as.ordered(col94), 
    col95=as.ordered(col95),
    col96=as.ordered(col96), 
    col97=as.ordered(col97), 
    col98=as.ordered(col98), 
    col99=as.ordered(col99),
    col100=as.ordered(col100),
    col101=as.ordered(col101),
    col103=as.ordered(col103),
    col104=as.ordered(col104),
    col105=as.ordered(col105),
    col106=as.ordered(col106),
    col107=as.ordered(col107),
    col108=as.ordered(col108), 
    col109=as.ordered(col109), 
    col110=as.ordered(col110),
    col111=as.ordered(col111),
    col112=as.ordered(col112),
    col113=as.ordered(col113), 
    col114=as.ordered(col114), 
    col115=as.ordered(col115), 
    col116=as.ordered(col116), 
    col117=as.ordered(col117), 
    col118=as.ordered(col118), 
    col119=as.ordered(col119), 
    col120=as.ordered(col120), 
    col121=as.ordered(col121), 
    col122=as.ordered(col122), 
    col123=as.ordered(col123), 
    col124=as.ordered(col124), 
    col125=as.ordered(col125), 
    col126=as.ordered(col126), 
    col127=as.ordered(col127), 
    col128=as.ordered(col128), 
    col129=as.ordered(col129), 
    col130=as.ordered(col130), 
    col131=as.ordered(col131), 
    col132=as.ordered(col132), 
    col135=as.ordered(col135), 
    col136=as.ordered(col136), 
    col137=as.ordered(col137), 
    col138=as.ordered(col138), 
    col139=as.ordered(col139), 
    col140=as.ordered(col140),
    col142=as.factor(col142),
    col144=as.ordered(col144),
    col145=as.ordered(col145),
    col146=as.ordered(col146),
    col150=as.ordered(col150),
    col154=as.ordered(col154),
    col160=as.factor(col160)) 



################################################################################################################################################################################################


#MODEL

## Estimands: Relative advantage, Compatibility, Complexity, Observability and Trialability.
## Controls: Decision making
## Prior: Normal (0, 1). This constrains the model towards zero, while still allowing it to search the whole range of the logit function.

###Priors code:

my_priors_dnwcsarticle_final <- c(set_prior("normal (0,1)", class = "b"))


###Model code:

fit_dnwcsarticle_final<-brms::brm(col160~
                                    
                                    ##Innovation characteristics
                                    
                                    ###Relative advantage (Estimand)
                                    
                                    mo(col92)+ #InitialCostMoney
                                    mo (col93)+ #InitialCostTime
                                    mo(col97) + #PerceivedRisks 
                                    mo(col113)+ #EnvBenC
                                    mo(col114)+ #EnvBenWHabitat
                                    mo(col115)+ #EnvBenDiv
                                    mo(col116)+ #EnvBenWaterQual
                                    mo(col117)+ #EnvBenFlood
                                    mo(col118)+ #EnvBenSoilQual
                                    mo(col119)+ #EconomicBen
                                    mo(col120)+ # SocialBenWB
                                    mo(col121)+ #SocialPrestige
                                    mo(col122)+ #SocialBenJobs
                                    
                                    ###Compatibility (Estimand)
                                    
                                    mo(col137) + #CompValBel 
                                    mo(col138) + #CompNeeds 
                                    mo(col139) + #CompCurrPract
                                    
                                    ###Complexity (Estimand)
                                    
                                    mo(col105) + #EaseUnd_GroundPrep
                                    mo(col106) + #EaseUnd_HerbDam
                                    mo(col107) + #EaseUnd_Planting
                                    mo(col110) + #EaseDo_GroundPrep
                                    mo(col111) + #EaseDo_HerbDam
                                    mo(col112) + #EaseDo_Planting
                                    
                                    ###Observability (Estimand)
                                    mo(col135) + #VisResults
                                    mo(col136) + #VisPract
                                    
                                    ###Trialability (Estimand)
                                    
                                    mo(col91) + #Reversibility
                                    
                                    
                                    ##Adopter characteristics
                                    
                                    ###Decision making (Control)
                                    
                                    col53 + #InnovationDecision
                                    mo(col101), #Formalization
                                  
                                  family="bernoulli",data_dnwcs_ordered, iter=10000, prior=my_priors_dnwcsarticle_final, control=list(max_treedepth=15),cores=3, seed=12345)



###Save the model as an rds file:

saveRDS(fit_dnwcsarticle_final, file="DNWCSModel_FinalNormalPrior.rds")

###This line can be used to load the rds file, using the same as the original fit so code can be used without changes:


fit_dnwcsarticle_final<-readRDS(file="DNWCSModel_FinalNormalPrior.rds")



########################################################################################################################################################################################

#MODEL COEFFICIENTs PLOT

##This shows the name of the variables in the model fit

get_variables(fit_dnwcsarticle_final) 

##This creates a database containing the model draws for the variables listed in the spread_draws argument (i.e. the ones we want to plot).

m<-fit_dnwcsarticle_final%>%
  spread_draws(b_col532, b_col533, bsp_mocol92, bsp_mocol93, bsp_mocol97, bsp_mocol113, bsp_mocol114, bsp_mocol115, bsp_mocol116, bsp_mocol117, bsp_mocol118, bsp_mocol119, bsp_mocol120, bsp_mocol121, bsp_mocol122, bsp_mocol137, bsp_mocol138, bsp_mocol139, bsp_mocol105, bsp_mocol106, bsp_mocol107, bsp_mocol110, bsp_mocol111, bsp_mocol112, bsp_mocol135, bsp_mocol136, bsp_mocol91, bsp_mocol101)%>%
  gather_variables() 


##This creates the plot showing the posterior distributions for the model's estimands. The point_interval argument within stat_halfeye is the one determining whether HDI or ETI is plotted. Follow this link for more details: https://mjskay.github.io/ggdist/reference/stat_halfeye.html


m%>%
  ggplot(aes(y=.variable, x=.value))+
  stat_halfeye(point_interval = "median_hdi", .width = c(0.50, 0.90))+ 
  vline_0(color = "black") + 
  ggplot2::scale_y_discrete(labels=c("bsp_mocol92"= "Initial costs - Money",
                                     "bsp_mocol93"= "Initial costs - Time",
                                     "bsp_mocol97"= "Perceived risks",
                                     "bsp_mocol113"= "Expected changes to carbon storage", 
                                     "bsp_mocol114" = "Expected changes to habitat for wildlife",
                                     "bsp_mocol115" = "Expected changes to species diversity",
                                     "bsp_mocol116" = "Expected changes to water quality", 
                                     "bsp_mocol117"= "Expected changes to flooding prevention",
                                     "bsp_mocol118" = "Expected changes to soil quality",
                                     "bsp_mocol119" = "Expected changes to earnings",
                                     "bsp_mocol120" = "Expected changes to community well-being ",
                                     "bsp_mocol121" = "Expected changes to social recognition",
                                     "bsp_mocol122" = "Expected changes to number of jobs",
                                     "bsp_mocol137"= "Compatibility with values and beliefs",
                                     "bsp_mocol138"= "Compatibility with needs",
                                     "bsp_mocol139"= "Compatibility with land use practices",
                                     "bsp_mocol105"= "Ease of understanding - Ground preparations",
                                     "bsp_mocol106"= "Ease of understanding - Herbivore damage controls",
                                     "bsp_mocol107" = "Ease of understanding - Planting woodland ",
                                     "bsp_mocol110"= "Ease of doing - Ground preparations",
                                     "bsp_mocol111"= "Ease of doing - Herbivore damage controls",
                                     "bsp_mocol112"= "Ease of doing - Planting woodland",
                                     "bsp_mocol135"= "Visibility of results",
                                     "bsp_mocol136"= "Visibility of practice",
                                     "bsp_mocol91"= "Reversibility",
                                     "bsp_mocol101"= "Formalisation of decision making*",
                                     "b_col533"= "Authority decision*",
                                     "b_col532"= "Collective decision*"),
                            limits = c( 
                              
                              #Spaces in this code are intentional for plot aesthetics
                              
                              "bsp_mocol101", #Formalisation of decision making
                              "b_col532", #Collective decision
                              "b_col533", #Authority decision
                              "bsp_mocol135", #Visibility of results
                              "bsp_mocol136", #Visibility of practice
                              "bsp_mocol137", #Compatibility with values and beliefs
                              "bsp_mocol138", #Compatibility with needs
                              "bsp_mocol91", #Reversibility
                              "bsp_mocol97", #Perceived risks
                              "bsp_mocol92", #Initial costs - Money
                              "bsp_mocol93", #Initial cost - Time
                              "bsp_mocol121", #Expected changes to social recognition
                              "bsp_mocol122", #Expected changes to number of jobs
                              "bsp_mocol119", #Expected changes to earnings
                              "bsp_mocol120", #Expected changes to community well-being
                              "bsp_mocol116", #Expected changes to water quality
                              "bsp_mocol115", #Expected changes to species diversity
                              "bsp_mocol117", #Expected changes to flooding prevention
                              "bsp_mocol113", #Expected changes to carbon storage
                              "bsp_mocol110", #Ease of doing - Ground preparations
                              "bsp_mocol112", #Ease of doing - Planting woodland
                              "bsp_mocol111", #Ease of doing - Herbivore damage controls
                              "bsp_mocol105", #Ease of understanding - Ground preparations
                              "bsp_mocol107", #Ease of understanding - Planting woodland
                              
                              "bsp_mocol101",
                              
                              "bsp_mocol106", #Ease of understanding - Herbivore damage controls
                              "bsp_mocol114", #Expected changes to habitat for wildlife
                              "bsp_mocol139", #Compatibility with land use practices
                              "bsp_mocol118"))+ #Expected changes to soil quality
  xlab("
              Standardised coefficient estimates")+
  ylab("Predictors")+
  scale_x_continuous(limits=c(-4, 4))+ # This limits the value range of the x-axis.
  theme_classic()+
  theme(axis.text.y = element_text(family="sans", colour = "black", size=12, face="plain"))+
  theme(axis.title.x = element_text(family="sans", colour = "black", size=12, hjust=0.35, face="plain"))+
  theme(axis.title.y = element_text(family="sans", colour = "black", size=12,face="plain"))+
  theme(axis.text.x = element_text(family="sans", colour = "black", size=12, face="plain"))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.x = element_blank())





########################################################################################################################################################################################


#CONDITIONAL EFFECTS PLOTS

##This code as a whole generates 4 graphs that are then displayed on a single panel all together at once.


##CONDITIONAL EFFECTS PLOT FOR COMPATIBILITY WITH LAND USE PRACTICES (col139)

###The following code creates a dataframe that allows the functions "fitted" or "post_predict" to take draws from the variable of interest. The "rep" argument simply repeats the number as many times as specified after the comma, in our case 5 times.

preddat_CompLandUse<-data.frame(col53=rep(2,5),
                                col91=rep(3,5),
                                col92=rep(3,5), 
                                col93=rep(3,5),
                                col97=rep(3,5), 
                                col101=rep(3,5),
                                col105=rep(3,5),
                                col106=rep(3,5),
                                col107=rep(3,5),
                                col110=rep(3,5),
                                col111=rep(3,5),
                                col112=rep(3,5),
                                col113=rep(3,5),
                                col114=rep(3,5),
                                col115=rep(3,5),
                                col116=rep(3,5),
                                col117=rep(3,5),
                                col118=rep(3,5),
                                col119=rep(3,5),
                                col120=rep(3,5),
                                col121=rep(3,5),
                                col122=rep(3,5),
                                col135=rep(3,5),
                                col136=rep(3,5),
                                col137=rep(3,5),
                                col138=rep(3,5),
                                col139=c(1,2,3,4,5)) #We give the column we want to plot the conditional effects for the full range of its values.


###The following line of code carries out the posterior predictions

postPreds_CompLandUse<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_CompLandUse, summary=FALSE))


###This line gives each predictor a name

names(postPreds_CompLandUse)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")


###This pivots the postPreds, creating a two column table one with the name of the  predictor and its level, and another with the values of the draws

postPreds_CompLandUse<-postPreds_CompLandUse%>%
  pivot_longer(cols=1:ncol(postPreds_CompLandUse), names_to="Pred1_Value", values_to="draw")


###The code below generates the database with summaries of the computed draws. In this particular case the mean, standard error, Q5 and Q95 for each level (1, 2, 3, 4, 5)

z1<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_CompLandUse, prob=c(0.05,0.90)))


###This code adds a column to the z database with the name of each level

z1$Pred1_Value<-1:5


###This code plots conditional effects probabilities with a trend line overlaying the posterior distribution

comp<-ggplot(z1,aes(x=Pred1_Value,y=Estimate))+
  stat_interval(data=postPreds_CompLandUse,aes(y=draw), .width = c(0.5, 0.90), point_interval = "mean_hdi")+
  scale_color_manual(values= c("gray70","gray40"))+
  geom_point(size=3)+
  geom_line()+
  theme_bw()+
  ggtitle("A")+
  labs(x="
Agreement with:'Creating woodland 
matches our land use practices'", colour="HDI") +
  scale_x_discrete(labels=c("Pred1_1"="Strongly 
disagree", "Pred1_2"="Disagree", "Pred1_3"="Neutral", "Pred1_4"="Agree", "Pred1_5"="Strongly 
agree"))+
  scale_y_continuous(position="right")+
  theme(axis.text.x = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(axis.title.x = element_text(family="sans",colour = "black", size=14, face="plain"))+
  theme(legend.title= element_text(family="sans",colour="black", size=10,face="plain"))+
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y.right = element_line(colour="black"))+
  theme(plot.title = element_text(family="sans", colour="black", size=10, face="bold"))





##CONDITIONAL EFFECTS PLOT FOR EXPECTED CHANGES TO HABITAT FOR WILDLIFE (col114) BASED ON FINAL MODEL


###The following code creates a dataframe that allows the functions fitted or post_predict to take draws from the variable of interest

preddat_ChangesHabitat<-data.frame(col53=rep(2,5),
                                   col91=rep(3,5),
                                   col92=rep(3,5), 
                                   col93=rep(3,5),
                                   col97=rep(3,5), 
                                   col101=rep(3,5),
                                   col105=rep(3,5),
                                   col106=rep(3,5),
                                   col107=rep(3,5),
                                   col110=rep(3,5),
                                   col111=rep(3,5),
                                   col112=rep(3,5),
                                   col113=rep(3,5),
                                   col114=c(1,2,3,4,5),#We give the column we want to plot the conditional effects for the full region of its values.
                                   col115=rep(3,5),
                                   col116=rep(3,5),
                                   col117=rep(3,5),
                                   col118=rep(3,5),
                                   col119=rep(3,5),
                                   col120=rep(3,5),
                                   col121=rep(3,5),
                                   col122=rep(3,5),
                                   col135=rep(3,5),
                                   col136=rep(3,5),
                                   col137=rep(3,5),
                                   col138=rep(3,5),
                                   col139=rep(3,5)) 


###The following line of code carries out the posterior predictions (What do posterior prediction mean??)

postPreds_ChangesHabitat<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_ChangesHabitat, summary=FALSE))


###This line gives each predictor a name

names(postPreds_ChangesHabitat)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")


###This pivots the postPreds, creating a two column table one with the name of the  predictor and its level, and another with the values of the draws

postPreds_ChangesHabitat<-postPreds_ChangesHabitat%>%
  pivot_longer(cols=1:ncol(postPreds_ChangesHabitat), names_to="Pred1_Value", values_to="draw")



###The code below generates the database with summaries of the computed draws. In this particular case the mean, standard error, Q5 and Q95 for each level (1, 2, 3, 4, 5)

z2<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_ChangesHabitat, prob=c(0.05,0.90)))


###This code adds a column to the z database with the name of each level

z2$Pred1_Value<-1:5


###This code plots conditional effects probabilities with a trend line overlaying the posterior distribution

habitat<-ggplot(z2,aes(x=Pred1_Value,y=Estimate))+
  stat_interval(data=postPreds_ChangesHabitat,aes(y=draw), .width = c(0.5,0.90), point_interval = "mean_hdi" )+
  scale_color_manual(values= c("gray70","gray40"))+
  geom_point(size=3)+
  geom_line()+
  ggtitle("C")+
  theme_bw()+
  labs(x="
Response to: 'How would you expect habitat for wildlife 
to change after woodland creation?'", colour="HDI") +
  scale_x_discrete(labels=c("Pred1_1"="Decrease 
greatly", "Pred1_2"="Decrease 
slightly", "Pred1_3"="No change", "Pred1_4"="Increase 
slightly", "Pred1_5"="Increase 
greatly"))+
  scale_y_continuous(position="right")+
  theme(axis.title.x = element_text(family="sans", colour = "black", size=14, face="plain"))+
  theme(axis.text.x = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(legend.title= element_text(family="sans",colour="black", size=10,face="plain"))+
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),, 
        axis.ticks.y.right = element_line(colour="black"))+
  theme(plot.title = element_text(family="sans", colour="black", size=10, face="bold"))





##CONDITIONAL EFFECTS PLOT FOR EXPECTED CHANGES TO SOIL QUALITY (col118) BASED ON FINAL MODEL


###The following code creates a dataframe that allows the functions fitted or post_predict to take draws from the variable of interest

preddat_ChangesSoil<-data.frame(col53=rep(2,5),
                                col91=rep(3,5),
                                col92=rep(3,5), 
                                col93=rep(3,5),
                                col97=rep(3,5), 
                                col101=rep(3,5),
                                col105=rep(3,5),
                                col106=rep(3,5),
                                col107=rep(3,5),
                                col110=rep(3,5),
                                col111=rep(3,5),
                                col112=rep(3,5),
                                col113=rep(3,5),
                                col114=rep(3,5),
                                col115=rep(3,5),
                                col116=rep(3,5),
                                col117=rep(3,5),
                                col118=c(1,2,3,4,5),#We give the column we want to plot the conditional effects for the full region of its values
                                col119=rep(3,5),
                                col120=rep(3,5),
                                col121=rep(3,5),
                                col122=rep(3,5),
                                col135=rep(3,5),
                                col136=rep(3,5),
                                col137=rep(3,5),
                                col138=rep(3,5),
                                col139=rep(3,5)) 


###The following line of code carries out the posterior predictions (What do posterior prediction mean??)

postPreds_ChangesSoil<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_ChangesSoil, summary=FALSE))


###This line gives each predictor a name

names(postPreds_ChangesSoil)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")


###This pivots the postPreds, creating a two column table one with the name of the  predictor and its level, and another with the values of the draws

postPreds_ChangesSoil<-postPreds_ChangesSoil%>%
  pivot_longer(cols=1:ncol(postPreds_ChangesSoil), names_to="Pred1_Value", values_to="draw")


###The code below generates the database with summaries of the computed draws. In this particular case the mean, standard error, Q5 and Q95 for each level (1, 2, 3, 4, 5)

z3<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_ChangesSoil, prob=c(0.05,0.90)))


###This code adds a column to the z database with the name of each level

z3$Pred1_Value<-1:5


###This code plots conditional effects probabilities with a trend line overlaying the posterior distribution

soilqual<-ggplot(z3,aes(x=Pred1_Value,y=Estimate))+
  stat_interval(data=postPreds_ChangesSoil,aes(y=draw), .width = c(0.5,0.9), point_interval = "mean_hdi")+
  scale_color_manual(values= c("gray70","gray40"))+
  geom_point(size=3)+
  geom_line()+
  ggtitle("D")+
  theme_bw()+
  labs(x="
Response to: 'How would you expect soil quality 
to change after woodland creation?'", colour="HDI") +
  scale_x_discrete(labels=c("Pred1_1"="Decrease 
greatly", "Pred1_2"="Decrease 
slightly", "Pred1_3"="No change", "Pred1_4"="Increase 
slightly", "Pred1_5"="Increase 
greatly"))+
  scale_y_continuous(labels=c("0.00"="0%    ","0.25"="25%   ", "0.5"="50%   ", "0.75"="75%   ", "1"="100%  " ), limits=c(0,1))+
  theme(axis.text.y = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(axis.title.x = element_text(family="sans", colour = "black", size=14, face="plain"))+
  theme(axis.text.x = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(legend.title= element_text(family="sans", colour="black", size=10,face="plain"))+
  theme(legend.position = "none", 
        axis.title.y = element_blank())+
  theme(plot.title = element_text(family="sans", colour="black", size=10, face="bold"))




##CONDITIONAL EFFECTS PLOT FOR EASE OF UNDERSTANDING HERBIVORE DAMAGE CONTROLS (col106) BASED ON FINAL MODEL


###The following code creates a dataframe that allows the functions fitted or post_predict to take draws from the variable of interest

preddat_UndHerbDamCtrl<-data.frame(col53=rep(2,5),
                                   col91=rep(3,5),
                                   col92=rep(3,5), 
                                   col93=rep(3,5),
                                   col97=rep(3,5), 
                                   col101=rep(3,5),
                                   col105=rep(3,5),
                                   col106=c(1,2,3,4,5),#We give the column we want to plot the conditional effects for the full region of its values
                                   col107=rep(3,5),
                                   col110=rep(3,5),
                                   col111=rep(3,5),
                                   col112=rep(3,5),
                                   col113=rep(3,5),
                                   col114=rep(3,5),
                                   col115=rep(3,5),
                                   col116=rep(3,5),
                                   col117=rep(3,5),
                                   col118=rep(3,5),
                                   col119=rep(3,5),
                                   col120=rep(3,5),
                                   col121=rep(3,5),
                                   col122=rep(3,5),
                                   col135=rep(3,5),
                                   col136=rep(3,5),
                                   col137=rep(3,5),
                                   col138=rep(3,5),
                                   col139=rep(3,5)) 


###The following line of code carries out the posterior predictions (What do posterior prediction mean??)

postPreds_UndHerbDamCtrl<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_UndHerbDamCtrl, summary=FALSE))


###This line gives each predictor a name

names(postPreds_UndHerbDamCtrl)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")


###This pivots the postPreds, creating a two column table one with the name of the  predictor and its level, and another with the values of the draws

postPreds_UndHerbDamCtrl<-postPreds_UndHerbDamCtrl%>%
  pivot_longer(cols=1:ncol(postPreds_UndHerbDamCtrl), names_to="Pred1_Value", values_to="draw")



###The code below generates the database with summaries of the computed draws. In this particular case the mean, standard error, Q5 and Q95 for each level (1, 2, 3, 4, 5)

z4<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_UndHerbDamCtrl, prob=c(0.05,0.90)))


###This code adds a column to the z database with the name of each level

z4$Pred1_Value<-1:5


#This code plots conditional effects probabilities with a trend line overlaying the posterior distribution

herbcontrol<-ggplot(z4, aes(x=Pred1_Value,y=Estimate))+
  stat_interval(data=postPreds_UndHerbDamCtrl,aes(y=draw), .width = c(0.5,0.90), point_interval = "mean_hdi")+
  scale_color_manual(values= c("gray70","gray40"))+
  geom_point(size=3)+
  geom_line()+
  ggtitle("B")+
  theme_bw()+
  labs(x="
Response to: 'How difficult or easy is/was it to understand 
interventions to protect new woodland from herbivore damage?'", colour="HDI")+
  scale_x_discrete(labels=c("Pred1_1"="Very 
difficult", "Pred1_2"="Difficult", "Pred1_3"="Neutral", "Pred1_4"="Easy", "Pred1_5"="Very 
easy"))+
  scale_y_continuous(labels=c("0.00"="0%    ","0.25"="25%   ", "0.5"="50%   ", "0.75"="75%   ", "1"="100%  " ))+
  theme(axis.text.y = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(axis.title.x = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(axis.text.x = element_text(family="sans", colour = "black", size=14,face="plain"))+
  theme(legend.title= element_text(family="sans",colour="black", size=10,face="plain"))+
  theme(legend.position = "none", 
        axis.title.y = element_blank())+
  theme(plot.title = element_text(family="sans", colour="black", size=10, face="bold"))


#The following code arranges the four graphs previously created into a single panel that displays them all together. We then use the "annotate_figure" function to add a common y axis to the whole panel.

condeffect4panel<-ggarrange(comp, herbcontrol, habitat, soilqual, align = "hv", common.legend=TRUE, legend= "right")


annotate_figure(condeffect4panel,
                left = text_grob("Probability of having adopted mixed native woodland creation initiatives",family="sans", size=14, color = "black", face="plain", rot = 90, vjust = 2,hjust = 0.44)) 





##########################################################################################################################################################################################

#MODEL COEFFICIENTs DATA

##Model estimate tables. Missing rhat, ees

get_variables(fit_dnwcsarticle_final)

m<-fit_dnwcsarticle_final%>%
  spread_draws(b_col532, b_col533, bsp_mocol92, bsp_mocol93, bsp_mocol97, bsp_mocol113, bsp_mocol114, bsp_mocol115, bsp_mocol116, bsp_mocol117, bsp_mocol118, bsp_mocol119, bsp_mocol120, bsp_mocol121, bsp_mocol122, bsp_mocol137, bsp_mocol138, bsp_mocol139, bsp_mocol105, bsp_mocol106, bsp_mocol107, bsp_mocol110, bsp_mocol111, bsp_mocol112, bsp_mocol135, bsp_mocol136, bsp_mocol91, bsp_mocol101)%>%
  gather_variables()

#HDI Parameters

m%>% 
  median_hdi(.value, .width = c(0.5,0.9))%>%
  print(n=58)

m%>%
  mean_hdi(.value, .width = c(0.5,0.9))%>%
  print(n=58)

m%>%
  mode_hdi(.value, .width = c(0.5,0.9))%>%
  print(n=58)


#ETI Parameters

m%>%
  median_qi(.value, .width = c(0.5,0.9))%>%
  print(n=58)

m%>%
  mean_qi(.value, .width = c(0.5,0.9))%>%
  print(n=58)

m%>%
  mode_qi(.value, .width = c(0.5,0.9))%>%
  print(n=58)

#Model summary

m%>%
  summarise_draws()%>%
  print(n=28)


##########################################################################################################################################################################################


#CONDITIONAL EFFECTS DATA

##Compatibility with current land Uses conditional effect estimates

postPreds_CompLandUse<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_CompLandUse, summary=FALSE))

names(postPreds_CompLandUse)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")

postPreds_CompLandUse%>%
  summarise_draws()

hdi(postPreds_CompLandUse, ci=c(0.5,0.9))

eti(postPreds_CompLandUse, ci=c(0.5,0.9))


# Expected changes to habitat for wildlife conditional effect estimates

postPreds_ChangesHabitat<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_ChangesHabitat, summary=FALSE))

names(postPreds_ChangesHabitat)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")

postPreds_ChangesHabitat%>%
  summarise_draws()

hdi(postPreds_ChangesHabitat, ci=c(0.5,0.9))

eti(postPreds_ChangesHabitat, ci=c(0.5,0.9))


# Expected changes to soil quality conditional effect estimates

postPreds_ChangesSoil<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_ChangesSoil, summary=FALSE))

names(postPreds_ChangesSoil)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")

postPreds_ChangesSoil%>%
  summarise_draws()

hdi(postPreds_ChangesSoil, ci=c(0.5,0.9))

eti(postPreds_ChangesSoil, ci=c( 0.5, 0.9))


#Ease of understanding herbivore damage controls conditional effect estimates

postPreds_UndHerbDamCtrl<-as.data.frame(fitted(fit_dnwcsarticle_final, newdata=preddat_UndHerbDamCtrl, summary=FALSE))

names(postPreds_UndHerbDamCtrl)<-c("Pred1_1","Pred1_2","Pred1_3", "Pred1_4", "Pred1_5")

postPreds_UndHerbDamCtrl%>%
  summarise_draws()

hdi(postPreds_UndHerbDamCtrl, ci=c(0.5,0.9))

eti(postPreds_UndHerbDamCtrl, ci=c(0.5,0.9))


##########################################################################################################################################################################################

#The conditional effects results we get using our code can be reproduced using the brms "conditional_effects" function. The code lines below show how to do this.

##This line creates a database that is used by the conditional_effects function to determine the values the predictors we condition on should be fixed at for the conditional effect calculations.

conditions <- data.frame(col53=2,
                         col91=3, 
                         col92=3, 
                         col93=3, 
                         col97=3,
                         col101=3,
                         col105=3,
                         col106=3,
                         col107=3,
                         col110=3,
                         col111=3,
                         col112=3, 
                         col113=3, 
                         col114=3, 
                         col115=3, 
                         col116=3, 
                         col117=3,
                         col118=3, 
                         col119=3, 
                         col120=3, 
                         col121=3, 
                         col122=3, 
                         col135=3, 
                         col136=3,
                         col137=3, 
                         col138=3, 
                         col139=3)


#This allows you to plot the mean conditional effect for a single predictor.Change "robust" to "T" or "TRUE" to plot the median conditional effect.

conditional_effects(fit_dnwcsarticle_final, effects = "col139",
                    conditions = conditions, robust = F)


#These three lines produce a table with the median conditional effect values

median_ce_col139<-conditional_effects(fit_dnwcsarticle_final, effects = "col139", conditions = conditions, surface=TRUE, robust = T)

view(median_ce_col139)

plot(median_ce_col139)


#These three lines produce a table with the mean conditional effect values

mean_ce_col139<-conditional_effects(fit_dnwcsarticle_final, effects = "col139", conditions = conditions, surface=TRUE, robust = F)

view(mean_ce_col139)

plot(mean_ce_col139)

#THE END