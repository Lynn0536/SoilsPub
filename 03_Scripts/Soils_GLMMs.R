################################################################################
###                   Generalized Linear Mixed Models                        ###
###                          EPA-Funded Soil Data                            ###
###                        Code by: Ashlynn N. Smith                         ###
###                     USEPA Project Number: GC-00D80521                    ###
###    Joint Project between Atlanta Botanical Garden & University of FL     ###
################################################################################

################################ Install Packages ##############################

install.packages("Rmisc")
install.packages("pals")
install.packages("ggsci")
install.packages("ISLR")
install.packages("report")
install.packages("sjplot") # NA this version of R
install.packages("flextable")
install.packages("boot")
install.packages("bbmle")
install.packages("performance")
install.packages("car")
install.packages("aods3") # NA this version of R
install.packages("cowplot")
install.packages("glmmTMB")
install.packages("Matrix")
install.packages("lme4")
install.packages("coin")
install.packages("ggpubr")
install.packages("effects")
install.packages("lmerTest")
install.packages("MuMIn")
install.packages("glmm.hp")
install.packages("emmeans")
install.packages("multcomp")
install.packages("DHARMa")

################################## Load Packages ###############################

#library(Rmisc)
library(pals)
library(ggsci)
library(ISLR)
library(report)
#library(sjPlot)
library(flextable)
library(boot)
library(bbmle)
library(performance)
library(car)
#library(aods3)
library(tidyverse)
library(vegan)
library(plyr)
library(cowplot)
library(glmmTMB)
library(Matrix)
library(lme4)
library(coin)
library(grid)
library(ggplot2)
library(ggpubr)
library(effects)
library(lmerTest)
library(MuMIn)
library(glmm.hp)
library(emmeans)
library(multcomp)
library(DHARMa)

######################### Call in Data #########################################

RawData <- read_csv("02_CleanData/EPA_Soil_CLEAN.csv",
                 col_types = cols(Year = col_factor(),
                                  Position = col_factor(),
                                  Treatment = col_factor(),
                                  Site = col_factor()))

SoilsData <- RawData %>%
  filter(Treatment != "Scraped") %>%
  filter(Treatment != "Burned") %>%
  filter(Treatment != "Scraped + Burned")

summary(SoilsData)   
str(SoilsData)

########################### TOTAL N ###########################################
############ Visualize the Total_N Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=Total_N)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Total_N))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Total_N))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=log(Total_N), fill=Position)) + 
  geom_boxplot(notch=TRUE)

################# Full Model Testing - Total_N ################################

?glmmTMB::family_glmmTMB # Family options 

NMod1 <- glmmTMB(Total_N ~ Treatment * Position * Year + (1|Site) + 
                      (1|Site:Treatment) + (1|Site:Treatment:Position), 
                    data=SoilsData, family=lognormal())

NMod1a <- glmmTMB(Total_N ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment), 
                 data=SoilsData, family=lognormal())

NMod1b <- glmmTMB(Total_N ~ Treatment * Position * Year + (1|Site), 
                  data=SoilsData, family=lognormal())

NMod2 <- glmmTMB(log(Total_N) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

NMod2a <- glmmTMB(log(Total_N) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment), 
                 data=SoilsData, family=gaussian())

NMod2b <- glmmTMB(log(Total_N) ~ Treatment * Position * Year + (1|Site), 
                 data=SoilsData, family=gaussian())


NMod3 <- glmmTMB(Total_N ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

AIC(NMod1, NMod2, NMod3, NMod1a, NMod1b)
AIC(NMod2, NMod2a, NMod2b)

# Now look at the residuals 
plot(simulateResiduals(NMod2))
hist(simulateResiduals(NMod2)) ## histogram should be flat
qqPlot(resid(NMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(NMod2))
plot(fitted(NMod2), residuals(NMod2))

########################### Total_N Model Results ##############################

Anova(NMod2, type="III")

# Pairwise Treatment  
PairsNMod <- emmeans(NMod2, ~Treatment, type='response') 
TRT_Pairs_N <- pairs(PairsNMod)
TN_Tibble <- as_tibble(summary(TRT_Pairs_N))
CI_Letters_TN <- cld(PairsNMod, Letters=letters, sort=TRUE, decreasing=TRUE)

# Pairwise Position X Year 
PairsNMod_2 <- emmeans(NMod2, ~Position*Year, type='response') 
PosYr_Pairs_N <- pairs(PairsNMod_2)
TN_Tibble2 <- as_tibble(summary(PosYr_Pairs_N))
CI_Letters_2 <- cld(PairsNMod_2, Letters=letters, sort=TRUE, decreasing=TRUE)


############ Visualize the Total_P Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=Total_P)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Total_P))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Total_P))) +
  geom_histogram()


ggplot(SoilsData, aes(x=Treatment, y=Total_P, fill=Position)) + 
  geom_boxplot(notch=TRUE)

################# Full Model Testing - Total_P ################################

?glmmTMB::family_glmmTMB # Family options 

PMod1 <- glmmTMB(Total_P ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=lognormal())

PMod1a <- glmmTMB(Total_P ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment), 
                  data=SoilsData, family=lognormal())

PMod1b <- glmmTMB(Total_P ~ Treatment * Position * Year + (1|Site), 
                  data=SoilsData, family=lognormal())

PMod2 <- glmmTMB(log(Total_P) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

PMod3 <- glmmTMB(Total_P ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian()) # DNC

AIC(PMod1, PMod2, PMod1a, PMod1b)

# Now look at the residuals 
plot(simulateResiduals(PMod2))
hist(simulateResiduals(PMod2)) ## histogram should be flat
qqPlot(resid(PMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(PMod2))
plot(fitted(PMod2), residuals(PMod2))

########################### Total_P Model Results ##############################

Anova(PMod2, type="III")

# Pairwise Treatment x Year 
PairsPMod <- emmeans(PMod2, ~Treatment|Year, type='response') 
TrtYr_Pairs_P <- pairs(PairsPMod)
TP_Tibble <- as_tibble(summary(TrtYr_Pairs_P))
CI_Letters <- cld(PairsPMod, Letters=letters, sort=TRUE, decreasing=TRUE)

# Pairwise Position x Year 
PairsPMod_2 <- emmeans(PMod2, ~Position*Year, type='response') 
PosYr_Pairs_P <- pairs(PairsPMod_2)
TP_Tibble2 <- as_tibble(summary(PosYr_Pairs_P))
CI_Letters_3 <- cld(PairsPMod_2, Letters=letters, sort=TRUE, decreasing=TRUE)

############################### TOTAL K #####################################

############ Visualize the Total_K Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=Total_K)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Total_K))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Total_K))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=log(Total_K), fill=Position)) + 
  geom_boxplot(notch=TRUE)


################# Full Model Testing - Total_K ################################

?glmmTMB::family_glmmTMB # Family options 

KMod2 <- glmmTMB(log(Total_K) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(KMod2))
hist(simulateResiduals(KMod2)) ## histogram should be flat
qqPlot(resid(KMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(KMod2))
plot(fitted(KMod2), residuals(KMod2))

########################### Total_K Model Results ##############################

Anova(KMod2, type="III")

# Pairwise Treatment x Year 
PairsKMod <- emmeans(KMod2, ~Treatment|Year, type='response') 
TrtYr_Pairs_K <- pairs(PairsKMod)
TK_Tibble <- as_tibble(summary(TrtYr_Pairs_K))
CI_Letters_TK <- cld(PairsKMod, Letters=letters, sort=TRUE, decreasing=TRUE)

# Pairwise Position X Year 
PairsKMod_2 <- emmeans(KMod2, ~Position*Year, type='response') 
PosYr_Pairs_K <- pairs(PairsKMod_2)
TK_Tibble2 <- as_tibble(summary(PosYr_Pairs_K))
CI_Letters_TK_2 <- cld(PairsKMod_2, Letters=letters, sort=TRUE, decreasing=TRUE)

# Pairwise Treatment X Position 
PairsKMod_3 <- emmeans(KMod2, ~Treatment*Position, type='response') 
TRTPos_Pairs_K <- pairs(PairsKMod_3)
TK_Tibble3 <- as_tibble(summary(TRTPos_Pairs_K))
CI_Letters_TK_3 <- cld(PairsKMod_3, Letters=letters, sort=TRUE, decreasing=TRUE)

############################### Cl #####################################
## Ended up eliminating this variable from publication after talking with analysis lab 
############ Visualize the Cl Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=Cl)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Cl))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Cl))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=Cl, fill=Position)) + 
  geom_boxplot(notch=TRUE)


ClMod2 <- glmmTMB(log(Cl) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(ClMod2))
hist(simulateResiduals(KMod2)) ## histogram should be flat
qqPlot(resid(ClMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(ClMod2))
plot(fitted(ClMod2), residuals(ClMod2))

##################### Cl Model Results  ##########################################
## May need to round values and use a poisson distribution #######################
## Ended up eliminating this variable from publication after talking with analysis lab 

Anova(ClMod2, type="III")

# Pairwise Treatment x Year 
PairsClMod <- emmeans(ClMod2, ~Treatment|Year, type='response') 
TrtYr_Pairs_Cl <- pairs(PairsClMod)
Cl_Tibble <- as_tibble(summary(TrtYr_Pairs_Cl))
CI_Letters_Cl <- cld(PairsClMod, Letters=letters, sort=TRUE, decreasing=TRUE)

############################### Bulk Density ###################################

########## Visualize the Bulk Density Data ##### Decide on Outliers NOW ########

# Original Data 
ggplot(SoilsData, aes(x=BulkDensity)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(BulkDensity))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(BulkDensity))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=BulkDensity, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# The original data for Bulk Density is the opposite of the other response variables 
# It has a left-skew distribution 
# So I will try and see how a model fits with original data 

BDMod3 <- glmmTMB(BulkDensity ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian()) 

# Now look at the residuals 
plot(simulateResiduals(BDMod3))
hist(simulateResiduals(BDMod3)) ## histogram should be flat
qqPlot(resid(BDMod3))  ## residuals should line up pretty closely to the blue line
hist(residuals(BDMod3))
plot(fitted(BDMod3), residuals(BDMod3))

# These results are not terrible using the original data! 

########################### Bulk Density Model Results ##########################

Anova(BDMod3, type="III")

# Pairwise Treatment x Position
PairsBDMod <- emmeans(BDMod3, ~Treatment*Position, type='response') 
TrtPos_Pairs_BD <- pairs(PairsBDMod)
BD_Tibble <- as_tibble(summary(TrtPos_Pairs_BD))
CI_Letters_BD <- cld(PairsBDMod, Letters=letters, sort=TRUE, decreasing=TRUE)

# Pairwise Treatment x Year
PairsBDMod2 <- emmeans(BDMod3, ~Treatment|Year, type='response') 
TrtYr_Pairs_BD <- pairs(PairsBDMod2)
BD_Tibble2 <- as_tibble(summary(TrtYr_Pairs_BD))
CI_Letters_BD2 <- cld(PairsBDMod2, Letters=letters, sort=TRUE, decreasing=TRUE)

################################# Mg ###########################################

########## Visualize the Mg Data ##### Decide on Outliers NOW ##################

# Original Data 
ggplot(SoilsData, aes(x=Mg)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Mg))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Mg))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=Mg, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# For Mg, the log transformation produced the most normal distribution 
# So I will use log(Mg) as the response variable 

MgMod2 <- glmmTMB(log(Mg) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(MgMod2))
hist(simulateResiduals(MgMod2)) ## histogram should be flat
qqPlot(resid(MgMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(MgMod2))
plot(fitted(MgMod2), residuals(MgMod2))

########################### Mg Model Results ###################################

Anova(MgMod2, type="III")

# Pairwise Treatment 
PairsMgMod <- emmeans(MgMod2, ~Treatment, type='response') 
TRT_Pairs_Mg <- pairs(PairsMgMod)
Mg_Tibble <- as_tibble(summary(TRT_Pairs_Mg))
CI_Letters_Mg <- cld(PairsMgMod, Letters=letters, sort=TRUE, decreasing=TRUE)

################################# Ca ###########################################

########## Visualize the Ca Data ##### Decide on Outliers NOW ##################

# Original Data 
ggplot(SoilsData, aes(x=Ca)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Ca))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Ca))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=Ca, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# For Ca, the log transformation produced the most normal distribution 
# So I will use log(Ca) as the response variable 

CaMod2 <- glmmTMB(log(Ca) ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment) + (1|Site:Treatment:Position), 
                  data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(CaMod2))
hist(simulateResiduals(CaMod2)) ## histogram should be flat
qqPlot(resid(CaMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(CaMod2))
plot(fitted(CaMod2), residuals(CaMod2))

########################### Ca Model Results ###################################

Anova(CaMod2, type="III")

# Pairwise Treatment 
PairsCaMod <- emmeans(CaMod2, ~Treatment, type='response') 
TRT_Pairs_Ca <- pairs(PairsCaMod)
Ca_Tibble <- as_tibble(summary(TRT_Pairs_Ca))
CI_Letters_Ca <- cld(PairsCaMod, Letters=letters, sort=TRUE, decreasing=TRUE)

################################# pH ###########################################

########## Visualize the pH Data ##### Decide on Outliers NOW ##################

# Original Data 
ggplot(SoilsData, aes(x=pHw)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(pHw))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(pHw))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=pHw, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# The original data for pH has a close to normal distribution already 
# So I will try and see how a model fits with original data 

pHMod3 <- glmmTMB(pHw ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())


# Now look at the residuals 
plot(simulateResiduals(pHMod3))
hist(simulateResiduals(pHMod3)) ## histogram should be flat
qqPlot(resid(pHMod3))  ## residuals should line up pretty closely to the blue line
hist(residuals(pHMod3))
plot(fitted(pHMod3), residuals(pHMod3))

########################### pH Model Results ###################################

Anova(pHMod3, type="III")

# Pairwise Treatment 
PairspHMod <- emmeans(pHMod3, ~Treatment|Year, type='response') 
TrtYr_Pairs_pH <- pairs(PairspHMod)
pH_Tibble <- as_tibble(summary(TrtYr_Pairs_pH))
CI_Letters_pH <- cld(PairspHMod, Letters=letters, sort=TRUE, decreasing=TRUE)

################################# CEC ###########################################

########## Visualize the CEC Data ##### Decide on Outliers NOW ##################

# Original Data 
ggplot(SoilsData, aes(x=CEC)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(CEC))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(CEC))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=CEC, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# The sqrt transformation produced the most normal data distribution, but the 
# distribution of the original data doesn't look too far from normal so I am 
# going to see how the model fits with original data first 

CECMod3 <- glmmTMB(CEC ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment) + (1|Site:Treatment:Position), 
                  data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(CECMod3))
hist(simulateResiduals(CECMod3)) ## histogram should be flat
qqPlot(resid(CECMod3))  ## residuals should line up pretty closely to the blue line
hist(residuals(CECMod3))
plot(fitted(CECMod3), residuals(CECMod3))

# This model is not perfect but not terrible. I am going to stick with the original 
# data here. The model residuals have a very close to normal distribution. 

########################### CEC Model Results ###################################

Anova(CECMod3, type="III")

# Pairwise Treatment 
PairsCECMod <- emmeans(CECMod3, ~Treatment, type='response') 
TRT_Pairs_CEC <- pairs(PairsCECMod)
CEC_Tibble <- as_tibble(summary(TRT_Pairs_CEC))
CI_Letters_CEC <- cld(PairsCECMod, Letters=letters, sort=TRUE, decreasing=TRUE)

################################# S ###########################################

########## Visualize the S Data ##### Decide on Outliers NOW ##################

# Original Data 
ggplot(SoilsData, aes(x=S)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(S))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(S))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=S)) + 
  geom_boxplot(notch=TRUE)

SMod2 <- glmmTMB(log(S) ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment) + (1|Site:Treatment:Position), 
                  data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(SMod2))
hist(simulateResiduals(SMod2)) ## histogram should be flat
qqPlot(resid(SMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(SMod2))
plot(fitted(SMod2), residuals(SMod2))


SMod3 <- glmmTMB(S ~ Treatment * Position * Year + (1|Site) + 
                     (1|Site:Treatment) + (1|Site:Treatment:Position), 
                   data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(SMod3))
hist(simulateResiduals(CECMod3)) ## histogram should be flat
qqPlot(resid(CECMod3))  ## residuals should line up pretty closely to the blue line
hist(residuals(SMod3))
plot(fitted(CECMod3), residuals(CECMod3))

# SMod3 (with data not log transformed) produced right-skewed model residuals 
# log transformation improved model fit, creating normally distributed model residuals 

########################### S Model Results ###################################

Anova(SMod2, type="III")

# Pairwise Treatment 
PairsSMod <- emmeans(SMod2, ~Treatment, type='response') 
TRT_Pairs_S <- pairs(PairsSMod)
S_Tibble <- as_tibble(summary(TRT_Pairs_S))
CI_Letters_S <- cld(PairsSMod, Letters=letters, sort=TRUE, decreasing=TRUE)


# Pairwise Year 
PairsSMod2 <- emmeans(SMod2, ~Year, type='response') 
Yr_Pairs_S <- pairs(PairsSMod2)
S_Tibble2 <- as_tibble(summary(Yr_Pairs_S))
CI_Letters_S2 <- cld(PairSMod2, Letters=letters, sort=TRUE, decreasing=TRUE)

############################### Fe #####################################

############ Visualize the Fe Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=Fe)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Fe))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Fe))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=Fe, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# log-transformed data did produce the most normal data distribution, but will 
# test both model formulas to see which had the best fit with respect to residuals 

FeMod2 <- glmmTMB(log(Fe) ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(FeMod2))
hist(simulateResiduals(FeMod2)) ## histogram should be flat
qqPlot(resid(FeMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(FeMod2))
plot(fitted(FeMod2), residuals(FeMod2))

FeMod3 <- glmmTMB(Fe ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian()) # this model did not converge 

########################### Fe Model Results ###################################

Anova(FeMod2, type="III")

# 3-way interaction effect between Treatment , Position , and Year 

# Pairwise Treatment x Year | Position
PairsFeMod <- emmeans(FeMod2, ~Treatment * Year | Position, type='response') 

CI_Letters_Fe <- cld(PairsFeMod, Letters=letters, sort=TRUE, decreasing=TRUE)

############################### Na #####################################

############ Visualize the Na Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=Na)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(Na))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(Na))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=Na, fill=Position)) + 
  geom_boxplot(notch=TRUE)


# log-transformed data did produce the most normal data distribution, but will 
# test both model formulas to see which had the best fit with respect to residuals 


NaMod3 <- glmmTMB(Na ~ Treatment * Position * Year + (1|Site) + 
                   (1|Site:Treatment) + (1|Site:Treatment:Position), 
                 data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(NaMod3))
hist(simulateResiduals(NaMod3)) ## histogram should be flat
qqPlot(resid(NaMod3))  ## residuals should line up pretty closely to the blue line
hist(residuals(NaMod3))
plot(fitted(NaMod3), residuals(NaMod3))

# The above model without log transformation did converge but the residuals do not 
# look good. Now will try the same model with the data log transformed 

NaMod2 <- glmmTMB(log(Na) ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment) + (1|Site:Treatment:Position), 
                  data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(NaMod2))
hist(simulateResiduals(NaMod2)) ## histogram should be flat
qqPlot(resid(NaMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(NaMod2))
plot(fitted(NaMod2), residuals(NaMod2))

# Log transformation greatly improved model fit 

########################### Na Model Results ###################################

Anova(NaMod2, type="III")

# 3-way interaction effect between Treatment , Position , and Year 

# Pairwise Treatment * Year | Position 
PairsNaMod <- emmeans(NaMod2, ~Treatment * Year | Position, type='response') 
TRTPosYr_Pairs_Na <- pairs(PairsNaMod)
Na_Tibble <- as_tibble(summary(TRTPosYr_Pairs_Na))
CI_Letters_Na <- cld(PairsNaMod, Letters=letters, sort=TRUE, decreasing=TRUE)

############################### OM #####################################

############ Visualize the OM Data ##### Decide on Outliers NOW ##########

# Original Data 
ggplot(SoilsData, aes(x=OM)) +
  geom_histogram()

# log-transformed data 
ggplot(SoilsData, aes(x=log(OM))) +
  geom_histogram()

# sqrt transformed data 
ggplot(SoilsData, aes(x=sqrt(OM))) +
  geom_histogram()

ggplot(SoilsData, aes(x=Treatment, y=OM, fill=Position)) + 
  geom_boxplot(notch=TRUE)

# log-transformed data did produce the most normal data distribution, but will 
# test both model formulas to see which had the best fit with respect to residuals 


OMMod3 <- glmmTMB(OM ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment) + (1|Site:Treatment:Position), 
                  data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(OMMod3))
hist(simulateResiduals(OMMod3)) ## histogram should be flat
qqPlot(resid(OMMod3))  ## residuals should line up pretty closely to the blue line
hist(residuals(OMMod3))
plot(fitted(OMMod3), residuals(OMMod3))

# The above model without log transformation did converge but the residuals do not 
# look good (they actually look terrible!). Now will try the same model with the data 
# log transformed.

OMMod2 <- glmmTMB(log(OM) ~ Treatment * Position * Year + (1|Site) + 
                    (1|Site:Treatment) + (1|Site:Treatment:Position), 
                  data=SoilsData, family=gaussian())

# Now look at the residuals 
plot(simulateResiduals(OMMod2))
hist(simulateResiduals(OMMod2)) ## histogram should be flat
qqPlot(resid(OMMod2))  ## residuals should line up pretty closely to the blue line
hist(residuals(OMMod2))
plot(fitted(OMMod2), residuals(OMMod2))

# Log transformation greatly improved model fit 

########################### OM Model Results ###################################

Anova(OMMod2, type="III")

# Only Treatment and Position Significant (No sig.interaction effects)

# Pairwise Treatment 
PairsOMMod_TRT <- emmeans(OMMod2, ~Treatment, type='response') 
TRT_Pairs_OM <- pairs(PairsOMMod_TRT)
OM_Tibble <- as_tibble(summary(TRT_Pairs_OM))
CI_Letters_OM_TRT <- cld(PairsOMMod_TRT, Letters=letters, sort=TRUE, decreasing=TRUE)

# Pairwise Position
PairsOMMod_POS <- emmeans(OMMod2, ~Position, type='response') 
POS_Pairs_OM <- pairs(PairsOMMod_POS)
OM_Tibble2 <- as_tibble(summary(POS_Pairs_OM))
CI_Letters_OM_POS <- cld(PairsOMMod_POS, Letters=letters, sort=TRUE, decreasing=TRUE)




