################################################################################
###                   Generalized Linear Mixed Models                        ###
###                          EPA-Funded Soil Data                            ###
###                        Code by: Ashlynn N. Smith                         ###
###                           USEPA Project Number:                          ###
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
hist(residuals(NMod1))
plot(fitted(NMod2), residuals(NMod2))

########################### Total_N Model Results ##############################

Anova(NMod2, type="III")

# Pairwise Treatment x Year 
PairsNMod <- emmeans(NMod2, ~Treatment, type='response') 
pairs(PairsNMod)
CI_Letters_Rich <- cld(PairsNMod, Letters=letters, sort=TRUE, decreasing=TRUE)

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
PairsNMod <- emmeans(NMod1, ~Treatment|Position, type='response') 
pairs(PairsNMod)
CI_Letters_Rich <- cld(PairsNMod, Letters=letters, sort=TRUE, decreasing=TRUE)

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


################# Full Model Testing - Total_P ################################

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
PairsKMod <- emmeans(KMod2, ~Treatment|Position, type='response') 
pairs(PairsKMod)
CI_Letters_Rich <- cld(PairsKMod, Letters=letters, sort=TRUE, decreasing=TRUE)















