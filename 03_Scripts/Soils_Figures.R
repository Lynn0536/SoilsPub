################################################################################
############# Graphing Results from Soil GLMM Results ##########################


# Library 

library(ggplot2)
library(ggpubr)
library(Rmisc)
library(tidyverse)
library(cowplot)

# Call in Raw Data 
RawData <- read_csv("02_CleanData/EPA_Soil_CLEAN.csv",
                    col_types = cols(Year = col_factor(),
                                     Position = col_factor(),
                                     Treatment = col_factor(),
                                     Site = col_factor()))

SoilsData <- RawData %>%
  filter(Treatment != "Scraped") %>%
  filter(Treatment != "Burned") %>%
  filter(Treatment != "Scraped + Burned")


################## Mg ###########################################################
## Remove row if there is an NA Value in the "Mg" column 
MgData <- SoilsData %>%
  drop_na(Mg) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

Mg_Summary <- summarySE(MgData, measurevar="Mg", groupvars=c("Treatment"))

MgTEXTData <- MgData %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(MaxMg = max(Mg)) %>%
  mutate(Letters = c("a","b","b","c","c","c"))

Box_Mg <- ggplot(MgData, aes(Treatment, Mg, fill=Treatment)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.Mg = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Available Mg (mg/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,325), breaks = seq(0,325, by = 50)) +
  geom_text(data=MgTEXTData, mapping=aes(y=MaxMg+25, x=Treatment, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14),
        legend.position="none") 
  #coord_flip()

############################# Ca ################################################
## Remove row if there is an NA Value in the "Ca" column 
CaData <- SoilsData %>%
  drop_na(Ca) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

Ca_Summary <- summarySE(CaData, measurevar="Ca", groupvars=c("Treatment"))

CaTEXTData <- CaData %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(MaxCa = max(Ca)) %>%
  mutate(Letters = c("a","a","a","b","b","b"))


Box_Ca <- ggplot(CaData, aes(Treatment, Ca, fill=Treatment)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.Ca = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Available Ca (mg/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,999), breaks = seq(0,999, by = 200)) +
  geom_text(data=CaTEXTData, mapping=aes(y=MaxCa+50, x=Treatment, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14),
        legend.position="none") 
  #coord_flip()

############################# CEC ###############################################
## Remove row if there is an NA Value in the "CEC" column 
CECData <- SoilsData %>%
  drop_na(CEC) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

CEC_Summary <- summarySE(CECData, measurevar="CEC", groupvars=c("Treatment"))

CECTEXTData <- CECData %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(MaxCEC = max(CEC)) %>%
  mutate(Letters = c("a","b","b","b","c","c"))


Box_CEC <- ggplot(CECData, aes(Treatment, CEC, fill=Treatment)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.CEC = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("CEC (meq/100g)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,20), breaks = seq(0,20, by = 5)) +
  geom_text(data=CECTEXTData, mapping=aes(y=MaxCEC+2, x=Treatment, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14),
        legend.position="none") 
  #coord_flip()

############################# S ###############################################
## Remove row if there is an NA Value in the "S" column 
SData <- SoilsData %>%
  drop_na(S) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

S_Summary <- summarySE(SData, measurevar="S", groupvars=c("Treatment"))

STEXTData <- SData %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(MaxS = max(S)) %>%
  mutate(Letters = c("a","ab","abc","bc","c","a"))


Box_S <- ggplot(SData, aes(Treatment, S, fill=Treatment)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.S = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Available S (mg/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  geom_text(data=STEXTData, mapping=aes(y=MaxS+4, x=Treatment, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14),
        legend.position="none") 
  #coord_flip()

############################# OM ###############################################
## Remove row if there is an NA Value in the "OM" column 
OMData <- SoilsData %>%
  drop_na(OM) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

OM_Summary <- summarySE(OMData, measurevar="OM", groupvars=c("Treatment"))

OMTEXTData <- OMData %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(MaxOM = max(OM)) %>%
  mutate(Letters = c("a","b","b","c","c","b"))


Box_OM <- ggplot(OMData, aes(Treatment, OM, fill=Treatment)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.OM = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("OM (%)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,60), breaks = seq(0,60, by = 10)) +
  geom_text(data=OMTEXTData, mapping=aes(y=MaxOM+8, x=Treatment, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), 
        axis.title.x = element_text(size=14, vjust = 0.1),
        axis.title.y = element_text(size= 11),
        legend.title = element_text(size=14),
        legend.position="none") 
#coord_flip()

FigX <- plot_grid(Box_Mg, Box_Ca, Box_CEC, Box_S, Box_OM, 
                    hjust=-7, nrow=5, label_size = 14)

FigXB <- add_sub(FigX, "Treatment", hjust=0.1)

ggdraw(FigXDP)  
