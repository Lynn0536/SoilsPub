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

#################### Treatment Main Effect Significant ##########################
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


########################### Treatment x Year ####################################
################## Total_N ######################################################

## Remove row if there is an NA Value in the "Total_N" column 
TNData <- SoilsData %>%
  drop_na(Total_N) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

TN_Summary <- summarySE(TNData, measurevar="Total_N", groupvars=c("Treatment", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                       Year == '2023' ~ '1-Year Post Treatment (2023)',
                       Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

TN <- ggplot(TN_Summary, aes(x=Time, y=Total_N, group = Treatment, color=Treatment))+ 
    geom_errorbar(aes(ymin=Total_N-se, ymax=Total_N+se), width=.2, 
                  position=position_dodge(0.0)) +
    geom_line(linewidth=1) + 
    geom_point(aes(shape=Treatment), size=4)+
    labs(x="Time", y = "Total N (units)")+
    theme_classic() + 
    scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
    scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
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
  
  
################## Total_P ######################################################  
TPData <- SoilsData %>%
  drop_na(Total_P) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

TP_Summary <- summarySE(TPData, measurevar="Total_P", groupvars=c("Treatment", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

TP <- ggplot(TP_Summary, aes(x=Time, y=Total_P, group = Treatment, color=Treatment))+ 
  geom_errorbar(aes(ymin=Total_P-se, ymax=Total_P+se), width=.2, 
                position=position_dodge(0.0)) +
  geom_line(linewidth=1) + 
  geom_point(aes(shape=Treatment), size=4)+
  labs(x="Time", y = "Total P (units)")+
  theme_classic() + 
  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14),
        legend.position=c(0.7,0.8)) 


################## Total_K ######################################################  
TKData <- SoilsData %>%
  drop_na(Total_K) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

TK_Summary <- summarySE(TPData, measurevar="Total_K", groupvars=c("Treatment", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

TK <- ggplot(TK_Summary, aes(x=Time, y=Total_K, group = Treatment, color=Treatment))+ 
  geom_errorbar(aes(ymin=Total_K-se, ymax=Total_K+se), width=.2, 
                position=position_dodge(0.0)) +
  geom_line(linewidth=1) + 
  geom_point(aes(shape=Treatment), size=4)+
  labs(x="Time", y = "Total K (units)")+
  theme_classic() + 
  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
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

################## Cl ######################################################  
ClData <- SoilsData %>%
  drop_na(Cl) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

Cl_Summary <- summarySE(ClData, measurevar="Cl", groupvars=c("Treatment", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

Cl <- ggplot(Cl_Summary, aes(x=Time, y=Cl, group = Treatment, color=Treatment))+ 
  geom_errorbar(aes(ymin=Cl-se, ymax=Cl+se), width=.2, 
                position=position_dodge(0.0)) +
  geom_line(linewidth=1) + 
  geom_point(aes(shape=Treatment), size=4)+
  labs(x="Time", y = "Cl (units)")+
  theme_classic() + 
  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= 11),
        legend.title = element_text(size=14),
        legend.position="none") 

################## Bulk Density  ######################################################  
BDData <- SoilsData %>%
  drop_na(BulkDensity) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

BD_Summary <- summarySE(BDData, measurevar="BulkDensity", groupvars=c("Treatment", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

BD <- ggplot(BD_Summary, aes(x=Time, y=BulkDensity, group = Treatment, color=Treatment))+ 
  geom_errorbar(aes(ymin=BulkDensity-se, ymax=BulkDensity+se), width=.2, 
                position=position_dodge(0.0)) +
  geom_line(linewidth=1) + 
  geom_point(aes(shape=Treatment), size=4)+
  labs(x="Time", y = "Bulk Density (units)")+
  theme_classic() + 
  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= 11),
        legend.title = element_text(size=14),
        legend.position="none") 

################## pH  ######################################################  
pHData <- SoilsData %>%
  drop_na(pHw) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

pH_Summary <- summarySE(pHData, measurevar="pHw", groupvars=c("Treatment", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

pH <- ggplot(pH_Summary, aes(x=Time, y=pHw, group = Treatment, colour=Treatment))+ 
  geom_errorbar(aes(ymin=pHw-se, ymax=pHw+se), width=.2, 
                position=position_dodge(0.0)) +
  geom_line(linewidth=1) + 
  geom_point(aes(shape=Treatment), size=4)+
  labs(x="Time", y = "pH")+
  theme_classic() + 
  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= 11),
        legend.title = element_text(size=14),
        legend.position="none") 

# arrange the six plots in 2 rows
prow <- plot_grid(
  TN + theme(legend.position="none"),
  TP + theme(legend.position=c(0.7,0.8)),
  TK + theme(legend.position="none"),
  Cl + theme(legend.position="none"),
  BD + theme(legend.position="none"),
  pH + theme(legend.position="none"),
    align = 'vh',
  labels = c("A", "B", "C","D","E","F"),
  hjust = -1,
  nrow = 2
)
prow

################## Treatment x Position X Year #################################
########################### Fe #################################################
FeData <- SoilsData %>%
  drop_na(Fe)

Fe_Summary <- summarySE(FeData, measurevar="Fe", groupvars=c("Treatment","Position", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)")) %>%
  mutate(TRT = case_when(Treatment == 'Control' ~ 'Control',
                         Treatment == 'Cleared' ~ 'C',
                         Treatment == 'Cleared + Burned' ~ 'C + B',
                         Treatment == 'Cleared + Scraped' ~ 'C + S',
                         Treatment == 'Cleared + Scraped + Burned' ~ 'C + S + B',
                         Treatment == 'Reference' ~ 'Reference')) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                                 "C + S", "C + S + B"))

Fe_Summary <- Fe_Summary %>%
    mutate(Letters = c("abcde","abcde","abc","ab","ab","ab","abcde","abcde","abcd","ab","ab","ab", "abcde",
                       "abcde", "abcde", "ab", "ab", "ab", "de", "bcde", "de", "b", "ab", "a", 
                       "e", "de", "cde", "ab", "ab", "ab", "ab", "a", "a", "ab", "ab", "ab"))

ggplot(Fe_Summary, aes(x=TRT, y=Fe, group=Time)) +
  geom_col(aes(fill=Time),position = "dodge") + 
  geom_errorbar(aes(ymin=Fe-se, ymax=Fe+se), width=.3, position=position_dodge(0.9)) +
  geom_text(aes(y=Fe+se+6, label=Letters), position=position_dodge(0.9)) +
  scale_fill_manual(values=c("lightcyan",'lightblue3','lightcyan4')) +
  xlab("Restoration Treatment") +
  ylab("Fe (units)") +
  theme_bw() +
  theme(panel.border = element_rect(color="black", fill=NA, size=1), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 16, vjust = -1),
        axis.title.y = element_text(size = 16, vjust = +3),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  facet_wrap(~factor(Position, c("Upper", "Bottom"))) +
  theme(strip.text = element_text(size=12))



# Condition X Depth #### Woody / Graminoid / Forb ##############################
figDATA <- CleanData %>%
  dplyr::filter(!Group=="NN") %>%
  dplyr::group_by(Site, Site_Condition, Group, Depth) %>%
  dplyr::summarize(Sum = sum(No_of_Individuals))

# bar graph with mean + SE bars 
figDATA2 <- summarySE(figDATA, measurevar="Sum", groupvars=c("Group","Site_Condition","Depth")) 

figDATA2 <- figDATA2 %>%
  dplyr::mutate(Depth = case_when(Depth == '1' ~ '1-Surface',
                                  Depth == '2' ~ '2-Mineral Soil')) %>%
  dplyr::mutate(Letters = c("a","b","b","b","a","b","b","b","b","b","a","b"))

ggplot(figDATA2, aes(x=Depth, y=Sum, group=Site_Condition)) +
  geom_col(aes(fill=Site_Condition),position = "dodge") + 
  geom_errorbar(aes(ymin=Sum-se, ymax=Sum+se), width=.3, position=position_dodge(0.9)) +
  geom_text(aes(y=Sum+se+3, label=Letters), position=position_dodge(0.9)) +
  scale_fill_manual(values=c('lightblue2','grey44')) +
  xlab("Sample Depth") +
  ylab("Mean Seedling Emergence") +
  theme_bw() +
  theme(panel.border = element_rect(color="black", fill=NA, size=1), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 16, vjust = -1),
        axis.title.y = element_text(size = 16, vjust = +3),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  facet_wrap(~factor(Group, c("Woody", "Forb", "Graminoid"))) +
  theme(strip.text = element_text(size=12))





########################### Position x Year  ####################################
##### Total_N # Total_P # Total_K Only ##########################################
########## Table for this rather than figure ####################################

TNData <- SoilsData %>%
  drop_na(Total_N) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

TN_Pos_Year_Summary <- summarySE(TNData, measurevar="Total_N", groupvars=c("Position", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))


TPData <- SoilsData %>%
  drop_na(Total_P) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

TP_Pos_Year_Summary <- summarySE(TPData, measurevar="Total_P", groupvars=c("Position", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))


TKData <- SoilsData %>%
  drop_na(Total_K) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))

TK_Pos_Year_Summary <- summarySE(TKData, measurevar="Total_K", groupvars=c("Position", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ 'Initial (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "Initial (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))


