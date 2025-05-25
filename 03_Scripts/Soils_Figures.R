################################################################################
################################################################################
###                        Graphing Soil GLMM Results                        ###
###                          EPA-Funded Soil Data                            ###
###                        Code by: Ashlynn N. Smith                         ###
###                     USEPA Project Number: GC-00D80521                    ###
###    Joint Project between Atlanta Botanical Garden & University of FL     ###
################################################################################


# Load Packages from the Library 

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
  filter(Treatment != "Scraped + Burned") %>%
  mutate(TRT = case_when(Treatment == 'Control' ~ 'Control',
                         Treatment == 'Cleared' ~ 'C',
                         Treatment == 'Cleared + Burned' ~ 'C + B',
                         Treatment == 'Cleared + Scraped' ~ 'C + S',
                         Treatment == 'Cleared + Scraped + Burned' ~ 'C + S + B',
                         Treatment == 'Reference' ~ 'Reference')) 


#################### Treatment Main Effect Significant ##########################
################## Mg ###########################################################
## Remove row if there is an NA Value in the "Mg" column 
MgData <- SoilsData %>%
  drop_na(Mg) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

Mg_Summary <- summarySE(MgData, measurevar="Mg", groupvars=c("TRT"))

MgTEXTData <- MgData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxMg = max(Mg)) %>%
  mutate(Letters = c("a","b","b","c","c","c"))

Box_Mg <- ggplot(MgData, aes(TRT, Mg, fill=TRT)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.Mg = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Available Mg (mg/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,325), breaks = seq(0,325, by = 100)) +
  geom_text(data=MgTEXTData, mapping=aes(y=MaxMg+25, x=TRT, label=Letters), size=5) +
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
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

Ca_Summary <- summarySE(CaData, measurevar="Ca", groupvars=c("TRT"))

CaTEXTData <- CaData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxCa = max(Ca)) %>%
  mutate(Letters = c("a","a","a","b","b","b"))


Box_Ca <- ggplot(CaData, aes(TRT, Ca, fill=TRT)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  geom_point(data=Ca_Summary, mapping=aes(x=TRT, y=Ca, fill="black"), shape=17, size=3,
             position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Available Ca (mg/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,999), breaks = seq(0,999, by = 200)) +
  geom_text(data=CaTEXTData, mapping=aes(y=MaxCa+100, x=TRT, label=Letters), size=5) +
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
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

CEC_Summary <- summarySE(CECData, measurevar="CEC", groupvars=c("TRT"))

CECTEXTData <- CECData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxCEC = max(CEC)) %>%
  mutate(Letters = c("a","b","b","c","c","b"))


Box_CEC <- ggplot(CECData, aes(TRT, CEC, fill=TRT)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.CEC = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("CEC (meq/100g)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,20), breaks = seq(0,20, by = 5)) +
  geom_text(data=CECTEXTData, mapping=aes(y=MaxCEC+2, x=TRT, label=Letters), size=5) +
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

############################# S #################################################
## Remove row if there is an NA Value in the "S" column 
SData <- SoilsData %>%
  drop_na(S) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

S_Summary <- summarySE(SData, measurevar="S", groupvars=c("TRT"))

S_Summary_Yr <- summarySE(SData, measurevar="S", groupvars=c("Year"))

STEXTData <- SData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxS = max(S)) %>%
  mutate(Letters = c("a","ab","abc","bc","c","a"))


Box_S <- ggplot(SData, aes(TRT, S, fill=TRT)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.S = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Available S (mg/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50, by = 10)) +
  geom_text(data=STEXTData, mapping=aes(y=MaxS+4, x=TRT, label=Letters), size=5) +
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
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

OM_Summary <- summarySE(OMData, measurevar="OM", groupvars=c("TRT"))

OMTEXTData <- OMData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxOM = max(OM)) %>%
  mutate(Letters = c("a","b","b","c","c","b"))


Box_OM <- ggplot(OMData, aes(TRT, OM, fill=TRT)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  stat_summary(fun.OM = "mean", geom="point", color="black", shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("OM (%)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,60), breaks = seq(0,60, by = 10)) +
  geom_text(data=OMTEXTData, mapping=aes(y=MaxOM+8, x=TRT, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= 11),
        legend.title = element_text(size=14),
        legend.position="none") 
#coord_flip()

############################# Total_N ###############################################
## Remove row if there is an NA Value in the "Total_N" column 
TNData <- SoilsData %>%
  drop_na(Total_N) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference')) %>%
  mutate(TN = Total_N*10)

TN_Summary <- summarySE(TNData, measurevar="TN", groupvars=c("TRT"))

TNTEXTData <- TNData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxTN = max(TN)) %>%
  mutate(Letters = c("a","b","b","c","c","b"))


Box_TN <- ggplot(TNData, aes(TRT, TN, fill=TRT)) +
  geom_boxplot(outlier.size=0.5, width=0.3) +
  geom_point(data=TN_Summary, mapping=aes(x=TRT, y=TN, fill="black"), shape=17, size=3,
               position= position_dodge2(width=0.75, preserve ="single")) +
  scale_fill_brewer(palette="Pastel2") +
  #scale_fill_manual(values=c("#666666", "#336666", "#006699", "#339999", "#33CCCC", "#33FFFF")) +
  xlab("Treatment") +
  ylab("Total N (g/kg)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,15), breaks = seq(0,15, by = 5.0)) +
  geom_text(data=TNTEXTData, mapping=aes(y=MaxTN+0.5, x=TRT, label=Letters), size=5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size=14, vjust = 0.1),
        axis.title.y = element_text(size= 11),
        legend.title = element_text(size=14),
        legend.position="none") 

#coord_flip()
# 
FigX <- plot_grid(Box_Mg, Box_Ca, Box_S, Box_CEC, Box_OM, Box_TN,
                    hjust=-7, nrow=6, label_size = 14)

FigXB <- add_sub(FigX, "Treatment", hjust=0.1)

ggdraw(FigX)  


########################### Treatment x Year ####################################

################## Total_P ######################################################  
TPData <- SoilsData %>%
  drop_na(Total_P) %>%
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                                 "C + S", "C + S + B"))

TP_Summary <- summarySE(TPData, measurevar="Total_P", groupvars=c("TRT", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ '2-Weeks Post Treatment (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "2-Weeks Post Treatment (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

#TP <- ggplot(TP_Summary, aes(x=Time, y=Total_P, group = Treatment, color=Treatment))+ 
  #geom_errorbar(aes(ymin=Total_P-se, ymax=Total_P+se), width=.2, 
                #position=position_dodge(0.0)) +
  #geom_line(linewidth=1) + 
  #geom_point(aes(shape=Treatment), size=4)+
  #labs(x="Time", y = "Total P (mg/kg)")+
  #theme_classic() + 
  #scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  #scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
  #theme(panel.border = element_blank(), 
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #axis.line = element_line(colour = "black"),
        #axis.text = element_text(size = 11),
        #axis.text.x = element_blank(), 
        #axis.title.y = element_text(size = 11),
        #axis.title.x = element_blank(),
        #legend.title = element_text(size=14),
        #legend.position=c(0.7,0.8)) 

TP2 <- ggplot(TP_Summary, aes(x=TRT, y=Total_P, group = Time))+ 
  geom_errorbar(aes(ymin=Total_P-se, ymax=Total_P+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=Total_P, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Total P (mg/kg)")+
  geom_rect(aes(ymin=58.75,ymax=165, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=22.05,ymax=81.81, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_text(size=20))

################## Total_K ######################################################  
TKData <- SoilsData %>%
  drop_na(Total_K) %>%
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                           "C + S", "C + S + B"))

TK_Summary <- summarySE(TPData, measurevar="Total_K", groupvars=c("TRT", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ '2-Weeks Post Treatment (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "2-Weeks Post Treatment (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

#TK <- ggplot(TK_Summary, aes(x=Time, y=Total_K, group = Treatment, color=Treatment))+ 
 # geom_errorbar(aes(ymin=Total_K-se, ymax=Total_K+se), width=.2, 
  #              position=position_dodge(0.0)) +
  #geom_line(linewidth=1) + 
  #geom_point(aes(shape=Treatment), size=4)+
  #labs(x="Time", y = "Total K (mg/kg)")+
  #theme_classic() + 
  #scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
  #scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
  #theme(panel.border = element_blank(), 
   #     panel.grid.major = element_blank(),
    #    panel.grid.minor = element_blank(),
     #   axis.line = element_line(colour = "black"),
      #  axis.text = element_text(size = 11),
       # axis.text.x = element_blank(), 
      #  axis.title.y = element_text(size = 11),
      #  axis.title.x = element_blank(),
      #  legend.title = element_text(size=14),
      #  legend.position="none") 


TK2 <- ggplot(TK_Summary, aes(x=TRT, y=Total_K, group = Time))+ 
  geom_errorbar(aes(ymin=Total_K-se, ymax=Total_K+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=Total_K, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Total K (mg/kg)")+
  geom_rect(aes(ymin=89.76,ymax=331.13, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=45.92,ymax=101.36, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_text(size=20))


################## Bulk Density  ######################################################  
BDData <- SoilsData %>%
  drop_na(BulkDensity) %>%
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                           "C + S", "C + S + B"))
max(BDData$BulkDensity)
min(BDData$BulkDensity)

BD_Summary <- summarySE(BDData, measurevar="BulkDensity", groupvars=c("TRT", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ '2-Weeks Post Treatment (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "2-Weeks Post Treatment (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

# BD <- ggplot(BD_Summary, aes(x=Time, y=BulkDensity, group = Treatment, color=Treatment))+ 
#  geom_errorbar(aes(ymin=BulkDensity-se, ymax=BulkDensity+se), width=.2, 
#                position=position_dodge(0.0)) +
#  geom_line(linewidth=1) + 
#  geom_point(aes(shape=Treatment), size=4)+
#  labs(x="Time", y = "Bulk Density (g/cm3)")+
#  theme_classic() + 
#  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
#  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
#  scale_x_discrete(labels = function(x) 
#    stringr::str_wrap(x, width = 15))+
#  theme(panel.border = element_blank(), 
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.line = element_line(colour = "black"),
#        axis.text = element_text(size = 11),
#        axis.text.x = element_text(size = 10), 
#        axis.title.x = element_blank(),
#        axis.title.y = element_text(size= 11),
#        legend.title = element_text(size=14),
#        legend.position="none") 

BD2 <- ggplot(BD_Summary, aes(x=TRT, y=BulkDensity, group = Time))+ 
  geom_errorbar(aes(ymin=BulkDensity-se, ymax=BulkDensity+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=BulkDensity, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Bulk Density (g/cm3)")+
  geom_rect(aes(ymin=0.23,ymax=0.72, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=0.77,ymax=1.23, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_text(size=20))


################## pH  ######################################################  
pHData <- SoilsData %>%
  drop_na(pH) %>%
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                           "C + S", "C + S + B"))

pH_Summary <- summarySE(pHData, measurevar="pH", groupvars=c("TRT", "Year")) %>%
  mutate(Time = case_when(Year == '2022' ~ '2-Weeks Post Treatment (2022)',
                          Year == '2023' ~ '1-Year Post Treatment (2023)',
                          Year == '2024' ~ '2-Years Post Treatment (2024)')) %>%
  mutate(Time = fct_relevel(Time, "2-Weeks Post Treatment (2022)", "1-Year Post Treatment (2023)",
                            "2-Years Post Treatment (2024)"))

# pH <- ggplot(pH_Summary, aes(x=Time, y=pH, group = Treatment, colour=Treatment))+ 
#  geom_errorbar(aes(ymin=pH-se, ymax=pH+se), width=.2, 
#                position=position_dodge(0.0)) +
#  geom_line(linewidth=1) + 
#  geom_point(aes(shape=Treatment), size=4)+
#  labs(x="Time", y = "pH")+
#  theme_classic() + 
#  scale_color_manual(values=c('black','lightcyan2','lightcyan3','lightcyan4','darkcyan','goldenrod2')) +
#  scale_shape_manual(values=c(4, 18, 15, 16, 17, 8)) +
#  scale_x_discrete(labels = function(x) 
#    stringr::str_wrap(x, width = 15))+
#  theme(panel.border = element_blank(), 
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.line = element_line(colour = "black"),
#        axis.text = element_text(size = 11),
#        axis.text.x = element_text(size = 10), 
#        axis.title.x = element_blank(),
#        axis.title.y = element_text(size= 11),
#        legend.title = element_text(size=14),
#        legend.position="none") 

PH2 <- ggplot(pH_Summary, aes(x=TRT, y=pH, group = Time))+ 
  geom_errorbar(aes(ymin=pH-se, ymax=pH+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=pH, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "pH")+
  geom_rect(aes(ymin=3.86,ymax=4.13, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=4.14,ymax=4.76, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(size=16), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_blank())


##### Figure 3 ############## arrange the six plots in 2 rows
Fig3 <- plot_grid(
  TP2 + theme(legend.position=c(0.85,0.65)),
  TK2 + theme(legend.position="none"),
  BD2 + theme(legend.position="none"),
  PH2 + theme(legend.position="none"),
    align = 'vh',
  labels = c("A", "B", "C","D"),
  hjust = -0.9,
  vjust = 1.2,
  nrow = 4
)
Fig3

################## Treatment x Position X Year #################################
########################### Fe #################################################
FeData <- SoilsData %>%
  drop_na(Fe)

Fe_Upper <- FeData %>%
  filter(Position != "Bottom")
  
Fe_Up_Summary <- summarySE(Fe_Upper, measurevar="Fe", groupvars=c("Treatment","Position", "Year")) %>%
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
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                                 "C + S", "C + S + B"))

#Fe_Summary <- Fe_Summary %>%
 #   mutate(Letters = c("abcde","abcde","abc","ab","ab","ab","abcde","abcde","abcd","ab","ab","ab", "abcde",
                      # "abcde", "abcde", "ab", "ab", "ab", "de", "bcde", "de", "b", "ab", "a", 
                      # "e", "de", "cde", "ab", "ab", "ab", "ab", "a", "a", "ab", "ab", "ab"))

#Fe <- ggplot(Fe_Summary, aes(x=TRT, y=Fe, group=Time)) +
 # geom_col(aes(fill=Time),position = "dodge") + 
#  geom_errorbar(aes(ymin=Fe-se, ymax=Fe+se), width=.3, position=position_dodge(0.9)) +
#  geom_text(aes(y=Fe+se+10, label=Letters), position=position_dodge(0.9)) +
#  scale_fill_manual(values=c("lightcyan",'lightblue3','lightcyan4')) +
#  xlab("Restoration Treatment") +
#  ylab("Fe (mg/kg)") +
#  theme_bw() +
#  theme(panel.border = element_rect(color="black", fill=NA, size=1), 
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.line = element_line(colour = "black"),
#        axis.text.y = element_text(size = 16),
#        axis.text.x = element_blank(), 
#        axis.title.x = element_blank(),
#        axis.title.y = element_text(size = 16, vjust = +3),
#        legend.title=element_text(size=12), 
#        legend.text=element_text(size=12)) +
#  facet_wrap(~factor(Position, c("Upper", "Bottom"))) +
#  theme(strip.text = element_text(size=12))

Fe_Upper_2 <- ggplot(Fe_Up_Summary, aes(x=TRT, y=Fe, group = Time))+ 
  geom_errorbar(aes(ymin=Fe-se, ymax=Fe+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=Fe, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Fe (mg/kg)", title="Upper") +
  scale_y_continuous(limits=c(10,375), breaks = seq(50,375, by = 50)) +
  geom_rect(aes(ymin=50.4,ymax=275.3, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=148.5,ymax=362.7, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        plot.title = element_text(size=24, hjust=0.5),
        legend.title = element_text(size=16)) 

Fe_Bottom <- FeData %>%
  filter(Position != "Upper")

Fe_B_Summary <- summarySE(Fe_Bottom, measurevar="Fe", groupvars=c("Treatment","Position", "Year")) %>%
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
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                           "C + S", "C + S + B"))

Fe_Bottom_2 <- ggplot(Fe_B_Summary, aes(x=TRT, y=Fe, group = Time))+ 
  geom_errorbar(aes(ymin=Fe-se, ymax=Fe+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=Fe, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Fe (mg/kg)", title="Bottom") +
  scale_y_continuous(limits=c(10,375), breaks = seq(50,375, by = 50)) +
  geom_rect(aes(ymin=96.2,ymax=365.4, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=99.5,ymax=200.0, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size=24, hjust=0.5),
        legend.title = element_text(size=16))

########################### Na #################################################
NaData <- SoilsData %>%
  drop_na(Na)

Na_Upper <- NaData %>%
  filter(Position != "Bottom")

Na_Up_Summary <- summarySE(Na_Upper, measurevar="Na", groupvars=c("Treatment","Position", "Year")) %>%
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
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                           "C + S", "C + S + B"))

# Na_Summary <- Na_Summary %>%
  # mutate(Letters = c("a","ab","abc","a","ab","bc", "bcd","bcd","abcd","cde","bcd","bcde","defg", "bcd",
                   #  "cdef", "bc", "bcd", "bc", "efg", "efg", "efg", "ef", "def", "def", "g", 
                   #  "fg", "efg", "f", "def", "f", "bcdefg", "bcde", "bcdefg", "bcde", "bcdef", "bcde"))

#Na <- ggplot(Na_Summary, aes(x=TRT, y=Na, group=Time)) +
  #geom_col(aes(fill=Time),position = "dodge") + 
  #geom_errorbar(aes(ymin=Na-se, ymax=Na+se), width=.3, position=position_dodge(0.9)) +
  #geom_text(aes(y=Na+se+2, label=Letters), position=position_dodge(0.9)) +
  #scale_fill_manual(values=c("lightcyan",'lightblue3','lightcyan4')) +
  #xlab("Restoration Treatment") +
  #ylab("Na (mg/kg)") +
  #theme_bw() +
  #theme(panel.border = element_rect(color="black", fill=NA, size=1), 
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #axis.line = element_line(colour = "black"),
        #axis.text.y = element_text(size = 16),
        #axis.text.x = element_text(size = 14), 
        #axis.title.x = element_text(size = 16, vjust = -1),
        #axis.title.y = element_text(size = 16, vjust = +3),
        #legend.title=element_text(size=12), 
        #legend.text=element_text(size=12)) +
  #facet_wrap(~factor(Position, c("Upper", "Bottom"))) +
  #theme(strip.text = element_text(size=12))

Na_Upper_2 <- ggplot(Na_Up_Summary, aes(x=TRT, y=Na, group = Time))+ 
  geom_errorbar(aes(ymin=Na-se, ymax=Na+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=Na, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Na (mg/kg)") +
  scale_y_continuous(limits=c(5,60), breaks = seq(0,60, by = 10)) +
  geom_rect(aes(ymin=21.8,ymax=53.6, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=13.1,ymax=19.7, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        legend.title = element_text(size=16)) 


Na_Bottom <- NaData %>%
  filter(Position != "Upper")

Na_B_Summary <- summarySE(Na_Bottom, measurevar="Na", groupvars=c("Treatment","Position", "Year")) %>%
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
  mutate(TRT = fct_relevel(TRT, "Control","Reference", "C", "C + B",
                           "C + S", "C + S + B"))


Na_Bottom_2 <- ggplot(Na_B_Summary, aes(x=TRT, y=Na, group = Time))+ 
  geom_errorbar(aes(ymin=Na-se, ymax=Na+se), width=.2, 
                position=position_dodge(0.5)) +
  geom_point(aes(x=TRT, y=Na, shape = Time), size=3, 
             position=position_dodge(0.5)) +
  labs(x="Treatment", y = "Na (mg/kg)") +
  scale_y_continuous(limits=c(5,60), breaks = seq(0,60, by = 10)) +
  geom_rect(aes(ymin=26.4,ymax=59.5, xmin=0, xmax=Inf), fill="bisque4", alpha=0.01)+
  geom_rect(aes(ymin=14.7,ymax=22.3, xmin=0, xmax=Inf), fill="lightgreen", alpha=0.01)+
  theme_classic() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(size = 14), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        legend.title = element_text(size=16)) 

# arrange the 4 plots in 2 rows
Fig4 <- plot_grid(
  Fe_Upper_2 + theme(legend.position="none"),
  Fe_Bottom_2 + theme(legend.position=c(-0.32,0.65)),
  Na_Upper_2 + theme(legend.position="none"),
  Na_Bottom_2 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A","","B", ""),
  hjust = -1,
  nrow = 2
)
Fig4


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

########################### Treatment X Position ####################################
############## Bulk Density # Total_K Only ##########################################
############## Table for this rather than figure ####################################

TKData <- SoilsData %>%
  drop_na(Total_K) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))


TK_Trt_Pos_Summary <- summarySE(TKData, measurevar="Total_K", groupvars=c("Treatment", "Position")) 


BDData <- SoilsData %>%
  drop_na(BulkDensity) %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", "Cleared", "Cleared + Burned",
                                 "Cleared + Scraped", "Cleared + Scraped + Burned"))


BD_Trt_Pos_Summary <- summarySE(BDData, measurevar="BulkDensity", groupvars=c("Treatment", "Position")) 


############# OM X Position Only - in-text discussion only #####################
OMData <- SoilsData %>%
  drop_na(OM) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

OM_Summary2 <- summarySE(OMData, measurevar="OM", groupvars=c("Position"))

################ Conversion of Fig 3 to Table ##################################
################ Significant Treatment Effect ONLY #############################

## Mg 

## Remove row if there is an NA Value in the "Mg" column 
MgData <- SoilsData %>%
  drop_na(Mg) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

Mg_Summary <- summarySE(MgData, measurevar="Mg", groupvars=c("TRT"))

MgMax <- MgData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxMg = max(Mg)) 

MgMin <- MgData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MinMg = min(Mg)) 

MedianMg <- MgData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MedianMg = median(Mg)) 


## Ca

## Remove row if there is an NA Value in the "Ca" column 
CaData <- SoilsData %>%
  drop_na(Ca) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

Ca_Summary <- summarySE(CaData, measurevar="Ca", groupvars=c("TRT"))

CaMax <- CaData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxCa = max(Ca)) 

CaMin <- CaData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MinCa = min(Ca)) 

MedianCa <- CaData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MedianCa = median(Ca)) 

## S

## Remove row if there is an NA Value in the "S" column 
SData <- SoilsData %>%
  drop_na(S) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

S_Summary <- summarySE(CaData, measurevar="S", groupvars=c("TRT"))

SMax <- SData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxS = max(S)) 

SMin <- SData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MinS = min(S)) 

MedianS <- SData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MedianS = median(S)) 

## OM

## Remove row if there is an NA Value in the "OM" column 
OMData <- SoilsData %>%
  drop_na(OM) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference'))

OM_Summary <- summarySE(CaData, measurevar="OM", groupvars=c("TRT"))

OMMax <- OMData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxOM = max(OM)) 

OMMin <- OMData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MinOM = min(OM)) 

MedianOM <- OMData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MedianOM = median(OM)) 

## Total N 

## Remove row if there is an NA Value in the "Total_N" column 
TNData <- SoilsData %>%
  drop_na(Total_N) %>%
  mutate(TRT = fct_relevel(TRT, "Control", "C", "C + B",
                           "C + S", "C + S + B", 'Reference')) %>%
  mutate(TN = Total_N*10) # converts to g/kg 

TN_Summary <- summarySE(TNData, measurevar="TN", groupvars=c("TRT"))

TNMax <- TNData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MaxTN = max(TN)) 

TNMin <- TNData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MinTN = min(TN)) 

MedianTN <- TNData %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(MedianTN = median(TN)) 

