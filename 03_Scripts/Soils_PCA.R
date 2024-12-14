################################################################################
###                        Principle Component Analysis                      ###
###                          EPA-Funded Soil Data                            ###
###                        Code by: Ashlynn N. Smith                         ###
###                           USEPA Project Number:                          ###
###    Joint Project between Atlanta Botanical Garden & University of FL     ###
################################################################################

# Install / Load Packages 

install.packages("corrr")
install.packages("ggcorrplot")
#install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("ggfortify")
install.packages("Factoshiny")
devtools::install_github("arleyc/PCAtest")

library(FactoMineR)
library(corrr)
library(ggcorrplot)
library(tidyverse)
library(factoextra)
library(ggfortify)
library(Factoshiny)
library(ggplot2)
library(PCAtest)

################ Call in Clean Data ################## 

RawData <- read_csv("02_CleanData/EPA_Soil_CLEAN.csv") 

SoilsData <- RawData %>%
  filter(Treatment != "Scraped") %>%
  filter(Treatment != "Burned") %>%
  filter(Treatment != "Scraped + Burned") %>%
  dplyr:: select(c(UniqueID, Site, Treatment, Position, Year, Total_N, Total_P, 
                   Total_K, Cl, BulkDensity, Mg, Ca, pHw, CEC, S, Fe, Na, OM)) %>%
  mutate(ID = rownames(.)) 

SoilsData$ID <- as.numeric(SoilsData$ID)

summary(SoilsData$ID)   
str(SoilsData)

########### Step 1: Normalize Data: Normalizing the data : ) ###############

Soil_Norm <- scale(SoilsData[,6:18])
head(Soil_Norm)

######### Step 2: Compute the Correlation Matrix ###########################

Soil_corr_matrix <- cor(Soil_Norm)
ggcorrplot(Soil_corr_matrix)

######## Step 3: Applying PCA ######################################
set.seed(05151986)
PCA_Data_1 <- princomp(Soil_Norm)
summary(PCA_Data_1)
Zscores <- PCA_Data_1$scores

###### Step 4: Create A Loading Matrix #######################

Loadings <- as.data.frame(PCA_Data_1$loadings[, 1:4])
write_csv(Loadings, "04_Figures/PCA_Comp_Loadings.csv")

##### Step 5: Create a Scree Plot #############################

Scree_Plot <- fviz_screeplot(PCA_Data_1, addlabels = TRUE)

#### Step 6: BiPlot of the Attributes 

BiPlot <- fviz_pca_var(PCA_Data_1, col.var = "black")

#First, all the variables that are grouped together are positively correlated 
#to each other. 

#Next, the higher the distance between the variable and the origin, the better 
#represented that variable is. 

#Finally, variables that are negatively correlated are displayed to the opposite 
#sides of the biplot’s origin. 

#### Step 7: Visualize the contribution of each variable ######### 

fviz_cos2(PCA_Data_1, choice = "var", axes = 1:2)

#### Step 8: BiPlot Combined with COS2 #########################

fviz_pca_var(PCA_Data_1, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#### Step 9: Use PCAtest package to test the significance of PCs and loadings 

PCA_Test <- PCAtest(Soil_Norm, nperm = 1000, nboot = 1000, alpha = 0.05, indload = TRUE, 
                    varcorr = FALSE, counter = FALSE, plot = TRUE)

summary(PCA_Test)

#### Step 10: Visualize 

## 10.1 - Merge explanatory variables from SoilsData (columns 1-5) with Zscores (columns 1-2)

# Convert Zscores to a dataframe THEN add a column called 'UniqueID' made up of consecutive 
# numbers that will equal the number of rows. This will create a column the datasets can be joined by!
Scores <- as.data.frame(Zscores[, 1:2])
Scores$ID <- 1:nrow(Scores) 

JoinedData <- left_join(SoilsData, Scores, by = "ID")

## 10.2 - Now remove unneeded columns 

JoinedData <- JoinedData %>%
  dplyr::select(ID, Site, Treatment, Position, Year, Comp.1, Comp.2)

JoinedData <- JoinedData %>%
  mutate(Year = as_factor(Year)) 

## 10.3 - Create column called VAR in the Loadings data frame 

Loadings <- setNames(cbind(rownames(Loadings), Loadings, row.names = NULL), 
         c("VAR", "PC1", "PC2"))

Loadings <- Loadings %>%
  dplyr::select(VAR, PC1, PC2)

## 1.3 Graph 

POSITION <- ggplot(data=JoinedData) +
  geom_point(mapping=aes(x=Comp.1, y=Comp.2, color = Position), size=3) +
  #scale_shape_manual(values=shapes) +
  ggrepel::geom_text_repel(data=Loadings, aes(x = PC1*15, y = PC2*15, label = VAR), 
                           size=4) +
  #geom_text(data=TITB, aes(x = Can1, y = Can2, label = Position), 
            #size=7) +
  geom_segment(data = Loadings, 
               aes(x = 0, xend = PC1*15, y = 0, yend = PC2*15),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey40", inherit.aes = FALSE) +
  xlab("PC1 (54.8% of the variation explained)") +
  ylab("PC2 (14.8% of the variation explained)") +
  theme_classic() +
  scale_y_continuous(limits=c(-5,10), breaks = seq(-5,10, by = 1.0)) +
  scale_x_continuous(limits=c(-5,11), breaks = seq(-5,11, by = 1.0)) +  
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12), 
        axis.title = element_text(size=14),
        legend.title = element_text(size=12),
        legend.position=c(0.95,0.01),
        legend.justification=c("right", "bottom"),
        legend.direction="vertical") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

YEAR <- ggplot(data=JoinedData) +
  geom_point(mapping=aes(x=Comp.1, y=Comp.2, color = Year), size=3) +
  #scale_shape_manual(values=shapes) +
  ggrepel::geom_text_repel(data=Loadings, aes(x = PC1*15, y = PC2*15, label = VAR), 
                           size=4) +
  #geom_text(data=TITB, aes(x = Can1, y = Can2, label = Position), 
  #size=7) +
  geom_segment(data = Loadings, 
               aes(x = 0, xend = PC1*15, y = 0, yend = PC2*15),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey40", inherit.aes = FALSE) +
  xlab("PC1 (54.8% of the variation explained)") +
  ylab("PC2 (14.8% of the variation explained)") +
  theme_classic() +
  scale_y_continuous(limits=c(-5,10), breaks = seq(-5,10, by = 1.0)) +
  scale_x_continuous(limits=c(-5,11), breaks = seq(-5,11, by = 1.0)) +  
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12), 
        axis.title = element_text(size=14),
        legend.title = element_text(size=12),
        legend.position=c(0.95,0.01),
        legend.justification=c("right", "bottom"),
        legend.direction="vertical") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

TREATMENT <- ggplot(data=JoinedData) +
  geom_point(mapping=aes(x=Comp.1, y=Comp.2, color = Treatment), size=3) +
  #scale_shape_manual(values=shapes) +
  ggrepel::geom_text_repel(data=Loadings, aes(x = PC1*15, y = PC2*15, label = VAR), 
                           size=4) +
  #geom_text(data=TITB, aes(x = Can1, y = Can2, label = Position), 
  #size=7) +
  geom_segment(data = Loadings, 
               aes(x = 0, xend = PC1*15, y = 0, yend = PC2*15),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey40", inherit.aes = FALSE) +
  xlab("PC1 (54.8% of the variation explained)") +
  ylab("PC2 (14.8% of the variation explained)") +
  theme_classic() +
  scale_y_continuous(limits=c(-5,10), breaks = seq(-5,10, by = 1.0)) +
  scale_x_continuous(limits=c(-5,11), breaks = seq(-5,11, by = 1.0)) +  
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12), 
        axis.title = element_text(size=14),
        legend.title = element_text(size=12),
        legend.position=c(0.95,0.95),
        legend.justification=c("right", "top"),
        legend.direction="vertical") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)
