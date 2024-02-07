##################################################################
##            PCA PARALLELISM TEST (PROJECTED LDna DATA)        ##
##################################################################

#### Prepare the data ####

# Load the required libraries
library(ggplot2)
library(MASS)
library(dplyr)
library(tidyverse)
library(caret)
library(klaR)
library(ggpubr)
library(ggrepel)
library(reshape2)
library(ggdendro)
library(grid)
library(ggridges)
library(gdata)
# Set working directory
setwd("~/Dropbox/Carla's paper/ldna/projected")

# Import PCA dataframes
pca5 = read.table("cluster5 _PCAproj.txt", sep="\t")
pca6 = read.table("cluster6 _PCAproj.txt", sep="\t")
pca10 = read.table("cluster10 _PCAproj.txt", sep="\t")
pca11a = read.table("cluster11a _PCAproj.txt", sep="\t")
pca11b= read.table("cluster11b _PCAproj.txt", sep="\t")
pca11c = read.table("cluster11c _PCAproj.txt", sep="\t")
pca12 = read.table("cluster12 _PCAproj.txt", sep="\t")
pca13 = read.table("cluster13 _PCAproj.txt", sep="\t")
pca16 = read.table("cluster16 _PCAproj.txt", sep="\t")
pca18 = read.table("cluster18 _PCAproj.txt", sep="\t")
pca20 = read.table("cluster20 _PCAproj.txt", sep="\t")
pca22 = read.table("cluster22 _PCAproj.txt", sep="\t")
pca27a = read.table("cluster27a _PCAproj.txt", sep="\t")
pca27b = read.table("cluster27b _PCAproj.txt", sep="\t")

# Transform type into a factor
pca5$Type = as.factor(pca5$Type)
pca6$Type = as.factor(pca6$Type)
pca10$Type = as.factor(pca10$Type)
pca11a$Type = as.factor(pca11a$Type)
pca11b$Type = as.factor(pca11b$Type)
pca11c$Type = as.factor(pca11c$Type)
pca12$Type = as.factor(pca12$Type)
pca13$Type = as.factor(pca13$Type)
pca16$Type = as.factor(pca16$Type)
pca18$Type = as.factor(pca18$Type)
pca20$Type = as.factor(pca20$Type)
pca22$Type = as.factor(pca22$Type)
pca27a$Type = as.factor(pca27a$Type)
pca27b$Type = as.factor(pca27b$Type)

# Create the training and test data
train_pca5 = pca5[which((pca5$Concrete_Location=="Alaska" & pca5$Type=="Freshwater") | (pca5$Concrete_Location=="Alaska" | pca5$Concrete_Location=="East_Russia") & pca5$Type=="Marine"),]
test_pca5 = anti_join(pca5, train_pca5)
train_pca6 = pca6[which((pca6$Concrete_Location=="Alaska" & pca6$Type=="Freshwater") | (pca6$Concrete_Location=="Alaska" | pca6$Concrete_Location=="East_Russia") & pca6$Type=="Marine"),]
test_pca6 = anti_join(pca6, train_pca6)
train_pca10 = pca10[which((pca10$Concrete_Location=="Alaska" & pca10$Type=="Freshwater") | (pca10$Concrete_Location=="Alaska" | pca10$Concrete_Location=="East_Russia") & pca10$Type=="Marine"),]
test_pca10 = anti_join(pca10, train_pca10)
train_pca11a = pca11a[which((pca11a$Concrete_Location=="Alaska" & pca11a$Type=="Freshwater") | (pca11a$Concrete_Location=="Alaska" | pca11a$Concrete_Location=="East_Russia") & pca11a$Type=="Marine"),]
test_pca11a = anti_join(pca11a, train_pca11a)
train_pca11b = pca11b[which((pca11b$Concrete_Location=="Alaska" & pca11b$Type=="Freshwater") | (pca11b$Concrete_Location=="Alaska" | pca11b$Concrete_Location=="East_Russia") & pca11b$Type=="Marine"),]
test_pca11b = anti_join(pca11b, train_pca11b)
train_pca11c = pca11c[which((pca11c$Concrete_Location=="Alaska" & pca11c$Type=="Freshwater") | (pca11c$Concrete_Location=="Alaska" | pca11c$Concrete_Location=="East_Russia") & pca11c$Type=="Marine"),]
test_pca11c = anti_join(pca11c, train_pca11c)
train_pca12 = pca12[which((pca12$Concrete_Location=="Alaska" & pca12$Type=="Freshwater") | (pca12$Concrete_Location=="Alaska" | pca12$Concrete_Location=="East_Russia") & pca12$Type=="Marine"),]
test_pca12 = anti_join(pca12, train_pca12)
train_pca13 = pca13[which((pca13$Concrete_Location=="Alaska" & pca13$Type=="Freshwater") | (pca13$Concrete_Location=="Alaska" | pca13$Concrete_Location=="East_Russia") & pca13$Type=="Marine"),]
test_pca13 = anti_join(pca13, train_pca13)
train_pca16 = pca16[which((pca16$Concrete_Location=="Alaska" & pca16$Type=="Freshwater") | (pca16$Concrete_Location=="Alaska" | pca16$Concrete_Location=="East_Russia") & pca16$Type=="Marine"),]
test_pca16 = anti_join(pca16, train_pca16)
train_pca18 = pca18[which((pca18$Concrete_Location=="Alaska" & pca18$Type=="Freshwater") | (pca18$Concrete_Location=="Alaska" | pca18$Concrete_Location=="East_Russia") & pca18$Type=="Marine"),]
test_pca18 = anti_join(pca18, train_pca18)
train_pca20 = pca20[which((pca20$Concrete_Location=="Alaska" & pca20$Type=="Freshwater") | (pca20$Concrete_Location=="Alaska" | pca20$Concrete_Location=="East_Russia") & pca20$Type=="Marine"),]
test_pca20 = anti_join(pca20, train_pca20)
train_pca22 = pca22[which((pca22$Concrete_Location=="Alaska" & pca22$Type=="Freshwater") | (pca22$Concrete_Location=="Alaska" | pca22$Concrete_Location=="East_Russia") & pca22$Type=="Marine"),]
test_pca22 = anti_join(pca22, train_pca22)
train_pca27a = pca27a[which((pca27a$Concrete_Location=="Alaska" & pca27a$Type=="Freshwater") | (pca27a$Concrete_Location=="Alaska" | pca27a$Concrete_Location=="East_Russia") & pca27a$Type=="Marine"),]
test_pca27a = anti_join(pca27a, train_pca27a)
train_pca27b = pca27b[which((pca27b$Concrete_Location=="Alaska" & pca27b$Type=="Freshwater") | (pca27b$Concrete_Location=="Alaska" | pca27b$Concrete_Location=="East_Russia") & pca27b$Type=="Marine"),]
test_pca27b= anti_join(pca27b, train_pca27b)

# Preprocessing parameters
param5 = train_pca5 %>% preProcess(method=c("center","scale"))
param6 = train_pca6 %>% preProcess(method=c("center","scale"))
param10 = train_pca10 %>% preProcess(method=c("center","scale"))
param11a = train_pca11a %>% preProcess(method=c("center","scale"))
param11b = train_pca11b %>% preProcess(method=c("center","scale"))
param11c = train_pca11c %>% preProcess(method=c("center","scale"))
param12 = train_pca12 %>% preProcess(method=c("center","scale"))
param13 = train_pca13 %>% preProcess(method=c("center","scale"))
param16 = train_pca16 %>% preProcess(method=c("center","scale"))
param18 = train_pca18 %>% preProcess(method=c("center","scale"))
param20 = train_pca20 %>% preProcess(method=c("center","scale"))
param22 = train_pca22 %>% preProcess(method=c("center","scale"))
#param25 = train_pca25 %>% preProcess(method=c("center","scale"))
param27a = train_pca27a %>% preProcess(method=c("center","scale"))
param27b = train_pca27b %>% preProcess(method=c("center","scale"))


#### Model PC1 ####

# Make the model
#Select no of PCs which explain min 80%
model5 = qda(Type~PC1, data=train_pca5)
model6 = qda(Type~PC1, data=train_pca6)
model10 = qda(Type~PC1, data=train_pca10)
model11a = qda(Type~PC1, data=train_pca11a)
#model11b = qda(Type~PC1, data=train_pca11b) #rank deficiency in group freshwater
model11c = qda(Type~PC1, data=train_pca11c)
model12 = qda(Type~PC1, data=train_pca12)
model13 = qda(Type~PC1, data=train_pca13)
model16 = qda(Type~PC1, data=train_pca16)
model18 = qda(Type~PC1, data=train_pca18)
#model20 = qda(Type~PC1, data=train_pca20)
model22 = qda(Type~PC1, data=train_pca22)
#model25 = qda(Type~PC1, data=train_pca25)
model27a = qda(Type~PC1, data=train_pca27a)
model27b = qda(Type~PC1, data=train_pca27b)

# Make predictions
predictions5 = model5 %>% predict(test_pca5)
predictions6 = model6 %>% predict(test_pca6)
predictions10 = model10 %>% predict(test_pca10)
predictions11a = model11a %>% predict(test_pca11a)
#predictions11b = model11b %>% predict(test_pca11b)
predictions11c = model11c %>% predict(test_pca11c)
predictions12 = model12 %>% predict(test_pca12)
predictions13 = model13 %>% predict(test_pca13)
predictions16 = model16 %>% predict(test_pca16)
predictions18 = model18 %>% predict(test_pca18)
#predictions20 = model20 %>% predict(test_pca20)
predictions22 = model22 %>% predict(test_pca22)
#predictions25 = model25 %>% predict(test_pca25)
predictions27a = model27a %>% predict(test_pca27a)
predictions27b = model27b %>% predict(test_pca27b)


## USE ACCURACY GENERATED BY ACCURACY TEST SCRIPT


# Data to plot
data5 = as.data.frame(predictions5)
data5 = cbind(test_pca5$Individual, test_pca5$Population, test_pca5$Concrete_Location, test_pca5$Type, data5)
names(data5) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data6 = as.data.frame(predictions6)
data6 = cbind(test_pca6$Individual, test_pca6$Population, test_pca6$Concrete_Location, test_pca6$Type, data6)
names(data6) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data10 = as.data.frame(predictions10)
data10 = cbind(test_pca10$Individual, test_pca10$Population, test_pca10$Concrete_Location, test_pca10$Type, data10)
names(data10) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data11a = as.data.frame(predictions11a)
data11a = cbind(test_pca11a$Individual, test_pca11a$Population, test_pca11a$Concrete_Location, test_pca11a$Type, data11a)
names(data11a) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
#data11b = as.data.frame(predictions11b)
#data11b = cbind(test_pca11b$Individual, test_pca11b$Population, test_pca11b$Concrete_Location, test_pca11b$Type, data11b)
#names(data11b) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data11c = as.data.frame(predictions11c)
data11c = cbind(test_pca11c$Individual, test_pca11c$Population, test_pca11c$Concrete_Location, test_pca11c$Type, data11c)
names(data11c) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data12 = as.data.frame(predictions12)
data12 = cbind(test_pca12$Individual, test_pca12$Population, test_pca12$Concrete_Location, test_pca12$Type, data12)
names(data12) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data13 = as.data.frame(predictions13)
data13 = cbind(test_pca13$Individual, test_pca13$Population, test_pca13$Concrete_Location, test_pca13$Type, data13)
names(data13) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data16 = as.data.frame(predictions16)
data16 = cbind(test_pca16$Individual, test_pca16$Population, test_pca16$Concrete_Location, test_pca16$Type, data16)
names(data16) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data18 = as.data.frame(predictions18)
data18 = cbind(test_pca18$Individual, test_pca18$Population, test_pca18$Concrete_Location, test_pca18$Type, data18)
names(data18) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
#data20 = as.data.frame(predictions20)
#data20 = cbind(test_pca20$Individual, test_pca20$Population, test_pca20$Concrete_Location, test_pca20$Type, data20)
#names(data20) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data22 = as.data.frame(predictions22)
data22 = cbind(test_pca22$Individual, test_pca22$Population, test_pca22$Concrete_Location, test_pca22$Type, data22)
names(data22) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
#data25 = as.data.frame(predictions25)
#data25 = cbind(test_pca25$Individual, test_pca25$Population, test_pca25$Concrete_Location, test_pca25$Type, data25)
#names(data25) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data27a = as.data.frame(predictions27a)
data27a = cbind(test_pca27a$Individual, test_pca27a$Population, test_pca27a$Concrete_Location, test_pca27a$Type, data27a)
names(data27a) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")
data27b = as.data.frame(predictions27b)
data27b = cbind(test_pca27b$Individual, test_pca27b$Population, test_pca27b$Concrete_Location, test_pca27b$Type, data27b)
names(data27b) = c("Individual", "Population", "Concrete_Location", "True_Type", "Test_Type", "Posterior_Freshwater", "Posterior_Marine")

# Order the population factors
data5$Population = factor(data5$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data6$Population = factor(data6$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data10$Population = factor(data10$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data11a$Population = factor(data11a$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
#data11b$Population = factor(data11b$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data11c$Population = factor(data11c$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data12$Population = factor(data12$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data13$Population = factor(data13$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data16$Population = factor(data16$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data18$Population = factor(data18$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
#data20$Population = factor(data20$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data22$Population = factor(data22$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
#data25$Population = factor(data25$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data27a$Population = factor(data27a$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
data27b$Population = factor(data27b$Population, levels=c("RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 


# Plot points
plot5 = ggplot(data=data5, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.5, cex=3) + 
  ggtitle("Cluster 5 - Chr. 1 (14 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: N/A%) ") +
  scale_x_discrete(limits = rev(levels(data5$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot6 = ggplot(data=data6, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8, cex=3) + 
  ggtitle("Cluster 6 - Chr. 1 (746 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%) ") +
  scale_x_discrete(limits = rev(levels(data6$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot10 = ggplot(data=data10, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8, cex=3) + 
  ggtitle("Cluster 10 - Chr. 4 (23 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data10$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot11a = ggplot(data=data11a, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 11a - Chr. 4 (113 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data11a$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot11b = ggplot(data=data11b, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 11b - Chr. 4 (11 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data11b$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot11c = ggplot(data=data11c, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 11c - Chr. 4 (32 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data11c$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot12 = ggplot(data=data12, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 12 - Chr. 4 (36 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)")+
  scale_x_discrete(limits = rev(levels(data12$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot13 = ggplot(data=data13, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 13 - Chr. 4 (56 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data13$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot16 = ggplot(data=data16, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 16 - Chr. 8 (31) loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data16$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot18 = ggplot(data=data18, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 18 - Chr. 9 (30 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 75%)") +
  scale_x_discrete(limits = rev(levels(data18$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot20 = ggplot(data=data20, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 20  - Chr. 9 (13 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data20$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot22 = ggplot(data=data22, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 22  - Chr. 11 (464 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data22$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))


plot27a = ggplot(data=data27a, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8, cex=3) + 
  ggtitle("Cluster 27a  - Chr. 20 (73 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 85%)") +
  scale_x_discrete(limits = rev(levels(data27a$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

plot27b = ggplot(data=data27b, aes(x=Population, y=Posterior_Freshwater, color=Concrete_Location)) + 
  geom_point(aes(shape=True_Type), alpha=0.8,cex=3) + 
  ggtitle("Cluster 27b  - Chr. 20 (43 loci)") +
  xlab("Population") + 
  ylab("Probablity of carrying freshwater loci (1PC - accuracy: 100%)") +
  scale_x_discrete(limits = rev(levels(data27b$Population))) +
  scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                     labels=c("Adriatic_Sea"="Adriatic Sea", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
  scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
  coord_flip() +
  guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="True Habitat Type",override.aes=list(size=3, alpha=1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))

pdf("PC1_Ldna_QDA_projected.pdf", width=20, height=15)

ggarrange(plot5,
          plot6+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()), 
          plot10 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot11a + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot11c  ,
          plot12 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot13 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot16 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot18 ,
          plot22 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot27a + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          plot27b + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()),
          ncol=4, nrow=3, labels="auto", hjust=-1, align="hv", common.legend = TRUE, legend="bottom")

dev.off()