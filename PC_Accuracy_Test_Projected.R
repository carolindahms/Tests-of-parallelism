# ACCURACY TEST FOR 1-4 PCs

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

# Import PCA projection dataframes
ff <-list.files(path = "./", pattern = ".txt")
ff<-ff[-1]
clusters <- lapply(ff, read.table)
names(clusters)=gsub(" _PCAproj.txt", "",ff)

#make dataframe to store test results
val<-c()
#list for results for each cluster

clusters<-clusters[-3] #remove cluster 11b (rank definciency)
clusters<-clusters[-8] #remove cluster 20 (rank definciency)
clusters<-clusters[-12] #remove cluster 5 (rank definciency)


results1<-list()
i=12

for(i in 1:length(clusters)){
  
  clusters[[i]]$Type <- as.factor(clusters[[i]]$Type)  
  # Create the training and test data
  ref <- clusters[[i]][which((clusters[[i]]$Concrete_Location=="Alaska" & clusters[[i]]$Type=="Freshwater") | (clusters[[i]]$Concrete_Location=="Alaska" | clusters[[i]]$Concrete_Location=="East_Russia") & clusters[[i]]$Type=="Marine"),]
  val<-c()
  
  for(j in 1:nrow(ref)){
    train_pca = ref[-j,]
    test_pca = anti_join(ref,train_pca)
    
    # Preprocessing parameters
    param = train_pca %>% preProcess(method=c("center","scale"))
    
    #### Model with PC1 ####
    model = qda(Type~PC1, data=train_pca)
    
    # Make predictions
    predictions = model %>% predict(test_pca)
    
    test = ifelse(predictions$class == test_pca$Type, 1,0)
    val=rbind(val,test)
  }
  results1[[i]] <- as.data.frame(val)
}

names(results1)<-names(clusters)

## 2 PCs
results2<-list()

for(i in 1:length(clusters)){
  
  clusters[[i]]$Type <- as.factor(clusters[[i]]$Type)  
  # Create the training and test data
  ref <- clusters[[i]][which((clusters[[i]]$Concrete_Location=="Alaska" & clusters[[i]]$Type=="Freshwater") | (clusters[[i]]$Concrete_Location=="Alaska" | clusters[[i]]$Concrete_Location=="East_Russia") & clusters[[i]]$Type=="Marine"),]
  val<-c()
  
  for(j in 1:nrow(ref)){
    train_pca = ref[-j,]
    test_pca = anti_join(ref,train_pca)
    
    # Preprocessing parameters
    param = train_pca %>% preProcess(method=c("center","scale"))
    
    #### Model ####
    model = qda(Type~PC1+PC2, data=train_pca)
    
    # Make predictions
    predictions = model %>% predict(test_pca)
    
    test = ifelse(predictions$class == test_pca$Type, 1,0)
    val=rbind(val,test)
  }
  results2[[i]] <- as.data.frame(val)
}

names(results2)<-names(clusters)

#### Cluster with 3 PCs


results3<-list()

for(i in 1:length(clusters)){
  
  clusters[[i]]$Type <- as.factor(clusters[[i]]$Type)  
  # Create the training and test data
  ref <- clusters[[i]][which((clusters[[i]]$Concrete_Location=="Alaska" & clusters[[i]]$Type=="Freshwater") | (clusters[[i]]$Concrete_Location=="Alaska" | clusters[[i]]$Concrete_Location=="East_Russia") & clusters[[i]]$Type=="Marine"),]
  val<-c()
  
  for(j in 1:nrow(ref)){
    train_pca = ref[-j,]
    test_pca = anti_join(ref,train_pca)
    
    # Preprocessing parameters
    param = train_pca %>% preProcess(method=c("center","scale"))
    
    #### Model####
    model = qda(Type~PC1+PC2+PC3, data=train_pca)
    
    # Make predictions
    predictions = model %>% predict(test_pca)
    
    test = ifelse(predictions$class == test_pca$Type, 1,0)
    val=rbind(val,test)
  }
  results3[[i]] <- as.data.frame(val)
}

names(results3)<-names(clusters)

#### Cluster with 4 PCs

results4<-list() #reank deficiency in marine

for(i in 1:length(clusters)){
  
  clusters[[i]]$Type <- as.factor(clusters[[i]]$Type)  
  # Create the training and test data
  ref <- clusters[[i]][which((clusters[[i]]$Concrete_Location=="Alaska" & clusters[[i]]$Type=="Freshwater") | (clusters[[i]]$Concrete_Location=="Alaska" | clusters[[i]]$Concrete_Location=="East_Russia") & clusters[[i]]$Type=="Marine"),]
  val<-c()
  
  for(j in 1:nrow(ref)){
    train_pca = ref[-j,]
    test_pca = anti_join(ref,train_pca)
    
    # Preprocessing parameters
    param = train_pca %>% preProcess(method=c("center","scale"))
    
    #### Model ####
    model = qda(Type~PC1+PC2+PC3+PC4, data=train_pca)
    
    # Make predictions
    predictions = model %>% predict(test_pca)
    
    test = ifelse(predictions$class == test_pca$Type, 1,0)
    val=rbind(val,test)
  }
  results4[[i]] <- as.data.frame(val)
}

names(results4)<-pc4


###################################

#combine
result<-c(results1,results2, results3)

#Test if assignment as expected
accuracies1<-c()
for(i in 1:length(results1)){
  accuracy<-sum(results1[[i]]$V1/length(results1[[i]]$V1))
  accuracies1<-rbind(accuracies1,accuracy)
}


accuracies2<-c()
for(i in 1:length(results2)){
  accuracy<-sum(results2[[i]]$V1/length(results2[[i]]$V1))
  accuracies2<-rbind(accuracies2,accuracy)
}

accuracies3<-c()
for(i in 1:length(results3)){
  accuracy<-sum(results3[[i]]$V1/length(results3[[i]]$V1))
  accuracies3<-rbind(accuracies3,accuracy)
}

all_acc<-cbind(accuracies1,accuracies2,accuracies3)

rownames(all_acc)=names(clusters)

write.table(all_acc, file="Accuracies LDna projected_PC1-PC3.txt", quote =F)

