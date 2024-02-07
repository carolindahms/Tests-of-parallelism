#### PREPARE THE LDna PROJECTED DATA ####

# Load the required libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(adegenet)
library(poppr)
library(zoo)

# Import individuals in the order of the bam files when calculating beagle file
individuals = read.table("~/Dropbox/Carla's paper/ldna/0_Individuals.txt")

# Import information on the individuals
individuals_information = read.table("~/Dropbox/Carla's paper/ldna/0_Individuals_Information.txt", header=T)
row_ind = as.matrix(read.table("~/Dropbox/Carla's paper/ldna/0_Individuals.txt"))

ff <-list.files(path = "./", pattern = "95.csv")
list<- lapply(ff, read.csv, header=T)
names(list)=gsub("_95.csv", "",ff)

#listdouble<-list()
#for(i in 1:length(list)){
#  listdouble[[i]] = cbind(list[[i]], list[[i]])
  
#}

pcadf<-list()

for(i in 1:length(list)) {
  cluster = list[[i]]
  # Change the -9 by NA
  cluster[cluster==-9] = NA
  
  # Change the row names
  rownames(cluster) = row_ind
  
  # Merge the dataframes to have all the information
  cluster$Individual = rownames(cluster)
  cluster = merge(individuals_information, cluster, by="Individual")
  rownames(cluster) = cluster$Individual
  
  #### A SIMPLE PCA ####
  
  # Training populations
  training_pops = c("RABBIT-SLOUGH", "RUS-AN-GA", "RUS-ASH-GA", "RUS-KHA-GA", "BEAR-PAW-LAKE", "BOOT-LAKE")
  
  # Training data
  training_cluster = cluster %>% filter (Population %in% training_pops)
  
  # Get a population vector
  training_pops = training_cluster$Population
  
  # Delete the non-loci information
  training_cluster[,1:5] = NULL
  
  # Transform to genind object
  training_genind = df2genind(training_cluster, sep=",", ploidy=2, pop=training_pops)
  
  # Remove loci if it is missing in more than 20% of the dataset 
  training_genind = missingno(training_genind, type="loci", cutoff=0.2, quiet=FALSE)
  
  # Loci that have stayed
  loci_kept = locNames(training_genind)
  
  # Transform data into dataframe again
  training_cluster = genind2df(training_genind)
  
  # Transform the loci columns into numeric
  training_cluster = training_cluster %>% 
    mutate_at(c(2:length(training_cluster)), as.numeric)
  
  # Take the rownames (individual names)
  training_ind = rownames(training_cluster)
  
  # Replace NA with mean values of the same population
  training_cluster = training_cluster %>% 
    group_by(pop) %>% 
    mutate_at(loci_kept, na.aggregate)
  
  # Transform to dataframe
  training_cluster = as.data.frame(training_cluster)
  
  # Change the rownames
  rownames(training_cluster) = training_ind
  
  # Remove the "pop" column
  training_cluster$pop = NULL
  
  # Remove rows with sd=0
  training_cluster_filt = Filter(function(x) sd(x) != 0, training_cluster)
  
  # Get rows that had sd 0
  training_sd0 = setdiff(colnames(training_cluster), colnames(training_cluster_filt))
  
  # Training PCA
  training_pca = prcomp(training_cluster_filt, center = TRUE, scale. = TRUE)
  
  # Getting the summary and other information
  summary(training_pca)
  str(training_pca)
  
  # Subset the values to plot
  data_training_pca = as.data.frame(training_pca$x)
  final_data_training_pca = data_training_pca
  final_data_training_pca$Individual = rownames(final_data_training_pca)
  
  # Add more information
  final_data_training_pca = merge(individuals_information, final_data_training_pca, by="Individual")
  
  # Test data
  test_pops = c("BOS-NER-GA", "ENG-BUT-GA", "FIN-KEV-GA", "GAC-NOR-KRI", "GAC-RUS-PRI", "GAC-SWE-FIS", 
                "GA-POR-LI", "GA-POR-MI", "GA-POR-RA", "GA-POR-SA", "GA-POR-TE", "GA-POR-VO", "ITA-STE",
                "KR", "M", "MON-SKA-GA", "MUR", "NB", "NN", "NOR-BAR-GA", "NOR-KVA-GA", "NOR-MYR-GA",
                "NOR-ORR-GA", "NOR-SBJ-GA", "NOR-SKF-GA", "RUS-LEV-GA", "RUS-SLI-GA")
  
  # Test data
  test_cluster = cluster %>% filter (Population %in% test_pops)
  
  # Get a population vector
  test_pops = test_cluster$Population
  
  # Remove the loci that were removed in the training set
  test_cluster = test_cluster[,which(names(test_cluster) %in% c("Individual", "Population", "Type", "Concrete_Location", "NS", loci_kept))]
  
  # Get the name of the columns
  loci_cols_test = colnames(test_cluster)
  loci_cols_test = loci_cols_test[loci_cols_test!= c("Individual", "Population", "Type", "Concrete_Location", "NS")]
  
  # Replace NA with mean values of the same population
  test_cluster = test_cluster %>% 
    group_by(Population) %>% 
    mutate_at(loci_cols_test, na.aggregate)
  
  # Replace NA with mean values of the same concrete location
  test_cluster = test_cluster %>% 
    group_by(Concrete_Location) %>% 
    mutate_at(loci_cols_test, na.aggregate)
  
  # Replace NA with mean values of the same type
  test_cluster = test_cluster %>% 
    group_by(Type) %>% 
    mutate_at(loci_cols_test, na.aggregate)
  
  # Transform to dataframe
  test_cluster = as.data.frame(test_cluster)
  
  # Name the rows according to the name of the sample
  rownames(test_cluster) = test_cluster$Individual
  
  # Remove the non-loci columns
  test_cluster[,1:5] = NULL
  
  # Remove same columns that have been removed in the training set (so the data frame and the $center of the training PCA have the same length)
  if(length(training_sd0!=0)) {
    test_cluster = test_cluster[,-which(names(test_cluster) %in% training_sd0)]
  }
  
  # Scale the variable values for test individuals in relation to the PCA's center
  test_scale = scale((test_cluster[,c(1:ncol(test_cluster))]), center=training_pca$center)
  
  # Apply the rotation of the PCA matrix to the test samples
  test_pred = test_scale %*% training_pca$rotation
  
  # PCA final data
  pca_proj = training_pca
  pca_proj$x = rbind(pca_proj$x, test_pred)
  final_plot_proj = as.data.frame(pca_proj$x)
  final_plot_proj$Individual = rownames(final_plot_proj)
  final_plot_proj = merge(individuals_information, final_plot_proj)
  final_plot_proj$Population = factor(final_plot_proj$Population, levels=c("RABBIT-SLOUGH", "RUS-ASH-GA", "RUS-AN-GA", "RUS-KHA-GA", "RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "BOOT-LAKE", "BEAR-PAW-LAKE", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR")) 
  final_plot_proj$Concrete_Location = factor(final_plot_proj$Concrete_Location, levels = c("Adriatic_Sea", "Alaska", "East_Russia", "English_Channel", "North_Scandinavia", "Finnish_Golf", "Iberian_Peninsula", "Mur_River","North_Sea"))
  
  # Calculate var explained by P1 and PC2, later select no. of PC which explain > 80%
  pc1 <- rep(round(summary(pca_proj)$importance[2,1]*100, digits=2), 257)
  pc2 <- rep(round(summary(pca_proj)$importance[2,2]*100, digits=2), 257)
  pcadf[[i]] = cbind(final_plot_proj, pc1, pc2)
  write.table(pcadf[[i]], file=paste(names(list[i]), "_PCAproj.txt"), sep="\t")
}

names(pcadf)=gsub("_95.csv", "",ff)

#### Combine Geom_Point by Population ####
# Reorder the factors

pcadf <- lapply(pcadf, function(x) {
  x$Population <- factor(x$Population, levels=c("RABBIT-SLOUGH", "RUS-ASH-GA", "RUS-AN-GA", "RUS-KHA-GA", "RUS-LEV-GA", "NOR-BAR-GA", "NOR-SBJ-GA", "GAC-RUS-PRI", "GAC-NOR-KRI", "GAC-SWE-FIS", "NOR-MYR-GA", "BOOT-LAKE", "BEAR-PAW-LAKE", "NOR-KVA-GA", "NOR-SKF-GA", "FIN-KEV-GA", "RUS-SLI-GA", "NOR-ORR-GA", "ENG-BUT-GA", "GA-POR-VO", "GA-POR-RA", "GA-POR-LI", "GA-POR-TE", "GA-POR-MI", "GA-POR-SA", "MUR", "NN", "ITA-STE", "BOS-NER-GA", "MON-SKA-GA", "M", "NB", "KR"))
  x
})

# Order the geographic location in alphabetical order
pcadf <- lapply(pcadf, function(x) {
  x$Concrete_Location <- factor(x$Concrete_Location, levels=c("Adriatic_Sea", "Alaska", "East_Russia", "English_Channel", "North_Scandinavia", "Finnish_Golf", "Iberian_Peninsula", "Mur_River","North_Sea"))
  x
})

#Rotate x axis
xrev<-c("cluster16","cluster6","cluster10", "cluster11a","cluster11b","cluster11c",
        "cluster12","cluster13","cluster20","cluster22","cluster27a", "cluster27b")
#xrev<-c("")

#Make shared y axis labels
ylab<-c("cluster5","cluster11c","cluster20")

#PC1

pc1proj <- list()

for(i in 1:length(pcadf)){
  first <- max(pcadf[[i]]$Eval)
  if(names(pcadf[i]) %in% ylab & !names(pcadf[i]) %in% xrev){
          plot<- ggplot(data=as.data.frame(pcadf[[i]]), aes(x=Population, y=PC1, color=Concrete_Location)) +
            geom_point(aes(shape=Type, alpha=Type), size=4) +
            ggtitle(names(pcadf[i])) +
            xlab("Population") +
            ylab(paste0("PC1", sep=" (", pcadf[[i]]$pc1, "%)")) +
            scale_x_discrete(limits = rev(levels(pcadf[[i]]$Population))) +
            #scale_y_reverse() +
            scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "Alaska"="darkolivegreen4", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                               labels=c("Adriatic_Sea"="Adriatic Sea", "Alaska"="Alaska", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
            scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
            scale_alpha_manual(values=c("Freshwater"=0.3, "Marine"=0.7)) +
            coord_flip() +
            guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="Habitat Type",override.aes=list(size=3, alpha=1))) +
            theme_bw() +
            theme(plot.title = element_text(hjust=0.5),
                  axis.text.y=element_text(color=c("skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "skyblue1", "navy", "navy", "navy", "navy", "navy", "navy", "navy", "navy", "navy", "navy", "navy")))
          
  }
          if(names(pcadf[i]) %in% ylab & names(pcadf[i]) %in% xrev){      
                  plot<- ggplot(data=as.data.frame(pcadf[[i]]), aes(x=Population, y=PC1, color=Concrete_Location)) +
                    geom_point(aes(shape=Type, alpha=Type), size=4) +
                    ggtitle(names(pcadf[i])) +
                    xlab("Population") +
                    ylab(paste0("PC1", sep=" (", pcadf[[i]]$pc1, "%)")) +
                    scale_x_discrete(limits = rev(levels(pcadf[[i]]$Population))) +
                    scale_y_reverse() +
                    scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "Alaska"="darkolivegreen4", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                                       labels=c("Adriatic_Sea"="Adriatic Sea", "Alaska"="Alaska", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
                    scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
                    scale_alpha_manual(values=c("Freshwater"=0.3, "Marine"=0.7)) +
                    coord_flip() +
                    guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="Habitat Type",override.aes=list(size=3, alpha=1))) +
                    theme_bw() +
                    theme(plot.title = element_text(hjust=0.5),
                          axis.text.y=element_text(color=c(rep("skyblue1",22), rep("navy",12))))
          }
  if(!names(pcadf[i]) %in% ylab & names(pcadf[i]) %in% xrev){
                  plot<- ggplot(data=as.data.frame(pcadf[[i]]), aes(x=Population, y=PC1, color=Concrete_Location)) +
                    geom_point(aes(shape=Type, alpha=Type), size=4) +
                    ggtitle(names(pcadf[i])) +
                    xlab("Population") +
                    ylab(paste0("PC1", sep=" (", pcadf[[i]]$pc1, "%)")) +
                    scale_x_discrete(limits = rev(levels(pcadf[[i]]$Population))) +
                    scale_y_reverse() +
                    scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "Alaska"="darkolivegreen4", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                                       labels=c("Adriatic_Sea"="Adriatic Sea", "Alaska"="Alaska", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
                    scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
                    scale_alpha_manual(values=c("Freshwater"=0.3, "Marine"=0.7)) +
                    coord_flip() +
                    guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="Habitat Type",override.aes=list(size=3, alpha=1))) +
                    theme_bw() +
                    theme(plot.title = element_text(hjust=0.5),
                          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank())
  }
  if(!names(pcadf[i]) %in% ylab & !names(pcadf[i]) %in% xrev){
    plot<- ggplot(data=as.data.frame(pcadf[[i]]), aes(x=Population, y=PC1, color=Concrete_Location)) +
      geom_point(aes(shape=Type, alpha=Type), size=4) +
      ggtitle(names(pcadf[i])) +
      xlab("Population") +
      ylab(paste0("PC1", sep=" (", pcadf[[i]]$pc1, "%)")) +
      scale_x_discrete(limits = rev(levels(pcadf[[i]]$Population))) +
      #scale_y_reverse() +
      scale_color_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "Alaska"="darkolivegreen4", "North_Scandinavia"="royalblue3", "East_Russia"="tomato4", "English_Channel"="chartreuse3", "Finnish_Golf"="purple3", "Iberian_Peninsula"="red", "North_Sea"="turquoise4", "Mur_River"="cyan2"),
                         labels=c("Adriatic_Sea"="Adriatic Sea", "Alaska"="Alaska", "North_Scandinavia"="Fennoscandia", "East_Russia"="East Russia", "English_Channel"="English Channel", "Finnish_Golf"="Gulf of Finland", "Iberian_Peninsula"="Iberian Peninsula", "North_Sea"="North Sea", "Mur_River"="Mur River")) +
      scale_shape_manual(values=c("Freshwater"=16, "Marine"=1)) +
      scale_alpha_manual(values=c("Freshwater"=0.3, "Marine"=0.7)) +
      coord_flip() +
      guides(colour=guide_legend(order=1, title="Geographic Region", override.aes=list(size=3, alpha=1)), shape=guide_legend(order=2, title="Habitat Type",override.aes=list(size=3, alpha=1))) +
      theme_bw() +
      theme(plot.title = element_text(hjust=0.5),
            axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank())
  }
  
  pc1proj[[i]]<-plot
  
}

names(pc1proj)=gsub("_95.csv", "",ff)

pc1proj<-pc1proj[c("cluster5", "cluster6", "cluster10", "cluster11a", "cluster11b", "cluster11c", "cluster12", "cluster13", "cluster16", "cluster18","cluster20","cluster22", "cluster27a","cluster27b")]

titles<-c("Cluster 5 (14 loci)", "Cluster 6 (746 loci)", "Cluster 10 (23 loci)", "Cluster 11a (113 loci)",
          "Cluster 11b (11 loci)","Cluster 11c (32 loci)", "Cluster 12 (36 loci)", "Cluster 13 (56 loci)", "Cluster 16 (31 loci)",
          "Cluster 18 (30 loci)", "Cluster 20 (13 loci)", "Cluster 22 (464 loci)", "Cluster 27a (73 loci)", "Cluster 27b (43 loci)")

for(i in 1:length(pc1proj)){
    pc1proj[[i]]$labels$title<-names(pc1proj[i])
  }



png("PC1_LDna_projected.png", units="in", width=35, height=20, res=900, units)
ggarrange(plotlist=pc1proj, ncol=5, nrow=3, labels="auto", hjust=-1, common.legend = TRUE, legend="bottom")
dev.off()

pdf("PC1_Ldna_projected_format.pdf", width=180, height=150)
ggarrange(plotlist=pc1proj, ncol=5, nrow=3, labels="auto", hjust=-1, common.legend = TRUE, legend="bottom")
dev.off()