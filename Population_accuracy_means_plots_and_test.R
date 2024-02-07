
### Pop means of accuracy (proportions of predictions matchign true ecotype)

#all clusters except 18,25,27a
data_proj<-combine(data5,data6,data10,data11a,data11c,data12,data13,data16,data22,data27a)
data_proj<-subset(data_proj, data_proj$True_Type=="Freshwater")

adriatic<-subset(data_proj, data_proj$Concrete_Location=="Adriatic_Sea")
adriatic_ac<-adriatic %>%
  group_by(Population, Concrete_Location,source) %>%
  summarize(
    accuracy = mean(Test_Type==True_Type))

ib<-subset(data_proj, data_proj$Concrete_Location=="Iberian_Peninsula")
iberian_ac<-ib %>%
  group_by(Population, Concrete_Location,source) %>%
  summarize(
    accuracy = mean(Test_Type==True_Type))

north<-subset(data_proj, data_proj$Concrete_Location=="English_Channel"| data_proj$Concrete_Location=="Finnish_Golf"|data_proj$Concrete_Location=="North_Scandinavia"|data_proj$Concrete_Location=="North_Sea")
north_ac<-north %>%
  group_by(Population, Concrete_Location, source) %>%
  summarize(
    accuracy = mean(Test_Type==True_Type))

acc<-rbind(adriatic_ac,iberian_ac,north_ac)

acc$Concrete_Location<-gsub("English_Channel", "Northern populations",acc$Concrete_Location)
acc$Concrete_Location<-gsub("Finnish_Golf", "Northern populations",acc$Concrete_Location)
acc$Concrete_Location<-gsub("North_Scandinavia", "Northern populations",acc$Concrete_Location)
acc$Concrete_Location<-gsub("North_Sea", "Northern populations",acc$Concrete_Location)

acc_pop_proj<-acc %>%
  group_by(Population, Concrete_Location) %>%
  summarize(
    accuracy = mean(accuracy))

acc_perpop<-ggplot(acc_pop_proj,aes(x=Concrete_Location, y=accuracy, fill=Concrete_Location))+
  geom_boxplot()+
  geom_point()+
  theme_bw()+
  ylab("Accuracy (Proportion of predictions matchign true ecotype)")+
  xlab("")+
  theme(axis.text.x=element_blank(),legend.position = "bottom")+
  scale_fill_manual(name="Geographic Region", values=c("Adriatic_Sea"="orange", "Iberian_Peninsula"="red", "Northern populations"="royalblue3"),
                    labels=c("Adriatic_Sea"="Adriatic Sea", "Iberian_Peninsula"="Iberian Peninsula", "Northern populations"="Northern Populations")) 

write.table(acc_pop_proj,file="Pop_acc_proj.txt", row.names=T, quote=F)

## Combine data for projected and non-projected data - pop means of clusters means
# first get acc_pop from Parallellism Test LDna script (non-projected)

data_pop<-combine(acc_pop,acc_pop_proj)

plot_combined<-ggplot(data_pop,aes(x=Concrete_Location, y=accuracy, fill=source))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), alpha=0.3)+
  theme_bw()+
  ylab("Accuracy (Proportion of predictions matching true ecotype)")+
  xlab("")+
  ylim(0,1.3)+
  theme(legend.position = "bottom")+
  scale_fill_manual(name="Method", values=c("acc_pop"="coral3", "acc_pop_proj"="cadetblue4"),
                    labels=c("acc_pop"="Non-projected", "acc_pop_proj"="Projected")) 


plot_combined_simple<-ggplot(data_pop,aes(x=Concrete_Location, y=accuracy, fill=Concrete_Location))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), size=2)+
  theme_classic()+
  ylab("Proportion of predictions matching true ecotype")+
  xlab("")+
  ylim(0,1.2)+
  theme(legend.position = "none")+
  scale_fill_manual(name="Geographic Region", values=c("Adriatic_Sea"="coral3", "Iberian_Peninsula"="orange", "Northern populations"="cadetblue4"),
                    labels=c("Adriatic Sea", "Iberian Peninsula", "Northern Populations")) 

write.table(data_pop, file="Accuracy for boxplots projected and nonprojected.txt", quote=F, row.names = F)


##ANOVA and TUKEY tests

m1<-lm(data=data_pop, accuracy~Concrete_Location)
a1<-aov(m1)
summary(a1)

Df Sum Sq Mean Sq F value   Pr(>F)    
Concrete_Location  2 1.3198  0.6599   35.78 3.45e-09 ***
  Residuals         35 0.6454  0.0184         

TukeyHSD(x=a1)


diff         lwr        upr     p adj
Iberian_Peninsula-Adriatic_Sea          0.2257080  0.09754766  0.3538684 0.0003634
Northern populations-Adriatic_Sea      -0.2421429 -0.37030321 -0.1139825 0.0001442
Northern populations-Iberian_Peninsula -0.4678509 -0.60084916 -0.3348526 0.0000000

