# Tree-diversity-and-biomass-in-Acacia-mangium-plantations
#Analysis for paper Biomass and C
#Exploratory DBH-HEIGHT-DIVERSITY
Raw_data_DBH<-read.table("GoodPaired_analyses_DBH_BiomassNoCoconut.csv",h=T,sep=",")
#Remove the grassland
Raw_data_DBH<-Raw_data_DBH[Raw_data_DBH$Sample__ID != "G3SB", ]
Raw_data_DBH$Landcover<-as.factor(Raw_data_DBH$Landcover)
levels(Raw_data_DBH$Landcover)
#This is to ensure that the first factor displayed is 2 years old, not 10 years old plantation:
Raw_data_DBH$Landcover <- factor(Raw_data_DBH$Landcover, levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
levels(Raw_data_DBH$Landcover)
#Address colors
colori<-c("#ff46a2","orange","yellow","turquoise")
names(colori)<-levels(Raw_data_DBH$Landcover)
colori
colori[as.character(Raw_data_DBH$Landcover)]
unique(Raw_data_DBH$Landcover)
#To print one graph per page
layout(1,1)
#DBH~Landcover
plot(Raw_data_DBH$DBH..cm.~Raw_data_DBH$Landcover, xlab="Landcover",ylab="DBH (cm)",col=colori,pch=16)

#Height~Landcover
plot(Raw_data_DBH$Height..m.~Raw_data_DBH$Landcover, xlab="Landcover",ylab="Height (m)",col=colori,pch=16)

#Diversity~Landcover
Number_species<-c((nlevels(as.factor(Raw_data_DBH$Common_name_species[Raw_data_DBH$Landcover == "2 years old"]))), 
                  (nlevels(as.factor(Raw_data_DBH$Common_name_species[Raw_data_DBH$Landcover == "10 years old"]))), 
                  (nlevels(as.factor(Raw_data_DBH$Common_name_species[Raw_data_DBH$Landcover =="24 years old"]))), 
                  (nlevels(as.factor(Raw_data_DBH$Common_name_species[Raw_data_DBH$Landcover == "Remnant"])))) 
Number_species 
Landcover<-levels(as.factor(Raw_data_DBH$Landcover)) 
Landcover 
Landcover<-factor(Landcover, levels = c("2 years old", "10 years old", "24 years old", "Remnant")) 
Diversity.df<-data.frame(Number_species,Landcover) 
Diversity.df$Landcover<-as.factor(Diversity.df$Landcover)
#To order them from the 2 years old to the remnant
Diversity.df$Landcover<-factor(Diversity.df$Landcover, levels = c("2 years old", "10 years old", "24 years old", "Remnant")) 
levels(Diversity.df$Landcover) 
#Another vector of colors wince I changed the order
colori1<-c("orange","#ff46a2","yellow","turquoise") 
names(colori1)<-levels(Raw_data_DBH$Landcover) 
#To check
colori1 
#Plot
barplot(Diversity.df$Number_species~Diversity.df$Landcover, col=colori1, xlab="Landcover",ylab="Cumulative number of species", ylim= c(0,55)) 


#Construction of frequency classes NOT considering the <5 cm DBH
Landcover<-c(rep(c("2 years old","10 years old","24 years old","Remnant"),4))
Landcover
Classes<-c(rep("class 5-10cm",4), rep("class 10-20cm", 4),rep("class 20-30cm",4),rep("class ≥30cm",4))
Classes<-as.factor(Classes)
#make empty vectors
class_5_10 <-rep(0,4)
class_10_20 <-rep(0,4)
class_20_30 <-rep(0,4)
class_30 <-rep(0,4)
#2 years old plantation
class_5_10[1]  <-sum(Raw_data_DBH$DBH..cm.>5 & Raw_data_DBH$DBH..cm. <= 10 & Raw_data_DBH$Landcover=="2 years old")
class_10_20[1]  <-sum(Raw_data_DBH$DBH..cm.>10 & Raw_data_DBH$DBH..cm. <= 20 & Raw_data_DBH$Landcover=="2 years old")
class_20_30[1]  <-sum(Raw_data_DBH$DBH..cm.>20 & Raw_data_DBH$DBH..cm. <= 30 & Raw_data_DBH$Landcover=="2 years old")
class_30[1]  <-sum(Raw_data_DBH$DBH..cm.>=30 & Raw_data_DBH$Landcover=="2 years old")
#10 years old plantation
class_5_10[2]  <-sum(Raw_data_DBH$DBH..cm.>5 & Raw_data_DBH$DBH..cm. <= 10 & Raw_data_DBH$Landcover=="10 years old")
class_10_20[2]  <-sum(Raw_data_DBH$DBH..cm.>10 & Raw_data_DBH$DBH..cm. <= 20 & Raw_data_DBH$Landcover=="10 years old")
class_20_30[2]  <-sum(Raw_data_DBH$DBH..cm.>20 & Raw_data_DBH$DBH..cm. <= 30 & Raw_data_DBH$Landcover=="10 years old")
class_30[2]  <-sum(Raw_data_DBH$DBH..cm.>=30 & Raw_data_DBH$Landcover=="10 years old")
#24 years old plantation
class_5_10[3]  <-sum(Raw_data_DBH$DBH..cm.>5 & Raw_data_DBH$DBH..cm. <= 10 & Raw_data_DBH$Landcover=="24 years old")
class_10_20[3]  <-sum(Raw_data_DBH$DBH..cm.>10 & Raw_data_DBH$DBH..cm. <= 20 & Raw_data_DBH$Landcover=="24 years old")
class_20_30[3]  <-sum(Raw_data_DBH$DBH..cm.>20 & Raw_data_DBH$DBH..cm. <= 30 & Raw_data_DBH$Landcover=="24 years old")
class_30[3]  <-sum(Raw_data_DBH$DBH..cm.>=30 & Raw_data_DBH$Landcover=="24 years old")
#Remnant
class_5_10[4]  <-sum(Raw_data_DBH$DBH..cm.>5 & Raw_data_DBH$DBH..cm. <= 10 & Raw_data_DBH$Landcover=="Remnant")
class_10_20[4]  <-sum(Raw_data_DBH$DBH..cm.>10 & Raw_data_DBH$DBH..cm. <= 20 & Raw_data_DBH$Landcover=="Remnant")
class_20_30[4]  <-sum(Raw_data_DBH$DBH..cm.>20 & Raw_data_DBH$DBH..cm. <= 30 & Raw_data_DBH$Landcover=="Remnant")
class_30[4]  <-sum(Raw_data_DBH$DBH..cm.>=30 & Raw_data_DBH$Landcover=="Remnant")
#To see the results
summary(class_5_10)
class_5_10
class_10_20
class_20_30
class_30
ClassValues<-c(class_5_10, class_10_20,class_20_30,class_30)
ClassValues
#Put in a matrix
Frequency_class.df<-data.frame(Landcover,ClassValues,Classes)
#See the table
Frequency_class.df
#make the landcover a factor
Landcover<-as.factor(Landcover)
Classes<-as.factor(Classes)
summary(Frequency_class.df)
Frequency_class.df$Landcover<-factor(Frequency_class.df$Landcover, levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Frequency_class.df$Classes<-factor(Frequency_class.df$Classes, levels=c("class ≥30cm","class 20-30cm","class 10-20cm","class 5-10cm","class ≤5cm"))
# library
library(ggplot2)
# Grouped
ggplot(Frequency_class.df, aes(fill=Classes, y=ClassValues, x=Landcover)) + 
  geom_bar(position="stack", stat="identity")+ labs(title="DBH frequency classes", x= 'Landcover',y='Number of trees')

#Proportion of DBH classes
library(dplyr)
# Define the DBH class intervals
class_breaks <- c(5,10, 20, 30, Inf)

# Create a new column 'DBH_class' using `cut` function
Raw_data_DBH <- Raw_data_DBH %>%
  mutate(DBH_class = cut(DBH..cm., breaks = class_breaks,
                         labels = c("5-10cm", "10-20cm", "20-30cm", ">=30cm"),
                         right = FALSE))

# Filter out trees with DBH < 5 cm
filtered_data <- Raw_data_DBH %>%
  filter(DBH..cm. >= 5)
#To check
filtered_data
# Group by Landcover and DBH class, then calculate proportions
proportions_df <- filtered_data %>%
  group_by(Landcover, DBH_class) %>%
  summarise(Count = n()) %>%
  group_by(Landcover) %>%
  mutate(Proportion = Count / sum(Count) * 100)
# Visualize the results
ggplot(proportions_df, aes(x = Landcover, y = Proportion, fill = DBH_class)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "cividis") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "DBH Frequency Classes", x = "Landcover", y = "Proportion of Trees")
proportions_df

#Analysis of the number of native and exotic species per landcover
#Analysis of number of native species per landcover. Here the NA are ignored
#This code calculates the number of native species (counting the different species) found in each Landcover type
#Without the coconut
Raw_data_DBH<-read.table("GoodPaired_analyses_DBH_BiomassNoCoconut.csv",h=T,sep=",")
#Remove Grassland
Raw_data_DBH<-Raw_data_DBH[Raw_data_DBH$Sample__ID != "G3SB", ]
Raw_data_DBH$Landcover<-as.factor(Raw_data_DBH$Landcover)
levels(Raw_data_DBH$Landcover)
Raw_data_DBH$Native<-as.factor(Raw_data_DBH$Native)
levels(Raw_data_DBH$Native)
#This is to ensure that the first factor displayed is 2 years old, not 10 years old plantation:
Raw_data_DBH$Landcover <- factor(Raw_data_DBH$Landcover, levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
levels(Raw_data_DBH$Landcover)
#Construction of matrix with number of native species per landcover
NLandcover<-c("2 years old","10 years old", "24 years old","Remnant")
ELandcover<-c("2 years old","10 years old", "24 years old","Remnant")
Raw_data_DBH
#First 4 values for Native, second 4 for Exotic
Landcover<-c(NLandcover,ELandcover)
Landcover
Native_Exotic<-c((rep("N",4)),(rep("E",4)))
Native_Exotic
#Empty vectors for the Native and Exotic trees
Native<-rep(0,4)
Exotic<-rep(0,4)
#Put into the vectors the number of Native and Exotic species
Native[1]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "2 years old"]))
Native[2]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "10 years old"]))
Native[3]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "24 years old"]))
Native[4]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "Remnant"]))
Native
Exotic[1]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "2 years old"]))
Exotic[2]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "10 years old"]))
Exotic[3]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "24 years old"]))
Exotic[4]<-length(unique(Raw_data_DBH$Common_name_species[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "Remnant"]))
Exotic
Origin<-c(Native,Exotic)
Origin
#Dataset construction
NativeExotic.df<-data.frame(Landcover,Origin, Native_Exotic)
summary(NativeExotic.df)
NativeExotic.df
NativeExotic.df$Landcover<-factor(NativeExotic.df$Landcover, levels= c("2 years old","10 years old", "24 years old","Remnant"))
NativeExotic.df$Native_Exotic<-factor(NativeExotic.df$Native_Exotic, levels= c("N","E"))
#Plot
library(ggplot2)
ggplot(NativeExotic.df, aes(fill=Native_Exotic, y=Origin, x=Landcover)) +  scale_fill_viridis_d(option = "cividis")+ geom_bar(position="stack", stat="identity")+ labs(title="Number of native and exotic species", x= 'Landcover',y='Number of species')


#DBH-Height-Biomass-Carbon
Paired_data_DBH<-read.table("GoodPaired_analyses_DBH_BiomassNoCoconut.csv",h=T,sep=",")
Paired_data_DBH$Landcover<- factor(Paired_data_DBH$Landcover , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
summary(Paired_data_DBH)
#DBH
plot(Paired_data_DBH$DBH..cm.)
qqnorm(Paired_data_DBH$DBH..cm.)
ks.test(Paired_data_DBH$DBH..cm., "pnorm")
shapiro.test(Paired_data_DBH$DBH..cm.)
#Height
plot(Paired_data_DBH$Height..m.)
qqnorm(Paired_data_DBH$Height..m.)
ks.test(Paired_data_DBH$Height..m., "pnorm")
shapiro.test(Paired_data_DBH$Height..m.)
#Biomass Eq.4
plot(Paired_data_DBH$AGB.Chave.2014.Eq..4)
qqnorm(Paired_data_DBH$AGB.Chave.2014.Eq..4)
ks.test(Paired_data_DBH$AGB.Chave.2014.Eq..4, "pnorm")
shapiro.test(Paired_data_DBH$AGB.Chave.2014.Eq..4)
#Biomass Eq.2005
plot(Paired_data_DBH$AGB.Chave.Eq..2005)
qqnorm(Paired_data_DBH$AGB.Chave.Eq..2005)
ks.test(Paired_data_DBH$AGB.Chave.Eq..2005, "pnorm")
shapiro.test(log(Paired_data_DBH$AGB.Chave.Eq..2005))
#Basal Area
#BA
plot(Paired_data_DBH$Basal.area.cm2)
qqnorm(Paired_data_DBH$Basal.area.cm2)
ks.test(Paired_data_DBH$Basal.area.cm2, "pnorm")
shapiro.test(Paired_data_DBH$Basal.area.cm2)
#Biomass total
plot(Paired_data_DBH$AGB_Total)
qqnorm(Paired_data_DBH$AGB_Total)
ks.test(Paired_data_DBH$AGB_Total, "pnorm") 
shapiro.test(Paired_data_DBH$AGB_Total)
#Carbon
plot(Paired_data_DBH$Carbon_Stock..kg.)
qqnorm(Paired_data_DBH$Carbon_Stock..kg.)
ks.test(Paired_data_DBH$Carbon_Stock..kg., "pnorm") 
shapiro.test(Paired_data_DBH$Carbon_Stock..kg.)
#Biomass per plot
##Biomass Per Plot in dataset "BiomassPerPlot"
#Biomass
BiomassPerPlot<-read.table("BiomassPerPlot.csv",sep=",",h=T)
BiomassPerPlot$Landcover<-as.factor(BiomassPerPlot$Landcover)
BiomassPerPlot$Landcover<-factor(BiomassPerPlot$Landcover, levels=c("2 years old", "10 years old", "24 years old", "Remnant"))
BiomassPerPlot
boxplot(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover, pch=21,bg=5)
#Data normality
plot(BiomassPerPlot$Biomass_Plot_Per_ha)
qqnorm(BiomassPerPlot$Biomass_Plot_Per_ha)
#no ks.test because less than 80 samples
shapiro.test(BiomassPerPlot$Biomass_Plot_Per_ha)
#They are normal so I use anova to compare between landcover types
#Elevation
#Elevation and Landcover to see that the landcover type is not distributed evenly
boxplot(Paired_analyses$Elevation ~Paired_analyses$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_analyses$Landcover,Paired_analyses$Elevation,correct=T)
#Elevation and DBH
plot(Paired_data_DBH$Elevation~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$Elevation,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$DBH..cm., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$DBH..cm., method="spearman",exact=T)
#Elevation and Height
plot(Paired_data_DBH$Elevation~Paired_data_DBH$Height..m.,pch=21,bg=4)
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$Elevation,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$Height..m., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$Height..m., method="spearman",exact=T)
#Elevation and Biomass Eq.4
plot(Paired_data_DBH$Elevation~Paired_data_DBH$AGB.Chave.2014.Eq..4,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.2014.Eq..4~Paired_data_DBH$Elevation,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="spearman",exact=T)
#Elevation and Biomass Eq.2005
plot(Paired_data_DBH$Elevation~Paired_data_DBH$AGB.Chave.Eq..2005,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$Elevation,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$AGB.Chave.Eq..2005, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$AGB.Chave.Eq..2005, method="spearman",exact=T)
#Elevation and Biomass total
plot(Paired_data_DBH$Elevation~Paired_data_DBH$AGB_Total,pch=21,bg=4)
plot(Paired_data_DBH$AGB_Total~Paired_data_DBH$Elevation,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$AGB_Total, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$AGB_Total, method="spearman",exact=T)
#Elevation and Carbon
plot(Paired_data_DBH$Elevation~Paired_data_DBH$Carbon_Stock..kg.,pch=21,bg=4)
plot(Paired_data_DBH$Carbon_Stock..kg.~Paired_data_DBH$Elevation,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$Carbon_Stock..kg., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Elevation,Paired_data_DBH$Carbon_Stock..kg., method="spearman",exact=T)
#Basal Area and Elevation
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$Elevation,pch=21,bg=4)
plot(Paired_data_DBH$Elevation~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Elevation, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Elevation, method="spearman",exact=T)
#Post hoc elevation
pairwise.wilcox.test(Paired_analyses$Elevation ,Paired_analyses$Landcover, method="holm")
#DBH
#DBH and Landcover
boxplot(Paired_data_DBH$DBH..cm. ~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$DBH..cm. ,correct=T)
#DBH and Hill side
boxplot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$Hill__side,pch=21,bg=4)
#Chi test (not ordination)
chisq.test(Paired_data_DBH$Hill__side,Paired_data_DBH$DBH..cm., correct=T)
#DBH and Height
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$Height..m.,pch=21,bg=4)
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$Height..m., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$Height..m., method="spearman",exact=T)
#DBH and Biomass Eq.4
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$AGB.Chave.2014.Eq..4,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.2014.Eq..4~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="spearman",exact=T)
#DBH and Biomass Eq.2005
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$AGB.Chave.Eq..2005,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$AGB.Chave.Eq..2005, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$AGB.Chave.Eq..2005, method="spearman",exact=T)
#DBH and Biomass total
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$AGB_Total,pch=21,bg=4)
plot(Paired_data_DBH$AGB_Total~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$AGB_Total, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$AGB_Total, method="spearman",exact=T)
#DBH and Carbon
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$Carbon_Stock..kg.,pch=21,bg=4)
plot(Paired_data_DBH$Carbon_Stock..kg.~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$Carbon_Stock..kg., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$DBH..cm.,Paired_data_DBH$Carbon_Stock..kg., method="spearman",exact=T)
#Basal Area and DBH
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$DBH..cm.,pch=21,bg=4)
plot(Paired_data_DBH$DBH..cm.~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$DBH..cm., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$DBH..cm., method="spearman",exact=T)
#Height
#Height and Landcover
boxplot(Paired_data_DBH$Height..m. ~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$Height..m.,correct=T)
#Height and Hill side
boxplot(Paired_data_DBH$Height..m.~Paired_data_DBH$Hill__side,pch=21,bg=4)
#Chi test (not ordination)
chisq.test(Paired_data_DBH$Hill__side,Paired_data_DBH$Height..m., correct=T)
#Height and Biomass Eq.4
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$AGB.Chave.2014.Eq..4,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.2014.Eq..4~Paired_data_DBH$Height..m.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="spearman",exact=T)
#Height and Biomass Eq.2005
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$AGB.Chave.Eq..2005,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$Height..m.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$AGB.Chave.Eq..2005, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$AGB.Chave.Eq..2005, method="spearman",exact=T)
#Height and Biomass total
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$AGB_Total,pch=21,bg=4)
plot(Paired_data_DBH$AGB_Total~Paired_data_DBH$Height..m.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$AGB_Total, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$AGB_Total, method="spearman",exact=T)
#Height and Carbon Stock
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$Carbon_Stock..kg.,pch=21,bg=4)
plot(Paired_data_DBH$Carbon_Stock..kg.~Paired_data_DBH$Height..m.,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$Carbon_Stock..kg., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Height..m.,Paired_data_DBH$Carbon_Stock..kg., method="spearman",exact=T)
#Basal Area and Height
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$Height..m.,pch=21,bg=4)
plot(Paired_data_DBH$Height..m.~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Height..m., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Height..m., method="spearman",exact=T)
#Biomass Eq.4
#Biomass Eq.4 and Landcover
boxplot(Paired_data_DBH$AGB.Chave.2014.Eq..4 ~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$AGB.Chave.2014.Eq..4,correct=T)
#Biomass Eq.4 and Biomass Eq.2005
plot(Paired_data_DBH$AGB.Chave.2014.Eq..4.~Paired_data_DBH$AGB.Chave.Eq..2005,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$AGB.Chave.2014.Eq..4,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$AGB.Chave.2014.Eq..4,Paired_data_DBH$AGB.Chave.Eq..2005, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$AGB.Chave.2014.Eq..4,Paired_data_DBH$AGB.Chave.Eq..2005, method="spearman",exact=T)
#Basal Area and Biomass Eq.4
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$AGB.Chave.2014.Eq..4,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.2014.Eq..4~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$AGB.Chave.2014.Eq..4, method="spearman",exact=T)
#Biomass Eq.2005
#Biomass Eq.2005 and Landcover
boxplot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$AGB.Chave.Eq..2005,correct=T)
#Biomass Eq.2005 and Hill side
boxplot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$Hill__side,pch=21,bg=4)
#Chi test (not ordination)
chisq.test(Paired_data_DBH$Hill__side,Paired_data_DBH$AGB.Chave.Eq..2005, correct=T)
#Basal Area and Biomass Eq.7
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$AGB.Chave.Eq..2005,pch=21,bg=4)
plot(Paired_data_DBH$AGB.Chave.Eq..2005~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$AGB.Chave.Eq..2005, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$AGB.Chave.Eq..2005, method="spearman",exact=T)
#Biomass total
#Biomass total and Landcover
boxplot(Paired_data_DBH$AGB_Total ~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$AGB_Total,correct=T)
#Biomass total and Hill side
boxplot(Paired_data_DBH$AGB_Total~Paired_data_DBH$Hill__side,pch=21,bg=4)
#Chi test (not ordination)
chisq.test(Paired_data_DBH$Hill__side,Paired_data_DBH$AGB_Total, correct=T)
#Basal Area and Biomass total
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$AGB_Total,pch=21,bg=4)
plot(Paired_data_DBH$AGB_Total~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$AGB_Total, method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$AGB_Total, method="spearman",exact=T)
#Carbon Stock
#Carbon stock and Landcover
boxplot(Paired_data_DBH$Carbon_Stock..kg.~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$Carbon_Stock..kg.,correct=T)
#Carbon stock and Hill side
boxplot(Paired_data_DBH$Carbon_Stock..kg.~Paired_data_DBH$Hill__side,pch=21,bg=4)
#Chi test (not ordination)
chisq.test(Paired_data_DBH$Hill__side,Paired_data_DBH$Carbon_Stock..kg., correct=T)
#Basal.area.cm2 and Carbon
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$Carbon_Stock..kg.,pch=21,bg=4)
plot(Paired_data_DBH$Carbon_Stock..kg.~Paired_data_DBH$Basal.area.cm2,pch=21,bg=4)
#Kendall correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Carbon_Stock..kg., method="kendall",exact=T)
#Spearman correlation
cor.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Carbon_Stock..kg., method="spearman",exact=T)
#Basal area
#Basal Area and Landcover
plot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$Landcover,pch=21,bg=4)
#Chi test
chisq.test(Paired_data_DBH$Landcover,Paired_data_DBH$Basal.area.cm2,correct=T)
#Basal Area and Hill side
boxplot(Paired_data_DBH$Basal.area.cm2~Paired_data_DBH$Hill__side,pch=21,bg=4)
#Chi test (not ordination)
chisq.test(Paired_data_DBH$Hill__side,Paired_data_DBH$Basal.area.cm2,correct=T)
#Biomass Per Plot
#The data are normal so I use ANOVA to compare between landcover types
#BiomassPerPlot and Landcover
summary(aov(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover))
plot(lm(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover))
colori<-c("orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(BiomassPerPlot$Landcover)
colori #to check
colori[as.character(BiomassPerPlot$Landcover)]
unique(BiomassPerPlot$Landcover)
plot(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
plot(BiomassPerPlot$Biomass.tonnes.ha.~BiomassPerPlot$Landcover,col=colori,xlab="Landcover",ylab="Above ground biomass (t/ha)")
#BiomassPerPlot and HillSide
summary(aov(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Hill__side))
#To compare just between Remnant and 2 years old:
summary(aov(BiomassPerPlot$Biomass_Plot_Per_ha[BiomassPerPlot$Landcover %in% c("2 years old", "Remnant")] ~ BiomassPerPlot$Landcover[BiomassPerPlot$Landcover %in% c("2 years old", "Remnant")]))
plot(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
plot(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
#DBH
#Change the dataset
pairwise.wilcox.test(Paired_data_DBH$DBH..cm. ,Paired_data_DBH$Landcover, method="holm")
#Biomass Eq.2005
pairwise.wilcox.test(Paired_data_DBH$AGB.Chave.Eq..2005,Paired_data_DBH$Landcover, method="holm")
#To explore the means
aggregate(Paired_data_DBH$AGB.Chave.2014.Eq..4 ~ Paired_data_DBH$Landcover, FUN = mean)
#Basal Area and Landcover
aggregate(Paired_data_DBH$Basal.area.cm2 ~ Paired_data_DBH$Landcover, FUN = mean) #Just to see the means
pairwise.wilcox.test(Paired_data_DBH$Basal.area.cm2,Paired_data_DBH$Landcover,method="holm")
BiomassPerPlot
#Post-hoc
#Comparisons, contrasts with the first level, which is the 2 years old plantation
BiomassPerPlot$Landcover<-relevel(BiomassPerPlot$Landcover,"2 years old")
BiomassPerPlotAnova<-lm(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
summary(BiomassPerPlotAnova)
#Changing level of comparison to the 10 years old
BiomassPerPlot$Landcover<-relevel(BiomassPerPlot$Landcover,"10 years old")
BiomassPerPlotAnova<-lm(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
summary(BiomassPerPlotAnova)
#Changing level of comparison to the 24 years old
BiomassPerPlot$Landcover<-relevel(BiomassPerPlot$Landcover,"24 years old")
BiomassPerPlotAnova<-lm(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
summary(BiomassPerPlotAnova)
#Changing level of comparison to the remnant forest
BiomassPerPlot$Landcover<-relevel(BiomassPerPlot$Landcover,"Remnant")
BiomassPerPlotAnova<-lm(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover)
summary(BiomassPerPlotAnova)
#Other tests for constrasts and Post-hoc
TukeyHSD(aov(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover, method="schaffe"))
TukeyHSD(aov(BiomassPerPlot$Biomass_Plot_Per_ha~BiomassPerPlot$Landcover))
pairwise.t.test(BiomassPerPlot$Biomass_Plot_Per_ha,BiomassPerPlot$Landcover, p.adjust.method = "holm")
pairwise.t.test(BiomassPerPlot$Biomass_Plot_Per_ha,BiomassPerPlot$Landcover, p.adjust.method = "bonferroni")
library(dunn.test)
dunn.test(BiomassPerPlot$Biomass_Plot_Per_ha,BiomassPerPlot$Landcover, method="bonferroni")
#Diversity: Hill’s numbers
#Hills numbers for tree diversity
#Resources: https://cran.r-project.org/web/packages/iNEXT/index.html
#https://besjournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F2041-210X.12613&file=mee312613-sup-0001-AppendixS1.pdf
#https://sites.google.com/view/chao-lab-website/software/inext
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12613
#https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-0133.1
## install iNEXT package from CRAN
install.packages("iNEXT")
## install iNEXT from github
#install.packages('devtools')
library(devtools)
#install.packages("httr")
library(httr)
#install_github('AnneChao/iNEXT')
## import packages
library(iNEXT)
library(ggplot2)
#See https://www.youtube.com/watch?app=desktop&v=bUFdei_zl88
#Diversity studied is the richness, the Shannon entropy and the inverse of Simpson. For Shannon, the higher the values,
#the higher is the entropy. For the inverse of Simpson, the higher is the value, the higher is the evenness.
#For interpretation see also: https://stats.libretexts.org/Bookshelves/Applied_Statistics/Natural_Resources_Biometrics_(Kiernan)/10%3A_Quantitative_Measures_of_Diversity_Site_Similarity_and_Habitat_Suitability/10.01%3A_Introduction__Simpsons_Index_and_Shannon-Weiner_Index
#Diversity for each plot
#Steps to convert the dataset into a matrix
TreeSpecies<-read.table("TreeSpecies.csv",sep=',',h=T)
#Control the dataset, which contains the Musa spp. (it only lacks of 2 trees which were unknown)
TreeSpecies
TreeSpecies<-as.data.frame(TreeSpecies)
numeric_data<-TreeSpecies[,sapply(TreeSpecies, is.numeric)]
str(numeric_data)
numeric_matrix<-as.matrix(numeric_data)
numeric_matrix_no_na<-na.omit(numeric_matrix)
D_abund<-iNEXT(numeric_matrix_no_na, datatype="abundance", q=0, se=T)
D_abund
plot(D_abund, ylim=c(0,40), xlim=c(0,150))
#For asymptotic understanding: https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-0133.1 and
#https://ericmarcon.github.io/entropart/articles/extrapolation.html#:~:text=Thus%2C%20the%20asymptotic%20estimators%20of,a%20function%20of%20sample%20size.
#Diversity for each landcover type, summing the data of each plot
TreeSpeciesLandcover<-read.table("TreeSpeciesLandcover.csv",sep=',',h=T)
TreeSpeciesLandcover
TreeSpeciesLandcover<-as.data.frame(TreeSpeciesLandcover)
numeric_dataLandcover<-TreeSpeciesLandcover[,sapply(TreeSpeciesLandcover, is.numeric)]
str(numeric_dataLandcover)
numeric_matrixLandcover<-as.matrix(numeric_dataLandcover)
numeric_matrix_no_naLandcover<-na.omit(numeric_matrixLandcover)
#I write the names of the colums, otherwise R will inser an "X" in fron of them, because it does not accept the name to start with a number, if not specified as follows:
colnames(numeric_matrix_no_naLandcover)<-c("2 years old", "10 years old", "24 years old", "Remnant")
numeric_matrix_no_naLandcover
D_abundLandcover<-iNEXT(numeric_matrix_no_naLandcover, datatype="abundance", q=c(0,1,2), se=T)
D_abundLandcover
plot(D_abundLandcover)
#Diversity for Exotic and Native species for each landcover type
Exotic
##Exotic diversity for each landcover type, merging the data of each plot
TreeSpeciesExoticLandcover<-read.table("TreeSpeciesExoticLandcover.csv",sep=',',h=T)
TreeSpeciesExoticLandcover
TreeSpeciesExoticLandcover<-as.data.frame(TreeSpeciesExoticLandcover)
numeric_dataExoticLandcover<-TreeSpeciesExoticLandcover[,sapply(TreeSpeciesExoticLandcover, is.numeric)]
str(numeric_dataExoticLandcover)
numeric_matrixExoticLandcover<-as.matrix(numeric_dataExoticLandcover)
numeric_matrix_no_naExoticLandcover<-na.omit(numeric_matrixExoticLandcover)
#I write the names of the colums, otherwise R will inser an "X" in fron of them, because it does not accept the name to start with a number, if not specified as follows:
colnames(numeric_matrix_no_naExoticLandcover)<-c("2 years old", "10 years old", "24 years old", "Remnant")
numeric_matrix_no_naExoticLandcover
D_abundExoticLandcover<-iNEXT(numeric_matrix_no_naExoticLandcover, datatype="abundance", q=c(0,1,2), se=T)
D_abundExoticLandcover
plot(D_abundExoticLandcover)
Native
##Native diversity for each landcover type, merging the data of each plot
TreeSpeciesNativeLandcover<-read.table("TreeSpeciesNativeLandcover.csv",sep=',',h=T)
TreeSpeciesNativeLandcover
TreeSpeciesNativeLandcover<-as.data.frame(TreeSpeciesNativeLandcover)
numeric_dataNativeLandcover<-TreeSpeciesExoticLandcover[,sapply(TreeSpeciesNativeLandcover, is.numeric)]
str(numeric_dataNativeLandcover)
numeric_matrixNativeLandcover<-as.matrix(numeric_dataNativeLandcover)
numeric_matrix_no_naNativeLandcover<-na.omit(numeric_matrixNativeLandcover)
#I write the names of the colums, otherwise R will inser an "X" in fron of them, because it does not accept the name to start with a number, if not specified as follows:
colnames(numeric_matrix_no_naNativeLandcover)<-c("2 years old", "10 years old", "24 years old", "Remnant")
numeric_matrix_no_naNativeLandcover
D_abundNativeLandcover<-iNEXT(numeric_matrix_no_naNativeLandcover, datatype="abundance", q=c(0,1,2), se=T)
D_abundNativeLandcover
plot(D_abundNativeLandcover)
#Pielou evenness
#Pielou evenness considering the landcover, thus the various samples
#See https://www.rpubs.com/roalle/mres_2019
library(vegan)
library(ggplot2)
#install.packages("viridis")
library(viridis)
PielouTreeSpecies<-read.table("TreeSpeciesLandcover2.csv",sep=',',h=T)
PielouTreeSpecies<-as.data.frame(PielouTreeSpecies)
Pielounumeric_data<-PielouTreeSpecies[,sapply(PielouTreeSpecies, is.numeric)]
str(Pielounumeric_data)
Pielounumeric_matrix<-as.matrix(Pielounumeric_data)
Pielounumeric_matrix_no_na<-na.omit(Pielounumeric_matrix)
# Shannon's H'
H <- diversity(Pielounumeric_matrix)
H
# Observed Richness
richness <- specnumber(Pielounumeric_matrix)  
richness
# Pielou's Evenness
evenness <- H/log(richness)
evenness

alpha <- data.frame(PielouTreeSpecies$Species, pielou = evenness)
colnames(alpha)<-c("Landcover","Pielou_evenness")
alpha
alpha$Landcover<-factor(alpha$Landcover,levels=c("2 years old","10 years old","24 years old","Remnant"))

#Plot
ggplot(alpha, aes(x = Landcover, y = Pielou_evenness, fill = Landcover)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "turbo",discrete = T) +
  ylab("Pielou's Evenness") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
#Chao1 and Chao2 indexes
#Chao1 index
#See https://immunarch.com/articles/web_only/v6_diversity.html
library(vegan)
install.packages("fossil")
library(fossil)
#Chao 1 index
Chao1community_matrix <- read.csv("TreeSpecies.csv", h=T, sep=",")
Chao1community_matrix
#Chao1 for 2 years old
chao1(Chao1community_matrix[2:57,2], taxa.row = T)
#Chao1 for 10 years old
chao1(Chao1community_matrix[2:57,6:9], taxa.row = T)
#Chao1 for 24 years old
chao1(Chao1community_matrix[2:57,10:13], taxa.row = T)
#Chao1 for Remnant
chao1(Chao1community_matrix[2:57,14:17], taxa.row = T)

#Display results of Chao1 index
Landcover<-c("2 Years old","10 Years old", "24 Years old", "Remnant")
Chao1index<-as.numeric(c("3.5","25.25","30","65.64286"))
DataframeChao1<-data.frame(Landcover,Chao1index)
#Visualize the dataframe
DataframeChao1
#order the factors
DataframeChao1$Landcover<-factor(DataframeChao1$Landcover, levels=c("2 Years old", "10 Years old", "24 Years old", "Remnant"))
#ggplot
library(ggplot2)

ggplot(DataframeChao1, aes(x = Landcover, y = Chao1index)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Landcover Type", y = "Chao1 Index", title = "Chao1 Index by Landcover") +
  theme_minimal()

#Chao 2 index
Chao2community_matrix <- read.csv("TreeSpecies.csv", h=T, sep=",")
Chao2community_matrix

#Chao2 for 2 years old
chao2(Chao2community_matrix[2:57,2:5], taxa.row = T)
#Chao2 for 10 years old
chao2(Chao2community_matrix[2:57,6:9], taxa.row = T)
#Chao2 for 24 years old
chao2(Chao2community_matrix[2:57,10:13], taxa.row = T)
#Chao2 for Remnant
chao2(Chao2community_matrix[2:57,14:17], taxa.row = T)

#Display results of Chao2 index
Landcover<-c("2 Years old","10 Years old", "24 Years old", "Remnant")
Chao2index<-as.numeric(c("5","26.5","32","69.3"))
DataframeChao2<-data.frame(Landcover,Chao2index)
#Visualize the dataframe
DataframeChao2
#order the factors
DataframeChao2$Landcover<-factor(DataframeChao2$Landcover, levels=c("2 Years old", "10 Years old", "24 Years old", "Remnant"))
#ggplot
library(ggplot2)
ggplot(DataframeChao1, aes(x = Landcover, y = Chao1index)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Landcover Type", y = "Chao1 Index", title = "Chao1 Index by Landcover") +
  theme_minimal()

#Permanova Tree Species
#Tree community composition variation
#Permanova
library(vegan)
#Bray Curtsie as distance metric because is the one used for abundance data
#Community matrix (species abundance data)
community_matrix <- read.csv("TreeSpecies2.csv", h=T)
community_matrix$Location<-factor(community_matrix$Location,levels=c("TwoYears","TenYears","TwentyfourYears","Remnant"))
ncol(community_matrix)
community_matrix
# Calculate distance matrix (e.g., Bray-Curtis distance)
distance_matrix <- vegdist(community_matrix[,2:57], method = "bray")
#Visualization of distance matrix
distance_matrix
# Perform PERMANOVA
TestPermanovaTreeSpecies<-adonis2(distance_matrix ~ community_matrix$Location, data=community_matrix)

#Test results
TestPermanovaTreeSpecies

library(ggplot2)

#Perform NMDS
nmds_results<-metaMDS(distance_matrix,k=2)

# Create a data frame for plotting
data_for_plot <- data.frame(NMDS1 = nmds_results$points[, 1],
                            NMDS2 = nmds_results$points[, 2],
                            Group = community_matrix$Location)

# Create the plot
ggplot(data_for_plot, aes(x = NMDS1, y = NMDS2, color = community_matrix$Location)) +
  geom_point() +
  stat_ellipse(aes(color = community_matrix$Location), type = "norm") +
  labs(x = "NMDS1", y = "NMDS2", title = "NMDS Plot with group overlap")
#Post hoc analysis for PerManova
#Post hoc
install.packages('devtools')
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)
pair.mod<-pairwise.adonis(distance_matrix,factors=community_matrix$Location)
pair.mod
#Biomass calculations
#Equation 4 Chave 2014 for trees having DBH and height: AGB = 0.0673 * (ρ * D^2 * H)^0.976 
#Equation Chave 2005 for moist tropical forests: AGB = ρ * exp(-1.499 + 2.148 * ln(D) + 0.207 * (ln(D))^2 – 0.0281 * (ln(D))^3 )
#Biomass per ha
#Sum (biomass of plot)/0.04(plot dimension in ha)
#Carbon stock
#Biomass*0.47
#Stems per ha
#Small stems
Small_stems<-read.table("Small_stems.csv",h=T,sep=",")
Small_stems
Small_stems$Landcover<-factor(Small_stems$Landcover,levels=c("TwoYearsOld","TenYearsOld","TwentyFourYearsOld","Remnant"))
#Data normality
#Shapiro is better in this case because Kolmogorov is for sample size higher than 80
plot(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot)
qqnorm(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot)
ks.test(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot, "pnorm")
shapiro.test(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot)
#Statistically different from normality
#Thus I use non-parametric tests for comparison, which considers 1 numeric (number of stems) and one categorical (landcover type). Thus I use the Chi-square, not assumin ordination between landcover types.
#I did not assume ordination because of variability in regrowth, making the abundance like an upside-down bell shape.
#Plot to see disposition
colori<-c("orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Small_stems$Landcover)
colori
plot(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot ~Small_stems$Landcover,col=colori)
#Chi test
chisq.test(Small_stems$Landcover,Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot,correct=T)
#Not statistically significant. TO ADD THE O2NB values
#DBH regrowth class 5 to 10 cm DBH
#DBH Regrowth
Regrowth<-read.table("DBH_Regrowth.csv",h=T,sep=",")
Regrowth
Regrowth$Landcover<-factor(Regrowth$Landcover,levels=c("2 years old","10 years old","24 years old","Remnant"))
#Check data normality
plot(Regrowth$DBH..cm.)
qqnorm(Regrowth$DBH..cm.)
ks.test(Regrowth$DBH..cm., "pnorm")
shapiro.test(Regrowth$DBH..cm.)

#Data are not normal, so I use a Chi-square test (also because I do not assume ordination, this is why I am not using the kruskal wallis)

#Plot to see disposition
colori<-c("orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Regrowth$Landcover)
colori
plot(Regrowth$DBH..cm.~ Regrowth$Landcover,col=colori)
#Chi test
chisq.test(Regrowth$Landcover,Regrowth$DBH..cm.,correct=T)
#Not statistically significant.
#Acacia focused analyses
#Acacia focused
AcaciaFocused<-read.table("AcaciaDevelopment.csv",h=T,sep=",")
AcaciaFocused
#Make factors
AcaciaFocused$Landtype<-factor(AcaciaFocused$Landtype, levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
colori<-c("orange","red", "purple","blue")
names(colori)<-levels(AcaciaFocused$Landtype)
colori #to check
colori[as.character(AcaciaFocused$Landtype)]
unique(AcaciaFocused$Landtype)
#DBH-Landtype
plot(AcaciaFocused$DBH..cm.~AcaciaFocused$Landtype,col=colori)
chisq.test(AcaciaFocused$Landtype,AcaciaFocused$DBH..cm.)
#DBH-Height
plot(AcaciaFocused$DBH..cm.~AcaciaFocused$Height..m.,col=colori[as.character(AcaciaFocused$Landtype)],pch=16)
#DBH-Total Biomass
plot(AcaciaFocused$DBH..cm.~AcaciaFocused$AGB_Total,col=colori[as.character(AcaciaFocused$Landtype)],pch=16)
#AGB_TOTAL-Soil pH
plot(AcaciaFocused$AGB_Total~AcaciaFocused$Soil_pH ,col=colori[as.character(AcaciaFocused$Landtype)],pch=16)
#AGB_TOTAL-pH kit
plot(AcaciaFocused$AGB_Total~AcaciaFocused$pH_kit,col=colori[as.character(AcaciaFocused$Landtype)],pch=16)
#CarbonStock-Landtype
plot(AcaciaFocused$CarbonPerha~AcaciaFocused$Landtype,col=colori)
#Biomass-Landtype
plot(AcaciaFocused$AGB_Total~AcaciaFocused$Landtype,col=colori)
chisq.test(AcaciaFocused$Landtype,AcaciaFocused$AGB_Total)
