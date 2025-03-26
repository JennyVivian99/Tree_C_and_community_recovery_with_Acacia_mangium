#### Permanova Community analysis ####
# Tree community composition variation excluding Grassland (since only 1 Cocus nucifera in 1 plot, and the unknown (2))
# Removed the unknown since the species is not known and thus may create biases if considered becasue treated as a level.
# Bray Curtis as distance metric because is the one used for abundance data
# Community matrix (species abundance data)
community_matrix <- read.csv("TreeSpecies2.csv", h=T)
# Exclude the plots with zero values, otherwise the PerManova will not work
community_matrix <- community_matrix[!community_matrix$Plot %in% c("G1SB", "G2NB", "G3SB","G4NB", "E3SB"), ]
community_matrix
community_matrix$Landcovertype<-factor(community_matrix$Landcovertype,levels=c("TwoYearsOld","TenYearsold","TwentyfourYearsold","Remnant"))
# Change names to the Landcover type, to make them more coincise and clear
levels(community_matrix$Landcovertype)<-c("2 years old","10 years old", "24 years old", "Remnant forest")
community_matrix
# Calculate distance matrix (e.g., Bray-Curtis distance)
distance_matrix <- vegdist(community_matrix[,3:57], method = "bray")
# Visualization of distance matrix
distance_matrix
# Perform PERMANOVA
TestPermanovaTreeSpecies<-adonis2(distance_matrix ~ community_matrix$Landcovertype, data=community_matrix)
# See test results
TestPermanovaTreeSpecies
# Perform NMDS
nmds_results<-metaMDS(distance_matrix,k=2)
plot(nmds_results)
# Create a data frame for plotting
data_for_plot <- data.frame(NMDS1 = nmds_results$points[, 1],
                            NMDS2 = nmds_results$points[, 2],
                            Group = community_matrix$Landcovertype)
# Create the plot
ggplot(data_for_plot, aes(x = NMDS1, y = NMDS2, color = community_matrix$Landcovertype)) +
  geom_point(size=3) +
  stat_ellipse(aes(color = community_matrix$Landcovertype), type = "norm", linetype=1, size=1.5) +
  labs(x = "NMDS1", y = "NMDS2", title = "NMDS Plot of tree communities for each landcover")+theme_bw()+theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
# Post hoc analysis
pair.mod<-pairwise.adonis(distance_matrix,factors=community_matrix$Landcovertype)
# Visualise
pair.mod

#### Gamma diversity through iNEXT ####
# iNEXT package for gamma diversity visualisation
# Load the community matrix (species abundance data)
community_matrix <- read.csv("TreeSpecies.csv", h=T)
# Divide in matrixes for each landcover type
community_matrix1<-community_matrix[,2:4]
community_matrix2<-community_matrix[,5:8]
community_matrix3<-community_matrix[,9:12]
community_matrix4<-community_matrix[,13:16]
# Combine them in a list
community_matrix2.0<-list(community_matrix1,community_matrix2,community_matrix3,community_matrix4)
# Beta diversity matrix calculation
iNEXTbeta3D(community_matrix2.0,	diversity	= "TD",	q	= c(0,	1,	2),	datatype	= "abundance",
            base	= "coverage")
# Save the results for taxonomic richness, q=0
iNEXTbetaNMDS<-iNEXTbeta3D(community_matrix2.0,datatype = "abundance",diversity = "TD",q = 0)
# Visualisation of the diversities. In this case, 
# by framing the landcover types as different datasets, it should be considered
# the gamma diversity, to see the different community composition among landcover types, and thus 
# among the datasets.
ggiNEXTbeta3D(iNEXTbetaNMDS,	type	= 'B')

#### Hills numbers for tree diversity ####
# Diversity studied is the richness, the Shannon entropy and the inverse of Simpson. For Shannon, the higher the values,
# the higher is the entropy. For the inverse of Simpson, the higher is the value, the higher is the evenness.
# Diversity for each landcover type, summing the data of each plot
# Load the data (this dataset does not include the unknown (2))
TreeSpeciesLandcover<-read.table("TreeSpeciesLandcover.csv",sep=',',h=T)
TreeSpeciesLandcover$Species<-as.factor(TreeSpeciesLandcover$Species)
str(TreeSpeciesLandcover$Species)
# Set as dataframe
TreeSpeciesLandcover<-as.data.frame(TreeSpeciesLandcover)
numeric_dataLandcover<-TreeSpeciesLandcover[,sapply(TreeSpeciesLandcover, is.numeric)]
# Set as matrix
numeric_matrixLandcover<-as.matrix(numeric_dataLandcover)
# Omit NAs
numeric_matrix_no_naLandcover<-na.omit(numeric_matrixLandcover)
# Check the structure
str(numeric_dataLandcover)
# Write the names of the colums, otherwise R will inser an "X" in front of them, 
# because it does not accept the name to start with a number, if not specified as follows:
colnames(numeric_matrix_no_naLandcover)<-c("Grassland","2 years old", "10 years old", "24 years old", "Remnant")
# Check
numeric_matrix_no_naLandcover
# Calculate the indexes
D_abundLandcover<-iNEXT(numeric_matrix_no_naLandcover, datatype="abundance", q=c(0,1,2), se=T)
# See the results
D_abundLandcover
print(D_abundLandcover)       
# Visualise the results
plot(D_abundLandcover)
# indexes
ChaoRichness(numeric_matrix_no_naLandcover, datatype="abundance")
ChaoShannon(numeric_matrix_no_naLandcover, datatype="abundance")
ChaoSimpson(numeric_matrix_no_naLandcover, datatype="abundance")

#### Linear Correlation Simpson Hill and Richness ####
LinearCor<-read.table("LinearCorrRichnessSimpsonHill.csv",sep=',',h=T)
summary(lm(LinearCor$Richness~LinearCor$SimpsonHill))

#### Pielou index ####
# Pielou evenness considering the landcover, thus the various samples
# Load the data
PielouTreeSpecies<-read.table("TreeSpeciesLandcover2.csv",sep=',',h=T)
# Set as dataframe
PielouTreeSpecies<-as.data.frame(PielouTreeSpecies)
Pielounumeric_data<-PielouTreeSpecies[,sapply(PielouTreeSpecies, is.numeric)]
# Check
str(Pielounumeric_data)
# Set as matrix
Pielounumeric_matrix<-as.matrix(Pielounumeric_data)
# NA omit to enable the analysis
Pielounumeric_matrix_no_na<-na.omit(Pielounumeric_matrix)
# Calculation
# Shannon's H'
H <- diversity(Pielounumeric_matrix)
# Check
H
# Observed Richness
richness <- specnumber(Pielounumeric_matrix)
# Check
richness
# Pielou's Evenness
evenness <- H/log(richness)
# Check
evenness
# Visualisation
alpha <- data.frame(PielouTreeSpecies$Species, pielou = evenness)
colnames(alpha)<-c("Landcover","Pielou_evenness")
# Check
alpha
# Set landcovers as factor
alpha$Landcover<-factor(alpha$Landcover,levels=c("2 years old","10 years old","24 years old","Remnant"))
#Plot (to omit NA if to use this plot)
ggplot(alpha, aes(x = Landcover, y = Pielou_evenness, fill = Landcover)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "turbo",discrete = T) +
  ylab("Pielou's Evenness") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

#### Chao indexes ####
# Chao1 index calculation
# Load the data and convert in numeric the numbers of the trees abundance
# This dataset does not have the unknown (2)
Chao1community_matrix <- read.csv("TreeSpecies.csv", h=T, sep=",")
Chao1community_matrix[2:57,2:16] <- apply(Chao1community_matrix[2:57,2:16],2, as.numeric)
# Check
str(Chao1community_matrix)
dim(Chao1community_matrix)
# Chao1 for 2 years old
chao1(Chao1community_matrix[2:55,2:4], taxa.row = T)
# Chao1 for 10 years old
chao1(Chao1community_matrix[2:55,5:8], taxa.row = T)
# Chao1 for 24 years old
chao1(Chao1community_matrix[2:55,9:12], taxa.row = T)
# Chao1 for Remnant
chao1(Chao1community_matrix[2:55,13:16], taxa.row = T)
# Display results of Chao1 index
Landcover<-c("2 Years old","10 Years old", "24 Years old", "Remnant")
Chao1index<-as.numeric(c("3","21","29","64.6"))
DataframeChao1<-data.frame(Landcover,Chao1index)
#Visualize the dataframe
DataframeChao1
# Order the factors
DataframeChao1$Landcover<-factor(DataframeChao1$Landcover, levels=c("2 Years old", "10 Years old", "24 Years old", "Remnant"))
# Visualise
ggplot(DataframeChao1, aes(x = Landcover, y = Chao1index)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Landcover Type", y = "Chao 1 Index", title = "Chao 1 Index by Landcover") +
  theme_minimal()

# Chao 2 index calculation
# Load the data
Chao2community_matrix <- read.csv("TreeSpecies.csv", h=T, sep=",")
Chao2community_matrix[2:57,2:16] <- apply(Chao2community_matrix[2:57,2:16],2, as.numeric)
# Check
str(Chao2community_matrix)
dim(Chao2community_matrix)
# Chao2 for 2 years old
chao2(Chao2community_matrix[2:55,2:4], taxa.row = T)
# Chao2 for 10 years old
chao2(Chao2community_matrix[2:55,5:8], taxa.row = T)
# Chao2 for 24 years old
chao2(Chao2community_matrix[2:55,9:12], taxa.row = T)
# Chao2 for Remnant
chao2(Chao2community_matrix[2:55,13:16], taxa.row = T)
# Display results of Chao2 index
Landcover<-c("2 Years old","10 Years old", "24 Years old", "Remnant")
Chao2index<-as.numeric(c("3","22.7","32","66.5"))
DataframeChao2<-data.frame(Landcover,Chao2index)
# Visualize the dataframe
DataframeChao2
# Order the factors
DataframeChao2$Landcover<-factor(DataframeChao2$Landcover, levels=c("2 Years old", "10 Years old", "24 Years old", "Remnant"))
# Visualise
ggplot(DataframeChao2, aes(x = Landcover, y = Chao2index)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Landcover Type", y = "Chao 2 Index", title = "Chao 2 Index by Landcover") +
  theme_minimal()

#### Native and exotic analysis ####
# Analysis of the number of native and exotic species (counting the different species) per landcover
# Load the data (if not done before) (not considered the unknown)
Raw_data_DBH<-read.table("GoodPaired_analyses_DBH_BiomassNoCoconut2.csv",h=T,sep=",")
# Transform as factor
Raw_data_DBH$Landcover <- factor(Raw_data_DBH$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
Raw_data_DBH$Native<-as.factor(Raw_data_DBH$Native)
# Check
levels(Raw_data_DBH$Landcover)
levels(Raw_data_DBH$Native)
# Construction of matrix with number of native species per landcover
NLandcover<-c("Grassland","2 years old","10 years old", "24 years old","Remnant")
ELandcover<-c("Grassland","2 years old","10 years old", "24 years old","Remnant")
# First 4 values for Native, second 4 for Exotic
Landcover<-c(NLandcover,ELandcover)
Landcover
Native_Exotic<-c((rep("N",5)),(rep("E",5)))
Native_Exotic
#Empty vectors for the Native and Exotic trees
Native<-0
Exotic<-0
# Write into the vectors the number of Native species
Native[1]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "Grassland"]))
Native[2]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "2 years old"]))
Native[3]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "10 years old"]))
Native[4]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "24 years old"]))
Native[5]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "Remnant"]))
# Check
Native
# Write into the vectors the number of Exotic species
unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "N" & Raw_data_DBH$Landcover == "Remnant"])
Exotic[1]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "Grassland"]))
Exotic[2]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "2 years old"]))
Exotic[3]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "10 years old"]))
Exotic[4]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "24 years old"]))
Exotic[5]<-length(unique(Raw_data_DBH$Scientific__name[Raw_data_DBH$Native == "E" & Raw_data_DBH$Landcover == "Remnant"]))
# Check
Exotic
# Merge the vectors together
Origin<-c(Native,Exotic)
# Check
Origin
# Dataset construction
NativeExotic.df<-data.frame(Landcover,Origin, Native_Exotic)
# Check
summary(NativeExotic.df)
NativeExotic.df
# Transform as factor
NativeExotic.df$Landcover<-factor(NativeExotic.df$Landcover, levels= c("Grassland","2 years old","10 years old", "24 years old","Remnant"))
NativeExotic.df$Native_Exotic<-factor(NativeExotic.df$Native_Exotic, levels= c("N","E"))
# Plot to visualise
ggplot(NativeExotic.df, aes(fill=Native_Exotic, y=Origin, x=Landcover)) +  scale_fill_viridis_d(option = "cividis")+ geom_bar(position="stack", stat="identity")+ labs(title="Number of native and exotic species", x= 'Landcover',y='Number of species')

# Test if the number of exotic and native species is statistically different across landcover types
# Load the dataset
NativeExoticDifference<-read.table("NativeExoticDifference.csv",h=T,sep=",")
# Check
NativeExoticDifference
# Group by plot, Landcover, and SpeciesOrigin, and count occurrences
species_counts <- NativeExoticDifference %>%
  group_by(Sample__ID, Landcover,Hill_side, Native) %>%
  summarise(Count = n())
# Check
species_counts
# Use the pivot wider() function to separate native and exotic counts
species_counts_wide <- species_counts %>%
  pivot_wider(
    id_cols = c(Sample__ID, Landcover, Hill_side),
    names_from = Native,
    values_from = Count,
    values_fill = list(Count = 0)
  )
# Check
species_counts_wide
# Calculate total species count per plot and landcover
species_counts_wide <- species_counts_wide %>%
  mutate(Total_Species = N + E) #I am not considering the NA
# Print or visualize the results
print(species_counts_wide)

# Exotic analysis
# Reframe the levels of the plots
species_counts_wide$Landcover<-factor(species_counts_wide$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Check the variance
leveneTest(species_counts_wide$E~species_counts_wide$Landcover)
# In my case the variance is not homogeneous
# Check distribution of the data to then transform them
hist(species_counts_wide$E)
# In my case the data are positively skewed, check qqplot after lmm
# Implementaion of a lmm
modelE<-lmer(species_counts_wide$E~species_counts_wide$Landcover+(1|species_counts_wide$Hill_side))
residuals_model<-residuals(modelE)
summary(modelE)
#check residuals
plot(modelE)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# In my case the residuals are ok, not evident clear patterns
#For pairwise comparisons
emms<-emmeans(modelE, pairwise ~ Landcover)
emms
#The significance do not change among 2-10-24-Remnant
#Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
#visualise the results
emms_backtransformed
#including the p-value
pairs(emms_backtransformed)

#Native
#Check the variance
leveneTest(species_counts_wide$N~species_counts_wide$Landcover)
#the variance is homogeneous
#check distribution of the data to then transform them
hist(species_counts_wide$N)
# In my case the data are positively skewed, check qqplot after lmm
# Implementaion of a lmm transforming the data with sqrt to implement normality
modelN<-lmer(sqrt(species_counts_wide$N)~species_counts_wide$Landcover+(1|species_counts_wide$Hill_side))
residuals_model<-residuals(modelN)
summary(modelN)
#check residuals
plot(modelN)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# In my case the residuals are ok, not evident clear patterns
#For pairwise comparisons
emms<-emmeans(modelN, pairwise ~ Landcover)
emms
#The significance do not change among 2-10-24-Remnant
#Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
#visualise the results
emms_backtransformed
#including the p-value
pairs(emms_backtransformed)

# Spatial models for native and exotic species
# In this dataset the plots with just NA are removed (G1-G2-G4-E3)
# Load the dataset
NativeExoticDifference<-read.table("NativeExoticDifferenceSpatial.csv",h=T,sep=",")
# Check
NativeExoticDifference
# Group by plot, Landcover, and SpeciesOrigin, and count occurrences
species_counts <- NativeExoticDifference %>%
  group_by(Sample__ID, Landcover,Hill_side, Native, Lat_ID, Long_ID) %>%
  summarise(Count = n())
# Check
species_counts
# Use the pivot wider() function to separate native and exotic counts
species_counts_wide <- species_counts %>%
  pivot_wider(
    id_cols = c(Sample__ID, Landcover, Hill_side, Lat_ID, Long_ID),
    names_from = Native,
    values_from = Count,
    values_fill = list(Count = 0)
  )
# Check
species_counts_wide
# Calculate total species count per plot and landcover
species_counts_wide <- species_counts_wide %>%
  mutate(Total_Species = N + E) #I am not considering the NA
# Print or visualize the results
print(species_counts_wide)

#Exotic
# Reframe the levels of the plots
species_counts_wide$Landcover<-factor(species_counts_wide$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Check
summary(species_counts_wide)
species_counts_wide
# Exploration of the basic model
data.spatialCor.gls <- gls(E ~ Landcover, data= species_counts_wide,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data (if not already done for the Native analysis) to a spatial points data frame (SPDF). This is also the format required to implement the models
# coordinates(species_counts_wide) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(species_counts_wide$E ~ 1, data=species_counts_wide)
plot(exp_variogram)
# Extract distance and gamma values
exp_variogram_data <- data.frame(distance = exp_variogram$dist, gamma = exp_variogram$gamma)
# Visualise
ggplot(exp_variogram_data, aes(x = distance, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(x = "Lag (h)", y = "Semivariance") +
  theme_minimal()
# Show non-stationary effect, not good a spatial model with a structure, but to try:
# Implementation of the models considering the spatial correlation and the different structures of the variograms
# Models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsGaus <- gls(E ~ Landcover, data= species_counts_wide,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsExp <- gls(E ~ Landcover, data= species_counts_wide,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(E ~ Landcover, data= species_counts_wide,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(E ~ Landcover, data= species_counts_wide,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(E ~ Landcover, data= species_counts_wide,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# The best fit is from the model that consider a spherical structural correlation. 
# Run the model
data.spatialCor.glsSpher
# See better the results
summary(data.spatialCor.gls)
#check residuals
residuals_model<-residuals(data.spatialCor.glsSpher)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# Normal, no patterns visible
# Post hoc comparisons
emms<-emmeans(data.spatialCor.glsSpher, specs = pairwise ~ Landcover, mode = "appx-satterthwaite")
# See the results
emms
# AIC models comparison, between spatial and not spatial
# Analyse the AIC
AIC(data.spatialCor.gls, modelE)
# The spatial model have a higher AIC, but it also allows for more df, while the model without spatial analysis 
# has one less df but much lower AIC
# Despite the lower AIC, the non stationarity of the model suggests to use simpler lmm

# Native analysis
# Reframe the levels of the plots
species_counts_wide$Landcover<-factor(species_counts_wide$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Check
summary(species_counts_wide)
# Exploration of the basic model
data.spatialCor.gls <- gls(N ~ Landcover, data= species_counts_wide,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(species_counts_wide) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(species_counts_wide$N ~ 1, data=species_counts_wide)
plot(exp_variogram)
# Extract distance and gamma values
exp_variogram_data <- data.frame(distance = exp_variogram$dist, gamma = exp_variogram$gamma)
# Visualise
ggplot(exp_variogram_data, aes(x = distance, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(x = "Lag (h)", y = "Semivariance") +
  theme_minimal()
# Implementation of the models considering the spatial correlation and the different structures of the variograms
# Models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsGaus <- gls(N ~ Landcover, data= species_counts_wide,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsExp <- gls(N ~ Landcover, data= species_counts_wide,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(N ~ Landcover, data= species_counts_wide,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(N ~ Landcover, data= species_counts_wide,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(N ~ Landcover, data= species_counts_wide,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# The best fit is from the model that consider a spherical structural correlation. 
# AIC models comparison, between spatial and not spatial
# Analyse the AIC
AIC(data.spatialCor.glsSpher, modelN)
# The spatial model have a higher AIC, but it also allows for more df, while the model without spatial analysis 
# has one less df but much lower AIC

#### Proportions of native and exotic ########
# Load the data
NativeExoticProportions<-read.table("NativeExoticDifference.csv",h=T,sep=",")
NativeExoticProportions
# Remove the grassland
# Raw_data_DBH<-Raw_data_DBH[Raw_data_DBH$Sample__ID != "G3SB", ]
# Ensure that the first factor displayed is 2 years old, not 10 years old plantation:
NativeExoticProportions$Landcover <- factor(NativeExoticProportions$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
levels(NativeExoticProportions$Landcover)
NativeExoticProportions$Native<-as.factor(NativeExoticProportions$Native)
# Count total trees by landcover (specify the usage of dyplr, otherwise Count=n() won't work using plyr)
Total_trees <- NativeExoticProportions %>%
  filter(!is.na(Native)) %>%
  group_by(Landcover) %>%
  dplyr::summarise(Count = n(), .groups = "drop")
# Check
Total_trees
# Count native species by landcovertype
native_by_landcover <- NativeExoticProportions%>%
  filter(Native == "N") %>%
  group_by(Landcover) %>%
  dplyr::summarize(Native_Count = n(), .groups = "drop")
# Check
print(native_by_landcover)
# Count exotic species by landcovertype
exotic_by_landcover <- NativeExoticProportions%>%
  filter(Native == "E") %>%
  group_by(Landcover) %>%
  dplyr::summarize(Exotic_Count = n(), .groups = "drop")
# Check
print(exotic_by_landcover)

# Dataset with the counts
# First, add the row for grassland for exotic
grassland<-data.frame(Landcover="Grassland",Exotic_Count=0)
exotic_by_landcover<-rbind(grassland,exotic_by_landcover) #important the order
NativeExoticCounts<-data.frame(Total_trees,native_by_landcover$Native_Count,exotic_by_landcover$Exotic_Count)
# Check
NativeExoticCounts

# Calculate the proportions
ProportionsNativeTrees<-NativeExoticCounts$native_by_landcover.Native_Count/NativeExoticCounts$Count
ProportionsExoticTrees<-NativeExoticCounts$exotic_by_landcover.Exotic_Count /NativeExoticCounts$Count
Landcover<- c("Grassland","2 years old", "10 years old", "24 years old", "Remnant")
# Dataframe of proprotions
Landcover1<-c(Landcover,Landcover)
TotalProportions2<-c(ProportionsNativeTrees,ProportionsExoticTrees)
Origins<-c(rep("N",5),rep("E",5))
TotalProportions<-data.frame(Landcover1,TotalProportions2,Origins)
# Check
TotalProportions
# Adjust factor
TotalProportions$Landcover1 <- factor(TotalProportions$Landcover1, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))

# Plot excluding grassland
ggplot(TotalProportions, aes(x = Landcover1, y = TotalProportions2, fill = Origins)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "cividis") +
  scale_x_discrete(limits = c("2 years old", "10 years old", "24 years old", "Remnant")) +
  labs(title = "Proportion of Native and Exotic Species by Landcover",
       x = "Landcover",
       y = "Proportion of Species",
       fill = "Species Type") +
  theme_bw() + #Clean theme
  theme(panel.grid = element_blank()) #Remove gridlines for even cleaner look
# Note that the warning message is normal since I escluded grassland, 
# and 2 rows are the ones for Exotic and Native proportion in that level.
# Analyses
# Load the data
NativeExoticAnalyses<-read.table("NativeExoticCountsForAnalyses.csv",h=T,sep=",")
NativeExoticAnalyses$Landcovertype<- factor(NativeExoticAnalyses$Landcovertype, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
NativeExoticAnalyses$Hill_Side<-as.factor(NativeExoticAnalyses$Hill_Side)
summary(NativeExoticAnalyses)
# Check variance
leveneTest(NativeExoticAnalyses$Counts_Native_Exotic ,NativeExoticAnalyses$Landcovertype)
# In my case the varance is significantly different from each other
# Check the data distribution
hist(NativeExoticAnalyses$Counts_Native_Exotic)
# In my case are positively skewed
# Check data normality. Shapiro is better in this case because Kolmogorov is for sample size higher than 80. Note that the residual should be normal. To check after the model is set.
plot(NativeExoticAnalyses$Counts_Native_Exotic)
qqnorm(NativeExoticAnalyses$Counts_Native_Exotic)
ks.test(NativeExoticAnalyses$Counts_Native_Exotic, "pnorm")
shapiro.test(NativeExoticAnalyses$Counts_Native_Exotic)
# Data are not normally distributed, but the residuals are the important ones
# Zero are present for the grassland, and for one plot of the 2 years old plantation, but none of the landcover types has only zeros.
# My data are paired in S and N, thus I will insert this as random effect. The elevation is highly correlated with the landcover, so that information
# is already present in the model.
# I fit a lmm which considers REML
model_ExoticNativeProportions<-lmer(log1p(NativeExoticAnalyses$Counts_Native_Exotic)~NativeExoticAnalyses$Landcovertype+(1|NativeExoticAnalyses$Hill_Side))
residuals_model_ExoticNativeProportions<-residuals(model_ExoticNativeProportions)
summary(model_ExoticNativeProportions)
# Check residuals
plot(model_ExoticNativeProportions)
plot(residuals_model_ExoticNativeProportions)
qqnorm(residuals_model_ExoticNativeProportions)
qqline(residuals_model_ExoticNativeProportions)
# Transformation with "log1p" help the qq plot and the homoschedasticity
# Check variance
leveneTest(log1p(NativeExoticAnalyses$Counts_Native_Exotic) ,NativeExoticAnalyses$Landcovertype)
# Still not homogeneous, but lmm can handle that
# For pairwise comparisons
emms<-emmeans(model_ExoticNativeProportions, pairwise ~ Landcovertype)
# Visualise
emms
# Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# Visualise the results
emms_backtransformed
# Including the p-value
pairs(emms_backtransformed)
