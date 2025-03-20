#### Acacia mangium contribution analysis ####
#Acacia mangium contribution to Biomass and therefore C stock, with grassland
# Load the data
ACBiomass<-read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
# Transform into factor
ACBiomass$Landcovertype<- factor(ACBiomass$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
ACBiomass$Hill_side<-as.factor(ACBiomass$Hill_side)
# Check
summary(ACBiomass)
# Plot to visualise
plot(ACBiomass$Acacia.mangium)
plot(ACBiomass$Acacia.mangium~ACBiomass$Landcovertype)
# Check the data variance
leveneTest(ACBiomass$Acacia.mangium,ACBiomass$Landcovertype)
# In my case the data are homogeneous
# Check the data distribution
hist(ACBiomass$Acacia.mangium)
# In my case they are bimodal, will check the residual after model implementation.
# I fit a lmm which considers REML
model_ACBiomass<-lmer(ACBiomass$Acacia.mangium~ACBiomass$Landcovertype+(1|ACBiomass$Hill_side))
plot(model_ACBiomass)
summary(model_ACBiomass)
#check residuals
ACresiduals_model<-residuals(model_ACBiomass)
plot(ACresiduals_model)
qqnorm(ACresiduals_model)
qqline(ACresiduals_model)
# In my case the residuals are ok, not clear patterns
# To implement pairwise comparisons
emms<-emmeans(model_ACBiomass, pairwise ~ Landcovertype)
emms
# The significance do not change among 2-10-24-Remnant
# Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# Visualise the results
emms_backtransformed
# Including the p-value
pairs(emms_backtransformed)

# Acacia biomass comparison with spatial model with grassland
# Load the data
ACBiomass<-read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
# Transform into factor
ACBiomass$Landcovertype<- factor(ACBiomass$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
ACBiomass$Hill_side<-as.factor(ACBiomass$Hill_side)
# Check
summary(ACBiomass)
# Exploration of the basic model
data.spatialCor.gls <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(ACBiomass) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(ACBiomass$Acacia.mangium ~ 1, ACBiomass)
plot(exp_variogram)
# Extract distance and gamma values
exp_variogram_data <- data.frame(distance = exp_variogram$dist, gamma = exp_variogram$gamma)
# Visualise
ggplot(exp_variogram_data, aes(x = distance, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(x = "Lag (h)", y = "Semivariance") +
  theme_minimal()
# The gaussian structure seems to be the one that fits the best these points
# Implementation of the models considering the spatial correlation and the different structures of the variograms
# Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsGaus <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsExp <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
# No linear model implemented because it does not arrive at convergence
# data.spatialCor.glsLin <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
#                               correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
#                               method = "REML")
data.spatialCor.glsRatio <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# The best fit is from the model that consider a gaussian structural correlation. 
# Run the model
data.spatialCor.glsGaus
# See better the results
summary(data.spatialCor.glsGaus)
#check residuals
residuals_model<-residuals(data.spatialCor.glsGaus)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# Normal, no patterns visible
# Post hoc comparisons
emms<-emmeans(data.spatialCor.glsGaus, specs = pairwise ~ Landcovertype, mode = "appx-satterthwaite")
# See the results 
emms
# AIC models comparison, between spatial and not spatial (both without grassland)
# Load the data (if not done before)
ACBiomass<-read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
# Transform into factor
ACBiomass$Landcovertype<- factor(ACBiomass$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
ACBiomass$Hill_side<-as.factor(ACBiomass$Hill_side)
# Fit the model to compare
model_ACBiomass<-lmer(ACBiomass$Acacia.mangium~ACBiomass$Landcovertype+(1|ACBiomass$Hill_side))
# Analyse the AIC
AIC(data.spatialCor.glsSpher, model_ACBiomass)
# The spatial model have a slightly higher AIC, but it also allows for more df, while the model without spatial analysis 
# has one less df but lower AIC

# Acacia mangium contribution to Biomass and therefore C stock, without grassland
# Load the data
ACBiomass<-read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
# Remove the grassland
ACBiomass <- ACBiomass[ACBiomass$Landcovertype != "Grassland", ]
# Transform into factor
ACBiomass$Landcovertype<- factor(ACBiomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
ACBiomass$Hill_side<-as.factor(ACBiomass$Hill_side)
# Check
summary(ACBiomass)
# Plot to visualise
plot(ACBiomass$Acacia.mangium)
plot(ACBiomass$Acacia.mangium~ACBiomass$Landcovertype)
# Check the data variance
leveneTest(ACBiomass$Acacia.mangium,ACBiomass$Landcovertype)
# In my case the data are homogeneous
# Check the data distribution
hist(ACBiomass$Acacia.mangium)
# In my case they are bimodal, will check the residual after model implementation.
# I fit a lmm which considers REML
model_ACBiomass<-lmer(ACBiomass$Acacia.mangium~ACBiomass$Landcovertype+(1|ACBiomass$Hill_side))
plot(model_ACBiomass)
summary(model_ACBiomass)
#check residuals
ACresiduals_model<-residuals(model_ACBiomass)
plot(ACresiduals_model)
qqnorm(ACresiduals_model)
qqline(ACresiduals_model)
# In my case the residuals are ok, not clear patterns
# To implement pairwise comparisons
emms<-emmeans(model_ACBiomass, pairwise ~ Landcovertype)
# The significance do not change among 2-10-24-Remnant
# Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# Visualise the results
emms_backtransformed
# Including the p-value
pairs(emms_backtransformed)

# Acacia biomass comparison with spatial model without grassland
# Load the data
ACBiomass<-read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
# Remove the grassland
ACBiomass <- ACBiomass[ACBiomass$Landcovertype != "Grassland", ]
# Transform into factor
ACBiomass$Landcovertype<- factor(ACBiomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
ACBiomass$Hill_side<-as.factor(ACBiomass$Hill_side)
# Check
summary(ACBiomass)
# Exploration of the basic model
acdata.spatialCor.gls <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                             method = "REML")
plot(acdata.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(ACBiomass) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(ACBiomass$Acacia.mangium ~ 1, ACBiomass)
plot(exp_variogram)
# Extract distance and gamma values
exp_variogram_data <- data.frame(distance = exp_variogram$dist, gamma = exp_variogram$gamma)
# Visualise
ggplot(exp_variogram_data, aes(x = distance, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(x = "Lag (h)", y = "Semivariance") +
  theme_minimal()
# The gaussian structure seems to be the one that fits the best these points
# Implementation of the models considering the spatial correlation and the different structures of the variograms
# Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsGaus <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsExp <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
#No linear model implemented because it does not arrive at convergence
#data.spatialCor.glsLin <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
#                               correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
#                               method = "REML")
data.spatialCor.glsRatio <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(Acacia.mangium ~ Landcovertype, data=ACBiomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(acdata.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# The best fit is from the model that consider a gaussian structural correlation. 
# Run the model
data.spatialCor.glsGaus
# See better the results
summary(data.spatialCor.glsGaus)
#check residuals
residuals_model<-residuals(data.spatialCor.glsGaus)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# Normal, no patterns visible
# Post hoc comparisons
emms<-emmeans(data.spatialCor.glsGaus, specs = pairwise ~ Landcovertype, mode = "appx-satterthwaite")
# See the results in the original scale
# Back-transform the EMMs
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed
# use pairs() to display also the p-values
pairs(emms_backtransformed)
# AIC models comparison, between spatial and not spatial (both without grassland)
# Load the data (if not done before)
ACBiomass<-read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
# Remove the grassland
ACBiomass <- ACBiomass[ACBiomass$Landcovertype != "Grassland", ]
# Transform into factor
ACBiomass$Landcovertype<- factor(ACBiomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
ACBiomass$Hill_side<-as.factor(ACBiomass$Hill_side)
# Fit the model to compare
model_ACBiomass<-lmer(ACBiomass$Acacia.mangium~ACBiomass$Landcovertype+(1|ACBiomass$Hill_side))
# Analyse the AIC
AIC(data.spatialCor.glsGaus, model_ACBiomass)
# The spatial model have a slightly higher AIC, but it also allows for more df, while the model without spatial analysis 
# has one less df but lower AIC

#### Biomass Contribution Visualisation ####
# Load the dataset
BiomassContrPlot<-read.table("BiomassContributionPlot.csv",h=T,sep=",")
# Check
summary(BiomassContrPlot)
# Transform as factor
BiomassContrPlot$Landcovertype<-factor(BiomassContrPlot$Landcovertype, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
BiomassContrPlot$Native<-as.factor(BiomassContrPlot$Native)
BiomassContrPlot$Species<-as.factor(BiomassContrPlot$Species)
str(BiomassContrPlot)

##test
# import data
data <- read.table("GoodPaired_analyses_DBH_BiomassNoCoconut2.csv",h=T,sep=",")
data
# calculate mean of proportions
BiomassContrPlotSummary <- BiomassContrPlot %>%
  group_by(Landcovertype, Native) %>%
  summarise(
    mean_Contribution = mean(Contributions),
    sd_Contribution = sd(Contributions),
    n = n(),
    .groups = "drop"
  )
# calculate sum of biomass per plot
# sum native and exotic biomasses
# calculate proportions of summed biomass/total biomass per plot per N/E
sum_biomass <- data %>% group_by(Sample__ID) %>% summarise(total_bio = sum(AGB_Total_individual, na.rm=T))

sum_bio_EN <- data %>% group_by(Sample__ID, Native) %>% summarise(NE_bio = sum(AGB_Total_individual, na.rm=T)) %>% 
  filter(Native == "N" | Native =="E")

# Bring in totals
merged_prop <- left_join(sum_bio_EN, sum_biomass, by="Sample__ID")
merged_prop <- merged_prop %>% mutate(prop = NE_bio/total_bio)

# Not needed but keep for possible dataset modification
#merged_prop$Landcover<-0
#merged_prop$Landcover[merged_prop$Sample__ID=="E1SB"]<-"2 years old"

unique_ID_landcover <- data %>% select(Sample__ID, Landcover) %>% group_by(Sample__ID, Landcover) %>% filter(row_number()==1)

# Bring in IDs to other data set from tidyverse
merged_prop <- left_join(merged_prop, unique_ID_landcover, by="Sample__ID")

# Now calculate means and sds at land cover level
mean_prop <- merged_prop %>% group_by(Landcover, Native) %>% summarise(mean_prop = mean(prop), sd_prop=sd(prop))

# Import mangium proportion dataset
mangium_contr<-read.table("AcaciaMangiumRelativeBiomassContributionForVisualisation.csv",h=T,sep=",")
# Make factors
mangium_contr$Landcovertype<-factor(mangium_contr$Landcovertype,levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Add column for Native
mangium_contr$Native<-rep("N",16)
# Create dummy variables for line, but may not be needed
mean_prop$x <- 0
mean_prop$x[mean_prop$Landcover=="2 years old"] <- 0.5
mean_prop$x[mean_prop$Landcover=="10 years old"] <- 1
mean_prop$x[mean_prop$Landcover=="24 years old"] <- 1.5
mean_prop$x[mean_prop$Landcover=="Remnant"] <- 2
# Reorder the factors
mean_prop$Landcover<-factor(mean_prop$Landcover,levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))

# Plot
ggplot() +geom_bar(data=mangium_contr, aes(x=Landcovertype, y=Average_Contribution, fill=Native), stat = "identity", position = position_dodge(0.5),
                   width=0.5, fill= "lightblue", alpha = 0.4) + 
  geom_errorbar(data=mangium_contr, aes(x=Landcovertype, ymin=Average_Contribution-STD, ymax=Average_Contribution+STD, color= "lightblue"), 
                width=0.2, position = position_dodge(0.5))+
  geom_point(data = mean_prop, aes(x=Landcover, y=mean_prop, colour=Native), cex=3, position=position_dodge(width=0.3))+
  geom_errorbar(data = mean_prop, aes(x=Landcover, ymin=mean_prop-sd_prop, ymax=mean_prop+sd_prop, color=Native), width=0.2, position=position_dodge(width=0.3))+ 
  geom_smooth(data = mean_prop, aes(x=as.numeric(Landcover), y=mean_prop, colour=Native), span = 0.5) + 
  theme_classic()+
  scale_color_manual(labels = c("Exotic","Acacia mangium","Native"), values = c("red", "lightblue", "darkgreen"))+
  labs(x = "Land cover", y = "Average biomass proportion contribution", 
       title = "Average biomass Contribution of native and exotic species by land cover",
       fill = "Species origins", 
       colour = "Species origins")

# Without grassland
# Filter out "Grassland" from both data frames
mangium_contr_filtered <- mangium_contr %>%
  filter(Landcovertype != "Grassland")

mean_prop_filtered <- mean_prop %>%
  filter(Landcover != "Grassland")

# Create dummy variables for line, but may not be needed
mean_prop$x <- 0
mean_prop$x[mean_prop$Landcover=="2 years old"] <- 0
mean_prop$x[mean_prop$Landcover=="10 years old"] <- 0.5
mean_prop$x[mean_prop$Landcover=="24 years old"] <- 1
mean_prop$x[mean_prop$Landcover=="Remnant"] <- 1.5
# Reorder the factors
mean_prop$Landcover<-factor(mean_prop$Landcover,levels = c("2 years old", "10 years old", "24 years old", "Remnant"))

# Create the ggplot with filtered data
ggplot() +
  geom_bar(data=mangium_contr_filtered, aes(x=Landcovertype, y=Average_Contribution, fill=Native), 
           stat = "identity", position = position_dodge(0.5), width=0.5, fill= "lightblue", alpha = 0.4) + 
  geom_errorbar(data=mangium_contr_filtered, aes(x=Landcovertype, ymin=Average_Contribution-STD, ymax=Average_Contribution+STD, color="lightblue"), 
                width=0.2, position = position_dodge(0.5)) +
  geom_point(data = mean_prop_filtered, aes(x=Landcover, y=mean_prop, colour=Native), cex=3, position=position_dodge(width=0.3))+
  geom_errorbar(data = mean_prop_filtered, aes(x=Landcover, ymin=mean_prop-sd_prop, ymax=mean_prop+sd_prop, colour=Native), width=0.2, position=position_dodge(width=0.3))+ 
  geom_smooth(data = mean_prop_filtered, aes(x=as.numeric(Landcover), y=mean_prop, colour=Native), span=0.5) + 
  theme_classic() +
  scale_color_manual(labels = c("Exotic","Acacia mangium","Native"), values = c("red", "lightblue", "darkgreen"))+
  labs(x = "Land cover", y = "Average biomass proportion contribution", 
       title = "Average biomass Contribution of native and exotic species by land cover",
       fill = "Species origins", 
       colour = "Species origins")
