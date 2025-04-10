#### Soil N ####
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Check
Soil_data
# Transform into factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
Soil_data$Hill_side<-as.factor(Soil_data$Hill_side)
# Check
summary(Soil_data)
# Plot to visualise
plot(Soil_data$TN...)
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Soil_data$Landcover)
plot(Soil_data$TN...~Soil_data$Landcover,col=colori)
# Check the data variance
leveneTest(Soil_data$TN...,Soil_data$Landcover)
# In my case the data are homogeneous
# Check the data distribution
hist(Soil_data$TN...)
# Pretty normal
# Construction of the model
TN_model<-lmer(Soil_data$TN...~Soil_data$Landcover+(1|Soil_data$Hill_side))
# Plots of models' residuals
plot(TN_model)
# Summary
summary(TN_model)
# Post hoc comparisons
emms<-emmeans(TN_model, pairwise ~ Landcover)
# See the results
emms
# TN stock is highly correlated with landcover type, since they change according to the classical clusters of the early stages and later ones
#check residuals
TNresiduals_model<-residuals(TN_model)
plot(TNresiduals_model)
qqnorm(TNresiduals_model)
qqline(TNresiduals_model)
# In my case the residuals are ok, not clear patterns
# Additional evaluation
kruskal.test(Biomass$TN...~Biomass$Landcovertype)

#### CN Ratio ####
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Check
Soil_data
# Transform into factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
Soil_data$Hill_side<-as.factor(Soil_data$Hill_side)
# Check
summary(Soil_data)
# Plot to visualise
plot(Soil_data$C_N_Ratio)
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Soil_data$Landcover)
plot(Soil_data$TN...~Soil_data$Landcover,col=colori)
# Check the data variance
leveneTest(Soil_data$C_N_Ratio,Soil_data$Landcover)
# In my case the data are homogeneous
# Check the data distribution
hist(Soil_data$C_N_Ratio)
# Pretty normal
# Construction of the model
CN_model<-lmer(Soil_data$C_N_Ratio~Soil_data$Landcover+(1|Soil_data$Hill_side))
# Plots of models' residuals
plot(CN_model)
# Summary
summary(CN_model)
# Post hoc comparisons
emms<-emmeans(CN_model, pairwise ~ Landcover)
# See the results
emms
#check residuals
CNresiduals_model<-residuals(CN_model)
plot(CNresiduals_model)
qqnorm(CNresiduals_model)
qqline(CNresiduals_model)
# In my case the residuals are ok, not clear patterns

#### Soil P ####
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Check
Soil_data
# Transform into factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
Soil_data$Hill_side<-as.factor(Soil_data$Hill_side)
# Check
summary(Soil_data)
# Plot to visualise
plot(Soil_data$AP.mg_kg.)
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Soil_data$Landcover)
plot(Soil_data$AP.mg_kg.~Soil_data$Landcover,col=colori)
# Check the data variance
leveneTest(Soil_data$AP.mg_kg.,Soil_data$Landcover)
# In my case the data are homogeneous
# Check the data distribution
hist(Soil_data$AP.mg_kg.)
# A little positively skewed, thus:
hist(log(Soil_data$AP.mg_kg.))
# Construction of the model
AP_model<-lmer(log(Soil_data$AP.mg_kg.)~Soil_data$Landcover+(1|Soil_data$Hill_side))
AP_model
# Plots of models' residuals
plot(AP_model)
# Summary
summary(AP_model)
# Post hoc comparisons
emms<-emmeans(AP_model, pairwise ~ Landcover)
# See the results
emms
# Back-transform the results
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed
#check residuals
APresiduals_model<-residuals(AP_model)
plot(APresiduals_model)
qqnorm(APresiduals_model)
qqline(APresiduals_model)
# Residuals are ok, not clear patterns

#### Soil Organic Carbon Stock analyses ####
# Load the data
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Check
Soil_data
# Remove outlier
Soil_data <- Soil_data[Soil_data$Sample_ID != "R3SC", ]
# Transform into factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
Soil_data$Hill_side<-as.factor(Soil_data$Hill_side)
# Check
summary(Soil_data)
# Plot to visualise
plot(Soil_data$SOCStock.tC_ha.)
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Soil_data$Landcover)
plot(Soil_data$SOCStock.tC_ha.~Soil_data$Landcover,col=colori)
# Check the data variance
leveneTest(Soil_data$SOCStock.tC_ha.,Soil_data$Landcover)
# In my case the data are homogeneous
# Check the data distribution
hist(Soil_data$SOCStock.tC_ha.)
# In my case data are just slighty positively skewed
# I fit a lmm which considers REML and dependence with the random factor
# If I use the lab pH the results are weird, if I use the same models but the field pH the results respects the plot of the SOC.
# To explore other models combination
# Model
SOC_model<-lmer(Soil_data$SOCStock.tC_ha.~Soil_data$Landcover+Soil_data$AP.mg_kg.+Soil_data$pH_field+Soil_data$C_N_Ratio+(1|Soil_data$Hill_side))
# Plots of models' residuals
plot(SOC_model)
# Summary
summary(SOC_model)
# C:N is not significant, thus I drop this parameter
SOC_model2<-lmer(Soil_data$SOCStock.tC_ha.~Soil_data$Landcover+Soil_data$AP.mg_kg.+Soil_data$pH_field+(1|Soil_data$Hill_side))
# Plots of models' residuals
plot(SOC_model2)
# Summary
summary(SOC_model2)
# Post hoc comparisons
emmeans(SOC_model2, pairwise ~ Landcover)
# Comparison between models
AIC(SOC_model,SOC_model2)
anova(SOC_model,SOC_model2)
logLik(SOC_model)
logLik(SOC_model2)
#check residuals of model2, which is the best between the 2
SOCresiduals_model<-residuals(SOC_model2)
plot(SOCresiduals_model)
qqnorm(SOCresiduals_model)
qqline(SOCresiduals_model)
# In my case the residuals are ok, not clear patterns
# To implement pairwise comparisons
emms<-emmeans(SOC_model2, pairwise ~ Landcover)
# See the results
emms

# Soil Organic Carbon analysis with spatial models
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Check
Soil_data
# Remove the outlier R3SC
Soil_data <- Soil_data[Soil_data$Sample_ID != "R3SC", ]
# Transform into factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
Soil_data$Hill_side<-as.factor(Soil_data$Hill_side)
# Check
summary(Soil_data)
# Exploration of the basic model
data.spatialCor.gls <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field+C_N_Ratio, data=Soil_data,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(Soil_data) <- ~Long_ID + Lat_ID
# Check
Soil_data
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(Soil_data$SOCStock.tC_ha. ~ 1, Soil_data)
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
data.spatialCor.glsGaus <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field+C_N_Ratio,  data = Soil_data,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsExp <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field+C_N_Ratio, data=Soil_data,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field+C_N_Ratio, data=Soil_data,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field+C_N_Ratio, data=Soil_data,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field+C_N_Ratio, data=Soil_data,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsLin, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls, data.spatialCor.glsExp)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsLin)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)

# Models without C:N, since not significant
# Exploration of the basic model assuming no correlation
data.spatialCor.gls <- gls(SOCStock.tC_ha. ~ Landcover+AP.mg_kg.+pH_field, data=Soil_data,
                           method = "REML")
# Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsGaus <- gls(SOCStock.tC_ha. ~  Landcover+AP.mg_kg.+pH_field,  data = Soil_data,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsExp <- gls(SOCStock.tC_ha. ~  Landcover+AP.mg_kg.+pH_field, data=Soil_data,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(SOCStock.tC_ha. ~  Landcover+AP.mg_kg.+pH_field, data=Soil_data,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(SOCStock.tC_ha. ~  Landcover+AP.mg_kg.+pH_field, data=Soil_data,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(SOCStock.tC_ha. ~  Landcover+AP.mg_kg.+pH_field, data=Soil_data,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsLin, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsLin)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)
# The best fit is from the model that consider a spherical structural correlation. 
# Run the model
data.spatialCor.glsSpher
# See better the results
summary(data.spatialCor.glsSpher)
#check residuals
residuals_model<-residuals(data.spatialCor.glsSpher)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# Normal, no patterns visible
# Post hoc comparisons (note that the comparison consider average pH which is 5.75)
emmsSpherSOC<-emmeans(data.spatialCor.glsSpher, pairwise ~ Landcover, mode="df.error")
# See the results
emmsSpherSOC

#### SOC Plots ####
# Load the dataset without converting it into spatial one
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Order the factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Retrieve Tukey HSD results to see the significance and write the letters for the significance above the box plots
emmsSpherSOC
# Write a vector with them
Letters<- c("A","A","AB","B","AB")
# Create a dataframe with them
BoxplotLetters <- data.frame(Landcover = unique(Soil_data$Landcover), Letters = Letters)
# Check
BoxplotLetters
# Merge the data frames
Soil_data <- merge(Soil_data, BoxplotLetters, by = "Landcover")
# Calculate label y-positions above the boxplots
# Find the upper whisker for each boxplot
upper_whiskers <- aggregate(SOCStock.tC_ha. ~ Landcover, data = Soil_data, FUN = function(x) boxplot.stats(x)$stats[5])
Soil_data <- merge(Soil_data, upper_whiskers, by = "Landcover", suffixes = c("", "_upper"))
#Calculate the position for the letters
Soil_data$label_y <- Soil_data$SOCStock.tC_ha._upper + 5
# Plot with default colors reported explicitly to be sure to match the colors in Aboveground plot (not reporting them brings the same result)
colori<-c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
names(colori)<-levels(Soil_data$Landcover)
ggplot(Soil_data, aes(x = Landcover, y = SOCStock.tC_ha.)) +
  geom_boxplot(aes(fill = Landcover)) +
  geom_point(aes(fill = Landcover), cex = 2, alpha = 0.5, colour = "black", pch = 21, stroke = 1) +
  geom_text(aes(y = label_y, label = Letters), size = 5) +  # Use merged data, correct y, and Letters column
  labs(x = "Landcover", y = "SOC t/ha") +
  theme_classic()+
  theme(axis.title.y = element_text(size=16))

# Plot to visualise without the outlier (if not already removed before)
# Load the dataset without converting it into spatial one
Soil_data<-read.table("Soil_data.csv",h=T, sep=",")
# Remove the outlier R3SC
Soil_data <- Soil_data[Soil_data$Sample_ID != "R3SC", ]
# Order the factor
Soil_data$Landcover<- factor(Soil_data$Landcover, levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Retrieve Tukey HSD results to see the significance and write the letters for the significance above the box plots
emmsSpherSOC
# Write a vector with them
Letters<- c("A","A","AB","B","AB")
# Create a dataframe with them
BoxplotLetters <- data.frame(Landcover = unique(Soil_data$Landcover), Letters = Letters)
# Check
BoxplotLetters
# Merge the data frames
Soil_data <- merge(Soil_data, BoxplotLetters, by = "Landcover")
# Calculate label y-positions above the boxplots
# Find the upper whisker for each boxplot
upper_whiskers <- aggregate(SOCStock.tC_ha. ~ Landcover, data = Soil_data, FUN = function(x) boxplot.stats(x)$stats[5])
Soil_data <- merge(Soil_data, upper_whiskers, by = "Landcover", suffixes = c("", "_upper"))
#Calculate the position for the letters
Soil_data$label_y <- Soil_data$SOCStock.tC_ha._upper + 5
# Plot with default colors reported explicitly to be sure to match the colors in Aboveground plot
colori<-c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
names(colori)<-levels(Soil_data$Landcover)
ggplot(Soil_data, aes(x = Landcover, y = SOCStock.tC_ha.)) +
  geom_boxplot(aes(fill = Landcover)) +
  geom_point(aes(fill = Landcover), cex = 2, alpha = 0.5, colour = "black", pch = 21, stroke = 1) +
  geom_text(aes(y = label_y, label = Letters), size = 5) +  # Use merged data, correct y, and Letters column
  labs(x = "Landcover", y = "SOC t/ha") +
  theme_classic()+
  scale_fill_manual(values = colori) +
  theme(axis.title.y = element_text(size=16))
