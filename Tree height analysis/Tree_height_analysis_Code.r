#### Tree height analysis ####
# Tree height comparison  without grassland
# Load the data (if not done before)
Height_comparisons<-read.table("Heightcomparison.csv",h=T,sep=",")
# Remove the grasslands observations
Height_comparisons <- Height_comparisons[Height_comparisons$Landcover != "Grassland", ]
# Transform into factor
Height_comparisons$Landcover<- factor(Height_comparisons$Landcover , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Height_comparisons$Hill__side <-as.factor(Height_comparisons$Hill__side)
# Check
summary(Height_comparisons)
# plot to visualise
plot(Height_comparisons$Height..m.)
# check variance of the data
leveneTest(Height_comparisons$Height..m.,Height_comparisons$Landcover)
#not homogeneous
#data distribution
hist(Height_comparisons$Height..m.)
#slightly positive skewed
#check normality
qqnorm(Height_comparisons$Height..m.)
shapiro.test(Height_comparisons$Height..m.)
#normal not from test but yes the graph
#usage of lmm to account for the dependency, using the Hill side as a random factor
Heightmodel <- lmer(Height..m. ~ Landcover + (1|Hill__side), data = Height_comparisons)
#Run the model
summary(Heightmodel)
# check model's residuals
plot(Heightmodel)
residuals_model<-residuals(Heightmodel)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# In my case, the residuals do not show clear patterns
# Implement pairwise comparisons
emms<-emmeans(Heightmodel, pairwise ~ Landcover)
emms

# Tree height comparison implementing a spatial model
# Load the data (if not done before)
Height_comparisons<-read.table("Heightcomparison.csv",h=T,sep=",")
# Remove the grassland observations
Height_comparisons <- Height_comparisons[Height_comparisons$Landcover != "Grassland", ]
# Transform into factor
Height_comparisons$Landcover<- factor(Height_comparisons$Landcover , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Height_comparisons$Hill__side <-as.factor(Height_comparisons$Hill__side)
# Check
summary(Height_comparisons)
# Remove the NA for the gls implementation
Height_comparisons2 <- Height_comparisons[complete.cases(Height_comparisons), ]
# Exploration of the basic model
data.spatialCor.gls <- gls(Height..m.~ Landcover, data=Height_comparisons2,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(Height_comparisons2) <- ~Long_ID + Lat_ID
# Creation of the experimental variogram to understand the type of correlation
exp_variogram <- variogram(Height..m. ~ 1, data=Height_comparisons2)
plot(exp_variogram)
# Extract distance and gamma values
exp_variogram_data <- data.frame(distance = exp_variogram$dist, gamma = exp_variogram$gamma)
# Visualise
ggplot(exp_variogram_data, aes(x = distance, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(x = "Lag (h)", y = "Semivariance") +
  theme_minimal()
# No pattern can be extracted to implement a gls, non-stationary
