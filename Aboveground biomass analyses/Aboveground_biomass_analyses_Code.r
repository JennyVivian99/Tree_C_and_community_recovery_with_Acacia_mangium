#### Aboveground Biomass analysis without height ####
# Biomass analyses excluding the grassland
# Load the data (if not done before)
Biomass<-read.table("BiomassContributionClean.csv",h=T,sep=",")
# Remove the grassland
Biomass <- Biomass[Biomass$Landcovertype != "Grassland", ]
# Transform into factor
Biomass$Landcovertype<- factor(Biomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Biomass$Hill_Side<-as.factor(Biomass$Hill_Side)
# Check
summary(Biomass)
# Plot to visualise
plot(Biomass$Cumulative_AGB_Plot_Ha_NoHeight)
# Check the data variance
leveneTest(Biomass$Cumulative_AGB_Plot_Ha_NoHeight,Biomass$Landcovertype)
# In my case the data are homogeneous
# Check the data distribution
hist(Biomass$Cumulative_AGB_Plot_Ha_NoHeight)
# In my case they are positively skewed
# Transformation with sqrt (later the sqrt is good for the residuals. 
# Without transformation, they would not be normally distributed and scattered in the plot randomly).
hist(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoHeight))
# Check data normality
qqnorm(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoHeight))
shapiro.test(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoHeight))
# In my case they are normal from test and from the graph
# Check variance after transformation
leveneTest(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoHeight),Biomass$Landcovertype)
# Still homogeneous
# Zero are present for the grassland, and for one plot of the 2 years old plantation, but none of the landcover types has only zeros
# My data are paired in S and N, thus I will insert this as random effect. The elevation is highly correlated with the landcover, so that information
# is already present in the model.
# I fit the lmm(s) which considers REML
model_Biomass<-lmer(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoHeight)~Biomass$Landcover+Biomass$AP.mg_kg.+Biomass$pH_field +Biomass$C_N_Ratio+(1|Biomass$Hill_Side))
summary(model_Biomass)
# Drop the AP and CN ratio because not significant
model_Biomass2<-lmer(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoHeight)~Biomass$Landcover+Biomass$pH_field+ (1|Biomass$Hill_Side))
summary(model_Biomass2)
# models comparison
logLik(model_Biomass)
logLik(model_Biomass2)
AIC(model_Biomass,model_Biomass2)
anova(model_Biomass,model_Biomass2)
# From anova marginally significant is the difference among models
#check residuals
residuals_model2<-residuals(model_Biomass2)
plot(residuals_model2)
qqnorm(residuals_model2)
qqline(residuals_model2)
# In my case the residuals are ok, not clear patterns
# To implement pairwise comparisons
emms<-emmeans(model_Biomass2, pairwise ~ Landcover)
emms
# The significance do not change among 2-10-24-Remnant
# Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# Visualise the results
emms_backtransformed
# Including the p-value
pairs(emms_backtransformed)

# Biomass comparison with spatial model without grassland
# Load the data (if not done before)
Biomass <-read.table("BiomassContributionClean.csv",h=T,sep=",")
# Remove the grassland
Biomass <- Biomass[Biomass$Landcovertype != "Grassland", ]
# Transform into factor
Biomass$Landcovertype<- factor(Biomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Biomass$Hill_Side<-as.factor(Biomass$Hill_Side)
# Check
summary(Biomass)
# Exploration of the basic model
data.spatialCor.gls <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(Biomass) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ 1, Biomass)
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
# Implementation of the model considering the spatial correlation and the Gaussian structure
data.spatialCor.glsGaus <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
#Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsExp <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp,data.spatialCor.glsLin, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
      data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsLin)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)

# Models without AP
# Exploration of the basic model
data.spatialCor.gls <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+pH_field, data=Biomass,
                           method = "REML")
# Implementation of the model considering the spatial correlation and the Gaussian structure
data.spatialCor.glsGaus <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+pH_field, data=Biomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
#Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsExp <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+pH_field, data=Biomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
# Linear correlation not used because the model does not converge
# data.spatialCor.glsLin <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+pH_field+C_N_Ratio, data=Biomass,
# correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
# method = "REML")
data.spatialCor.glsRatio <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+pH_field, data=Biomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoHeight) ~ Landcovertype+pH_field, data=Biomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
      data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)
# Comparison of results between equally well-performing spatial models
emmsEXP<-emmeans(data.spatialCor.glsExp, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsGAUS<-emmeans(data.spatialCor.glsGaus, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsRATIO<-emmeans(data.spatialCor.glsRatio, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsSPHER<-emmeans(data.spatialCor.glsSpher, specs = pairwise ~ Landcovertype,mode = "df.error")
# See the results
emmsEXP
emmsGAUS
emmsRATIO
emmsSPHER
# See the results in the original scale
# Back-transform the EMMs
emms_backtransformedEXP <- regrid(emmsEXP, transform = T,predict.type)
emms_backtransformedGAUS <- regrid(emmsGAUS, transform = T,predict.type)
emms_backtransformedRATIO <- regrid(emmsRATIO, transform = T,predict.type)
emms_backtransformedSPHER <- regrid(emmsSPHER, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformedEXP
emms_backtransformedGAUS
emms_backtransformedRATIO
emms_backtransformedSPHER
# All models selected agree with their results. Inspection of models residual plots for selection of the simplest
plot(data.spatialCor.glsExp)
plot(data.spatialCor.glsGaus)
#check residuals
residuals_modelExp<-residuals(data.spatialCor.glsExp)
residuals_modelGaus<-residuals(data.spatialCor.glsGaus)
plot(residuals_modelExp)
qqnorm(residuals_modelExp)
qqline(residuals_modelExp)
plot(residuals_modelGaus)
qqnorm(residuals_modelGaus)
qqline(residuals_modelGaus)
# General evaluation
summary(data.spatialCor.glsExp)
summary(data.spatialCor.glsGaus)
# Both equal. Selection of the exponential model given the variogram graph displaying a similar pattern to the exponential one.
# Values for C stock can be converted by multiplying the Biomass per 0.47. P-values remains the same

#### Aboveground Biomass analysis with height ####
# Biomass analyses excluding the grassland
# Load the data (if not done before)
Biomass<-read.table("BiomassContributionClean.csv",h=T,sep=",")
# Remove the grassland
Biomass <- Biomass[Biomass$Landcovertype != "Grassland", ]
# Transform into factor
Biomass$Landcovertype<- factor(Biomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Biomass$Hill_Side<-as.factor(Biomass$Hill_Side)
# Check
summary(Biomass)
# Plot to visualise
plot(Biomass$Cumulative_AGB_Plot_Ha)
# Check the data variance
leveneTest(Biomass$Cumulative_AGB_Plot_Ha,Biomass$Landcovertype)
# In my case the data are homogeneous
# Check the data distribution
hist(Biomass$Cumulative_AGB_Plot_Ha)
# In my case they are positively skewed
# Transformation with sqrt (later the sqrt is good for the residuals. 
# Without transformation, they would not be normally distributed and scattered in the plot randomly).
hist(sqrt(Biomass$Cumulative_AGB_Plot_Ha))
# Check data normality
qqnorm(sqrt(Biomass$Cumulative_AGB_Plot_Ha))
shapiro.test(sqrt(Biomass$Cumulative_AGB_Plot_Ha))
# In my case they are normal from test and from the graph
# Check variance after transformation
leveneTest(sqrt(Biomass$Cumulative_AGB_Plot_Ha),Biomass$Landcovertype)
# Still homogeneous
# Zero are present for the grassland, and for one plot of the 2 years old plantation, but none of the landcover types has only zeros
# My data are paired in S and N, thus I will insert this as random effect. The elevation is highly correlated with the landcover, so that information
# is already present in the model.
# I fit the lmm(s) which considers REML
model_Biomass<-lmer(sqrt(Biomass$Cumulative_AGB_Plot_Ha)~Biomass$Landcover+Biomass$AP.mg_kg.+Biomass$pH_field +Biomass$C_N_Ratio+(1|Biomass$Hill_Side))
summary(model_Biomass)
# Drop the AP and CN ratio because not significant
model_Biomass2<-lmer(sqrt(Biomass$Cumulative_AGB_Plot_Ha)~Biomass$Landcover+Biomass$pH_field +(1|Biomass$Hill_Side))
summary(model_Biomass2)
# models comparison
logLik(model_Biomass)
logLik(model_Biomass2)
AIC(model_Biomass,model_Biomass2)
anova(model_Biomass,model_Biomass2)
# From anova marginally significant is the difference among models
#check residuals
residuals_model2<-residuals(model_Biomass2)
plot(residuals_model2)
qqnorm(residuals_model2)
qqline(residuals_model2)
# In my case the residuals are ok, not clear patterns
# To implement pairwise comparisons
emms<-emmeans(model_Biomass2, pairwise ~ Landcover)
emms
# The significance do not change among 2-10-24-Remnant
# Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# Visualise the results
emms_backtransformed
# Including the p-value
pairs(emms_backtransformed)

# Biomass comparison with spatial model without grassland
# Load the data (if not done before)
Biomass <-read.table("BiomassContributionClean.csv",h=T,sep=",")
# Remove the grassland
Biomass <- Biomass[Biomass$Landcovertype != "Grassland", ]
# Transform into factor
Biomass$Landcovertype<- factor(Biomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Biomass$Hill_Side<-as.factor(Biomass$Hill_Side)
# Check
summary(Biomass)
# Exploration of the basic model
data.spatialCor.gls <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(Biomass) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(sqrt(Cumulative_AGB_Plot_Ha) ~ 1, Biomass)
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
# Implementation of the model considering the spatial correlation and the Gaussian structure
data.spatialCor.glsGaus <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
#Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsExp <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp,data.spatialCor.glsLin, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
      data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsLin)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)

# Models without AP
# Exploration of the basic model
data.spatialCor.gls <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field, data=Biomass,
                           method = "REML")
# Implementation of the model considering the spatial correlation and the Gaussian structure
data.spatialCor.glsGaus <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field, data=Biomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
#Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsExp <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field, data=Biomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
# Linear correlation not used because the model does not converge
# data.spatialCor.glsLin <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field, data=Biomass,
# correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
# method = "REML")
data.spatialCor.glsRatio <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field, data=Biomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field, data=Biomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
      data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)
# Comparison of results between equally well-performing spatial models
emmsEXP<-emmeans(data.spatialCor.glsExp, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsGAUS<-emmeans(data.spatialCor.glsGaus, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsRATIO<-emmeans(data.spatialCor.glsRatio, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsSPHER<-emmeans(data.spatialCor.glsSpher, specs = pairwise ~ Landcovertype,mode = "df.error")
# See the results
emmsEXP
emmsGAUS
emmsRATIO
emmsSPHER
# See the results in the original scale
# Back-transform the EMMs
emms_backtransformedEXP <- regrid(emmsEXP, transform = T,predict.type)
emms_backtransformedGAUS <- regrid(emmsGAUS, transform = T,predict.type)
emms_backtransformedRATIO <- regrid(emmsRATIO, transform = T,predict.type)
emms_backtransformedSPHER <- regrid(emmsSPHER, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformedEXP
emms_backtransformedGAUS
emms_backtransformedRATIO
emms_backtransformedSPHER
# All models selected agree with their results. Inspection of models residual plots for selection of the simplest
plot(data.spatialCor.glsExp)
plot(data.spatialCor.glsGaus)
#check residuals
residuals_modelExp<-residuals(data.spatialCor.glsExp)
residuals_modelGaus<-residuals(data.spatialCor.glsGaus)
plot(residuals_modelExp)
qqnorm(residuals_modelExp)
qqline(residuals_modelExp)
plot(residuals_modelGaus)
qqnorm(residuals_modelGaus)
qqline(residuals_modelGaus)
# General evaluation
summary(data.spatialCor.glsExp)
summary(data.spatialCor.glsGaus)
# Both equal. Selection of the exponential model given the variogram graph displaying a similar pattern to the exponential one.
# The lmm model have a slightly higher AIC, and 1 less df in comparison to multiple well-performing spatial models
# Values for C stock can be converted by multiplying the Biomass per 0.47. P-values remains the same

#### Aboveground Biomass excluding dead trees with height ####
# Biomass analyses excluding the grassland
# Load the data (if not done before)
Biomass<-read.table("BiomassContributionNoDeadTrees.csv",h=T,sep=",")
# Remove the grassland
Biomass <- Biomass[Biomass$Landcovertype != "Grassland", ]
# Transform into factor
Biomass$Landcovertype<- factor(Biomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Biomass$Hill_Side<-as.factor(Biomass$Hill_Side)
# Check
summary(Biomass)
# Plot to visualise
plot(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees)
# Check the data variance
leveneTest(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees,Biomass$Landcovertype)
# In my case the data are homogeneous
# Check the data distribution
hist(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees)
# In my case they are positively skewed
# Transformation with sqrt (later the sqrt is good for the residuals. 
# Without transformation, they would not be normally distributed and scattered in the plot randomly).
hist(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees))
# Check data normality
qqnorm(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees))
shapiro.test(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees))
# In my case they are normal from test and from the graph
# Check variance after transformation
leveneTest(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees),Biomass$Landcovertype)
# Still homogeneous
# Zero are present for the grassland, and for one plot of the 2 years old plantation, but none of the landcover types has only zeros
# My data are paired in S and N, thus I will insert this as random effect. The elevation is highly correlated with the landcover, so that information
# is already present in the model.
# I fit the lmm(s) which considers REML
model_Biomass<-lmer(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees)~Biomass$Landcover+Biomass$AP.mg_kg.+Biomass$pH_field +Biomass$C_N_Ratio+(1|Biomass$Hill_Side))
summary(model_Biomass)
# Drop the AP and CN ratio because not significant
model_Biomass2<-lmer(sqrt(Biomass$Cumulative_AGB_Plot_Ha_NoDeadTrees)~Biomass$Landcover+Biomass$pH_field +Biomass$C_N_Ratio+(1|Biomass$Hill_Side))
summary(model_Biomass2)
# models comparison
logLik(model_Biomass)
logLik(model_Biomass2)
AIC(model_Biomass,model_Biomass2)
anova(model_Biomass,model_Biomass2)
# From anova marginally significant is the difference among models
#check residuals
residuals_model2<-residuals(model_Biomass2)
plot(residuals_model2)
qqnorm(residuals_model2)
qqline(residuals_model2)
# In my case the residuals are ok, not clear patterns
# To implement pairwise comparisons
emms<-emmeans(model_Biomass2, pairwise ~ Landcover)
emms
# The significance do not change among 2-10-24-Remnant
# Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# Visualise the results
emms_backtransformed
# Including the p-value
pairs(emms_backtransformed)

# Biomass comparison with spatial model without grassland
# Load the data (if not done before)
Biomass <-read.table("BiomassContributionNoDeadTrees.csv",h=T,sep=",")
# Remove the grassland
Biomass <- Biomass[Biomass$Landcovertype != "Grassland", ]
# Transform into factor
Biomass$Landcovertype<- factor(Biomass$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
Biomass$Hill_Side<-as.factor(Biomass$Hill_Side)
# Check
summary(Biomass)
# Exploration of the basic model
data.spatialCor.gls <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(Biomass) <- ~Long_ID + Lat_ID
# Implementation of variogram to understand the type of correlation
exp_variogram <- variogram(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ 1, Biomass)
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
# Implementation of the model considering the spatial correlation and the Gaussian structure
data.spatialCor.glsGaus <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
#Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsExp <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsLin <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                              correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+AP.mg_kg.+pH_field+C_N_Ratio, data=Biomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls, data.spatialCor.glsExp,data.spatialCor.glsLin, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
      data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsLin)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)

# Models without AP
# Exploration of the basic model
data.spatialCor.gls <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+pH_field, data=Biomass,
                           method = "REML")
# Implementation of the model considering the spatial correlation and the Gaussian structure
data.spatialCor.glsGaus <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+pH_field, data=Biomass,
                               correlation = corGaus(form = ~Long_ID + Lat_ID, nugget = TRUE),
                               method = "REML")
#Other models with different correlation structure to verify the selection of the one with the lowest AIC
data.spatialCor.glsExp <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+pH_field, data=Biomass,
                              correlation = corExp(form = ~Long_ID + Lat_ID, nugget = TRUE),
                              method = "REML")
# Linear correlation not used because the model does not converge
# data.spatialCor.glsLin <- gls(sqrt(Cumulative_AGB_Plot_Ha) ~ Landcovertype+pH_field+C_N_Ratio, data=Biomass,
# correlation = corLin(form = ~Long_ID + Lat_ID, nugget = TRUE),
# method = "REML")
data.spatialCor.glsRatio <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+pH_field, data=Biomass,
                                correlation = corRatio(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(sqrt(Cumulative_AGB_Plot_Ha_NoDeadTrees) ~ Landcovertype+pH_field, data=Biomass,
                                correlation = corSpher(form = ~Long_ID + Lat_ID, nugget = TRUE),
                                method = "REML")
# Evaluation through AIC
AIC(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsRatio,data.spatialCor.glsSpher)
anova(data.spatialCor.gls,data.spatialCor.glsExp, data.spatialCor.glsGaus,
      data.spatialCor.glsRatio,data.spatialCor.glsSpher)
# Evaluate loglikelihood
logLik(data.spatialCor.gls)
logLik(data.spatialCor.glsExp)
logLik(data.spatialCor.glsGaus)
logLik(data.spatialCor.glsRatio)
logLik(data.spatialCor.glsSpher)
# Comparison of results between equally well-performing spatial models
emmsEXP<-emmeans(data.spatialCor.glsExp, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsGAUS<-emmeans(data.spatialCor.glsGaus, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsRATIO<-emmeans(data.spatialCor.glsRatio, specs = pairwise ~ Landcovertype,mode = "df.error")
emmsSPHER<-emmeans(data.spatialCor.glsSpher, specs = pairwise ~ Landcovertype,mode = "df.error")
# See the results
emmsEXP
emmsGAUS
emmsRATIO
emmsSPHER
# See the results in the original scale
# Back-transform the EMMs
emms_backtransformedEXP <- regrid(emmsEXP, transform = T,predict.type)
emms_backtransformedGAUS <- regrid(emmsGAUS, transform = T,predict.type)
emms_backtransformedRATIO <- regrid(emmsRATIO, transform = T,predict.type)
emms_backtransformedSPHER <- regrid(emmsSPHER, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformedEXP
emms_backtransformedGAUS
emms_backtransformedRATIO
emms_backtransformedSPHER
# All models selected agree with their results. Inspection of models residual plots for selection of the simplest
plot(data.spatialCor.glsExp)
plot(data.spatialCor.glsGaus)
#check residuals
residuals_modelExp<-residuals(data.spatialCor.glsExp)
residuals_modelGaus<-residuals(data.spatialCor.glsGaus)
plot(residuals_modelExp)
qqnorm(residuals_modelExp)
qqline(residuals_modelExp)
plot(residuals_modelGaus)
qqnorm(residuals_modelGaus)
qqline(residuals_modelGaus)
# General evaluation
summary(data.spatialCor.glsExp)
summary(data.spatialCor.glsGaus)
# Both equal. Selection of the exponential model given the variogram graph displaying a similar pattern to the exponential one.
# Values for C stock can be converted by multiplying the Biomass per 0.47. P-values remains the same

#### Aboveground Carbon Plots ####
# Load the dataset without converting it into spatial one
# Load the data (if not done before)
AGCarbonStock <-read.table("CarbonDistribution.csv",h=T,sep=",")
# Remove the grassland
AGCarbonStock <- AGCarbonStock[AGCarbonStock$Landcovertype != "Grassland", ]
# Transform into factor
AGCarbonStock$Landcovertype<- factor(AGCarbonStock$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
AGCarbonStock$Hill_Side<-as.factor(AGCarbonStock$Hill_Side)
# Retrieve Tukey HSD results to see the significance and write the letters for the significance above the box plots
emmsEXP
# Write a vector with them
Letters<- c("A","B","B","B")
# Create a dataframe with them
BoxplotLetters <- data.frame(Landcovertype = unique(AGCarbonStock$Landcovertype), Letters = Letters)
# Check
BoxplotLetters
# Merge the data frames
AGCarbonStock <- merge(AGCarbonStock, BoxplotLetters, by = "Landcovertype")
# Calculate label y-positions above the boxplots
# Find the upper whisker for each boxplot
upper_whiskers <- aggregate(Cumulative_AGCarbon_Plot_Ha ~ Landcovertype, data = AGCarbonStock, FUN = function(x) boxplot.stats(x)$stats[5])
AGCarbonStock <- merge(AGCarbonStock, upper_whiskers, by = "Landcovertype", suffixes = c("", "_upper"))
#Calculate the position for the letters
AGCarbonStock$label_y <- AGCarbonStock$Cumulative_AGCarbon_Plot_Ha_upper + 20
# Plot default colors of ggplot to match with SOC plots
show_col(hue_pal()(5))
colori<-c("#A3A500","#00BF7D","#00B0F6","#E76BF3")
names(colori)<-levels(AGCarbonStock$Landcovertype)
ggplot(AGCarbonStock, aes(x = Landcovertype, y = Cumulative_AGCarbon_Plot_Ha)) +
  geom_boxplot(aes(fill = Landcovertype)) +
  geom_point(aes(fill = Landcovertype), cex = 2, alpha = 0.5, colour = "black", pch = 21, stroke = 1) +
  geom_text(aes(y = label_y, label = Letters), size = 5) +  # Use merged data, correct y, and Letters column
  labs(x = "Landcover", y = "Aboveground carbon t/ha") +
  theme_classic()+
  theme(axis.title.y = element_text(size=16))

# Plots for C comparisons
# Load the data (if not done before)
AGC_compare <-read.table("C_comparisons.csv",h=T,sep=",")
# Remove the grassland
#AGC_compare <- AGC_compare[AGC_compare$Landcovertype != "Grassland", ]
# Transform into factor
#AGC_compare$Landcovertype<- factor(AGC_compare$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant", "15_yo_China","Unthinned_mangium_Thailand",
#                                                                          "mangium_invaded_Matos_estimated","average_global_tropical_estimated"))
AGC_compare
# Create a new 'PlotCategory' column to define how groups are plotted
AGC_compare <- AGC_compare %>%
  mutate(PlotCategory = case_when(
    # Group these into "Developmental Stages" for line/shadow
    Landcover %in% c("Grassland", "2 years old", "10 years old", "24 years old") ~ "Developmental Stages",
    # All other categories are treated as separate points with SD error bars
    TRUE ~ Landcover
  )) %>%
  # Create a transformed time variable for plotting to shorten the gap
  # For times <= 24, use original time. For time = 60, map it to a new value
  # (e.g., 24 + (60 - 24) * factor). A factor of 0.1 makes 60 map to 27.6.
  mutate(Time_Plot = if_else(Time > 24, 24 + (Time - 24) * 0.1, as.numeric(Time)))
# Calculate summary statistics (mean, SD, and count) for each PlotCategory at each Time_Plot point
summary_data <- AGC_compare %>%
  group_by(PlotCategory, Time_Plot) %>% # Group by Time_Plot now
  summarise(
    mean_biomass = mean(Biomass, na.rm = TRUE),
    sd_biomass = sd(Biomass, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  )
# Separate data for different plotting styles
# Categories for a continuous line with ribbon (only "Developmental Stages")
categories_for_continuous_line <- c("Developmental Stages")
plot_data_continuous_line <- summary_data %>%
  filter(PlotCategory %in% categories_for_continuous_line)
# All other categories will now be points with SD error bars
plot_data_points_with_sd <- summary_data %>%
  filter(!PlotCategory %in% categories_for_continuous_line)
# plot_data_hlines will now be empty as all categories are handled above
plot_data_hlines <- data.frame(PlotCategory = character(), single_biomass_value = numeric())
# Plotting with ggplot2
p <- ggplot() +
  # Add ribbons for standard deviation (shadows) for continuous lines
  geom_ribbon(data = plot_data_continuous_line,
              aes(x = Time_Plot, ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass, # Use Time_Plot
                  fill = PlotCategory),
              alpha = 0.2) +
  # Add continuous lines for mean biomass
  geom_line(data = plot_data_continuous_line,
            aes(x = Time_Plot, y = mean_biomass, color = PlotCategory), # Use Time_Plot
            size = 1) +
  # Add points with SD error bars for all other categories
  geom_point(data = plot_data_points_with_sd,
             aes(x = Time_Plot, y = mean_biomass, color = PlotCategory), # Use Time_Plot
             size = 3, shape = 16) + # Larger point for clarity
  geom_errorbar(data = plot_data_points_with_sd,
                aes(x = Time_Plot, ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass, # Use Time_Plot
                    color = PlotCategory),
                width = 0.5, size = 0.8) + # Width of the error bar caps, thickness of the line
  # Customizing the plot aesthetics
  labs(
    title = "Biomass Over Time by Landcover Category",
    x = "Time (Years)",
    y = "Biomass (Cumulative)",
    color = "Category", # Unified legend title for color
    fill = "Category"   # Unified legend title for fill
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right"
  ) +
  expand_limits(y = 0) +
  # Set x-axis breaks to show all relevant unique time points from your data
  # Use the transformed Time_Plot for breaks, and original Time for labels
  scale_x_continuous(
    breaks = unique(AGC_compare$Time_Plot),
    labels = unique(AGC_compare$Time)
  )
# Print the plot
p
