#### Basal Area analyses ####
# Cumulative trees basal area analysis, without considering grassland
# Cumulative tree basal area calculation with Musa sp., considering values for each plot scaled to the hectar
# Load the data
BasalArea<-read.table("BasalAreaPerHaPerPlot.csv",h=T,sep=",")
# Remove the grasslands observations
BasalArea <- BasalArea[BasalArea$Landcover != "Grassland", ]
# Convert into factor
BasalArea$Landcover<- factor(BasalArea$Landcover , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
BasalArea$Hill__side<-as.factor(BasalArea$Hill__side)
# Check
summary(BasalArea)
# Plot to visualise
plot(BasalArea$Cumulative_Basal_Area_Hectar)
plot(BasalArea$Cumulative_Basal_Area_Hectar~BasalArea$Landcover)
# Check the data variance
leveneTest(BasalArea$Cumulative_Basal_Area_Hectar,BasalArea$Landcover)
# In my case is homogeneous
# Check the data distribution
hist(BasalArea$Cumulative_Basal_Area_Hectar)
# In my case is positively skewed
# Check also the relationship
plot(BasalArea$Cumulative_Basal_Area_Hectar~BasalArea$Landcover)
# I do the transformation with log1p, so it can handle the zeros and the residuals are normal (see after model implementation).
# Zero are present for the grassland, and for one plot of the 2 years old plantation, but none of the landcover types has only zeros
# My data are paired in S and N, thus I will insert this as random effect. The elevation is highly correlated with the landcover, so that information
# is already present in the model.
# Therefore, I fit a lmm which considers REML
model_BasalArea <- lmer(sqrt(Cumulative_Basal_Area_Hectar) ~ Landcover + (1|Hill__side), data = BasalArea)
# Run the model
model_BasalArea
summary(model_BasalArea)
# AIC and BIC not displayed because useless if not other models are compared. 
# AIC() can be used later when I compare between models.
# Check the residuals
residuals_modelBA<-residuals(model_BasalArea)
plot(model_BasalArea)
plot(residuals_modelBA)
qqnorm(residuals_modelBA)
qqline(residuals_modelBA)
# In my case the residuals are ok residuals, not clear patterns
# To implement pairwise comparisons
emmeans(model_BasalArea , pairwise ~ Landcover)
# See the results in the original scale
# Back-transform the EMMs
emms<-emmeans(model_BasalArea , pairwise ~ Landcover)
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed
#use pairs() to display also the p-values
pairs(emms_backtransformed)

# Basal Area analysis with non parametric test: Kruskal Wallis
# Load the data
Basal_Area2<-read.table("CumulativeBAHa.csv",h=T,sep=",")
# Transform as factor
Basal_Area2$Landcover<-factor(Basal_Area2$Landcover , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Check
Basal_Area2
# Run the kruskal wallis test
kruskal.test(Basal_Area2$Cumulative_Basal_Area_Hectar~Basal_Area2$Landcover)
# Since it is significant, I run the pairwise comparison
pairwise.wilcox.test(Basal_Area2$Cumulative_Basal_Area_Hectar,Basal_Area2$Landcover,p.adjust.method = "BH")
# In my case, the results are equal to the parametric test done with lmer()
