#### Small stems analysis ####
# Small stems analysis without grassland
# Load the data
Small_stems<-read.table("Small_stems.csv",h=T,sep=",")
# Remove grassland
Small_stems <- Small_stems[Small_stems$Landcover != "Grassland", ]
# Transform into factor
Small_stems$Landcover<-factor(Small_stems$Landcover,levels=c("TwoYearsOld","TenYearsOld","TwentyFourYearsOld","Remnant"))
Small_stems$Hill_side<-as.factor(Small_stems$Hill_side)
# Check
summary(Small_stems)
# Visualise, plot stems
plot(Small_stems$Stems.per.ha)
plot(Small_stems$Stems.per.ha~Small_stems$Landcover)
# Check variance
leveneTest(Small_stems$Stems.per.ha,Small_stems$Landcover)
# In my case is homogeneous
# Check the data distribution
hist(Small_stems$Stems.per.ha)
# In my case is positively skewed
# Transformation with sqrt (later the sqrt is good for the residuals. Without transformation, they would not be normally distributed and scattered in the plot randomly).
hist(sqrt(Small_stems$Stems.per.ha))
# Check data normality. Shapiro is better in this case because Kolmogorov is for sample size higher than 80. Note that the residual should be normal. To check after the model is set.
plot(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot)
qqnorm(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot)
ks.test(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot, "pnorm")
shapiro.test(Small_stems$Number.of.stems.below.5cm.DBH..above.2m.height.in.a.quarter.of.the.plot)
# Data are normal from test and from the graph
# Check the variance after transformation
leveneTest(sqrt(Small_stems$Stems.per.ha),Small_stems$Landcover)
# Still homogeneous
# Zero are present for the grassland, and for one plot of the 2 years old plantation, but none of the landcover types has only zeros.
# My data are paired in S and N, thus I will insert this as random effect. The elevation is highly correlated with the landcover, so that information
# is already present in the model.
# I fit a lmm which considers REML
model_SmallStems<-lmer(sqrt(Small_stems$Stems.per.ha)~Small_stems$Landcover+(1|Small_stems$Hill_side))
residuals_model<-residuals(model_SmallStems)
summary(model_SmallStems)
# Check residuals
plot(model_SmallStems)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# ok residuals, not clear patterns, normally distributed
# For pairwise comparisons
emms<-emmeans(model_SmallStems, pairwise ~ Landcover)
#The significance do not change among 2-10-24-Remnant
#Back-transform
emms_backtransformed <- regrid(emms, transform = T,predict.type)
#visualise the results
emms_backtransformed
#including the p-value
pairs(emms_backtransformed)

# Non-parametric tests for comparison, which considers 1 numeric (number of stems) and one categorical (landcover type). 
# Thus I use the Chi-square, not assuming ordination between landcover types.
# I did not assume ordination because of variability in regrowth, making the abundance like an upside-down bell shape.
# Plot to see disposition
colori<-c("orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(Small_stems$Landcover)
plot(Small_stems$Stems.per.ha ~Small_stems$Landcover,col=colori)
#Kruskal
kruskal.test(Small_stems$Landcover,Small_stems$Stems.per.ha,correct="HB")
# Not statistically significant the difference
