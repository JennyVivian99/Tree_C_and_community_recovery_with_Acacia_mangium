#### DBH analysis ####
# Note that DBH analyses consider Musa spp. (thus inclueded also in the Basal Area evaluations), but biomass evaluations do not.
# Trees DBH analysis without grassland
# Load the data (if not done before)
DBH_comparisons<-read.table("Heightcomparison.csv",h=T,sep=",")
# Remove the grasslands observations
DBH_comparisons <- DBH_comparisons[DBH_comparisons$Landcover != "Grassland", ]
# Transform into factors
DBH_comparisons$Landcover<- factor(DBH_comparisons$Landcover , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
DBH_comparisons$Hill__side <-as.factor(DBH_comparisons$Hill__side)
# Check
summary(DBH_comparisons)
# plot to visualise
plot(DBH_comparisons$DBH..cm.~DBH_comparisons$Landcover)
ggplot(DBH_comparisons, aes(x=Landcover, y=DBH..cm.))+
  geom_boxplot()+
  geom_jitter(width=0.05, cex=3, alpha=0.05)+
  theme_classic()
# check the variance of the data
leveneTest(DBH_comparisons$DBH..cm.,DBH_comparisons$Landcover)
# In my case is not homogeneous
# Visualise the data distribution
hist(DBH_comparisons$DBH..cm.)
# In my case is positively skewed
# Transform the data
hist(sqrt(DBH_comparisons$DBH..cm.))
# Check normality of the data
qqnorm(sqrt(DBH_comparisons$DBH..cm.))
shapiro.test(sqrt(DBH_comparisons$DBH..cm.))
# In my case they are normal not from test but from the graph
# Check variance after transformation
leveneTest((sqrt(DBH_comparisons$DBH..cm.)),DBH_comparisons$Landcover)
# The variance is still not homogeneous
# Usage of lmm to account for the dependency, using the Hill side as a random factor
DBHmodel <- lmer((sqrt(DBH..cm.)) ~ Landcover + (1|Hill__side), data = DBH_comparisons)
# Run the model
summary(DBHmodel)
# check model's residuals
plot(DBHmodel)
residuals_model<-residuals(DBHmodel)
plot(residuals_model)
qqnorm(residuals_model)
qqline(residuals_model)
# To implement pairwise comparisons
emms<-emmeans(DBHmodel, pairwise ~ Landcover)
emms
# See the results in the original scale
# Back-transform the EMMs
emms_backtransformed <- regrid(emms, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed
#use pairs() to display also the p-values
pairs(emms_backtransformed)

#Spatial model DBH, without grassland
# Load the data (if not done before)
DBH_comparisons<-read.table("Heightcomparison.csv",h=T,sep=",")
# Remove the grasslands observations
DBH_comparisons <- DBH_comparisons[DBH_comparisons$Landcover != "Grassland", ]
# Tranform into factors
DBH_comparisons$Landcover<- factor(DBH_comparisons$Landcover , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
DBH_comparisons$Hill__side <-as.factor(DBH_comparisons$Hill__side)
# Check
summary(DBH_comparisons)
# Exploration of the basic model
data.spatialCor.gls <- gls(DBH..cm.~ Landcover, data=DBH_comparisons,
                           method = "REML")
plot(data.spatialCor.gls)
# Convert data to a spatial points data frame (SPDF). This is also the format required to implement the models
coordinates(DBH_comparisons) <- ~Long_ID + Lat_ID
# Creation of the experimental variogram to understand the type of correlation
exp_variogram <- variogram(DBH..cm. ~ 1, data=DBH_comparisons)
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

#### DBH proportions####
# DBH proportions without grassland (only present 1 Coccus nucifera in 1 plot in grassland)
# Load the data
Raw_data_DBH<-read.table("GoodPaired_analyses_DBH_BiomassNoCoconut2.csv",h=T,sep=",")
# Remove the grassland
Raw_data_DBH<-Raw_data_DBH[Raw_data_DBH$Sample__ID != "G3SB", ]
# Ensure that the first factor displayed is 2 years old, not 10 years old plantation:
Raw_data_DBH$Landcover <- factor(Raw_data_DBH$Landcover, levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
levels(Raw_data_DBH$Landcover)
Raw_data_DBH$Hill__side<-as.factor(Raw_data_DBH$Hill__side)
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
# Check
filtered_data
# Group by Landcover and DBH class, then calculate proportions
# proportions_df <- filtered_data %>%
# group_by(Sample__ID,Landcover,Hill__side, DBH_class) %>%
# summarise(Count = n()) %>%
# group_by(Landcover) %>%
# mutate(Proportion = Count / sum(Count) * 100)
# want proportions at the plot-level, not grouped by land cover
proportions_df <- filtered_data %>%
  group_by(Sample__ID,Landcover,Hill__side, DBH_class) %>%
  dplyr::summarise(Count = n()) %>%
  group_by(Sample__ID) %>%
  mutate(Proportion = Count / sum(Count) * 100)
head(proportions_df)
# Check
print(proportions_df,n=50)
# Visualize the results
ggplot(proportions_df, aes(x = Landcover, y = Proportion, fill = DBH_class)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d(option = "cividis") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "DBH Frequency Classes", x = "Landcover", y = "Proportion of Trees")+
  theme_bw()
# To answer: Are the proportions statistically different across landcovertypes?
# Check the variance
leveneTest(proportions_df$Proportion~proportions_df$Landcover)
# In my case, the variance is homogeneous
# Check distribution of the data
hist(proportions_df$Proportion)
# On my case the data are positively skewed
# Transform the data
hist(sqrt(proportions_df$Proportion))
# Fit a lmm to account for dependence using the Hill_side
# class 5-10cm
# Filter the data frame for the specific DBH_class
data_subset5_10cm <- proportions_df[proportions_df$DBH_class == "5-10cm", ]
# Fit the lmer model
model_DBHproportions5_10cm <- lmer(sqrt(Proportion) ~ Landcover + (1|Hill__side), data = data_subset5_10cm)
residuals_model5_10cm<-residuals(model_DBHproportions5_10cm)
summary(model_DBHproportions5_10cm)
#check residuals
plot(model_DBHproportions5_10cm)
plot(residuals_model5_10cm)
qqnorm(residuals_model5_10cm)
qqline(residuals_model5_10cm)
# ok residuals, not clear patterns
# To implement pairwise comparisons
emmeans(model_DBHproportions5_10cm , pairwise ~ Landcover)
# See the results in the original scale
# Back-transform the EMMs
emms5_10cm<-emmeans(model_DBHproportions5_10cm , pairwise ~ Landcover)
emms_backtransformed5_10cm <- regrid(emms5_10cm, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed5_10cm
#use pairs() to display also the p-values
pairs(emms_backtransformed5_10cm)

# class 10_20cm
# Filter the data frame for the specific DBH_class
data_subset10_20cm <- proportions_df[proportions_df$DBH_class == "10-20cm", ]
# Fit the lmer model
model_DBHproportions10_20cm <- lmer(sqrt(Proportion) ~ Landcover + (1|Hill__side), data = data_subset10_20cm)
residuals_model10_20cm<-residuals(model_DBHproportions10_20cm)
summary(model_DBHproportions10_20cm)
#check residuals
plot(model_DBHproportions10_20cm)
plot(residuals_model10_20cm)
qqnorm(residuals_model10_20cm)
qqline(residuals_model10_20cm)
#ok residuals, not clear patterns
# To implement pairwise comparisons
emmeans(model_DBHproportions10_20cm , pairwise ~ Landcover)
# See the results in the original scale
# Back-transform the EMMs
emms10_20cm<-emmeans(model_DBHproportions10_20cm , pairwise ~ Landcover)
emms_backtransformed10_20cm <- regrid(emms10_20cm, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed10_20cm
#use pairs() to display also the p-values
pairs(emms_backtransformed10_20cm)

# class 20_30cm
# Filter the data frame for the specific DBH_class
data_subset20_30cm <- proportions_df[proportions_df$DBH_class == "20-30cm", ]
# Fit the lmer model
model_DBHproportions20_30cm <- lmer(sqrt(Proportion) ~ Landcover + (1|Hill__side), data = data_subset20_30cm)
residuals_model20_30cm<-residuals(model_DBHproportions20_30cm)
summary(model_DBHproportions20_30cm)
#check residuals
plot(model_DBHproportions20_30cm)
plot(residuals_model20_30cm)
qqnorm(residuals_model20_30cm)
qqline(residuals_model20_30cm)
#ok residuals, not clear patterns
# To implement pairwise comparisons
emmeans(model_DBHproportions20_30cm , pairwise ~ Landcover)
# See the results in the original scale
# Back-transform the EMMs
emms20_30cm<-emmeans(model_DBHproportions20_30cm , pairwise ~ Landcover)
emms_backtransformed20_30cm <- regrid(emms20_30cm, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed20_30cm
#use pairs() to display also the p-values
pairs(emms_backtransformed20_30cm)

# class >=30cm
# Filter the data frame for the specific DBH_class
data_subset_30cm <- proportions_df[proportions_df$DBH_class == ">=30cm", ]
# Fit the lmer model
model_DBHproportions_30cm  <- lmer(sqrt(Proportion) ~ Landcover + (1|Hill__side), data = data_subset_30cm )
residuals_model_30cm <-residuals(model_DBHproportions_30cm)
summary(model_DBHproportions_30cm)
#check residuals
plot(model_DBHproportions_30cm)
plot(residuals_model_30cm)
qqnorm(residuals_model_30cm)
qqline(residuals_model_30cm)
#ok residuals, not clear patterns
# To implement pairwise comparisons
emmeans(model_DBHproportions_30cm , pairwise ~ Landcover)
# See the results in the original scale
# Back-transform the EMMs
emms_30cm<-emmeans(model_DBHproportions_30cm , pairwise ~ Landcover)
emms_backtransformed_30cm <- regrid(emms_30cm, transform = T,predict.type)
# View the back-transformed EMMs and pairwise comparisons
emms_backtransformed_30cm
#use pairs() to display also the p-values
pairs(emms_backtransformed_30cm)
