#### Correlation between all the soil and air variables ####
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Test the correlation
cor(pcaplot[4:12])
# compute better and see results
res1<- cor(pcaplot[4:12], method="spearman")
round(res,2)
# Display p-values
res<-rcorr(as.matrix(pcaplot[4:12]),type='spearman')
# Extract the correlation coefficients
res$r
# Extract p-values
res$P
# To visualise
corrplot(res1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, cl.pos = "b")
# Another type
corrplot(res1, method = 'number',tl.col = "black")
# Another way to visualise
chart.Correlation(pcaplot[4:12], histogram=TRUE, pch=19)
# With other colors
corrplot(res1, order = 'hclust', addrect = 2, tl.col = "black")
#### Correlation between Air parameters/Soil nutrients and landcover ####
# Air T
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Plot to visualise
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(pcaplot$Landcovertype)
plot(pcaplot$Air.T~pcaplot$Landcovertype,col=colori)
# Test the correlation
summary(lm(pcaplot$Air.T~pcaplot$Landcovertype))
# Present the correlation

# Air h
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Plot to visualise
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(pcaplot$Landcovertype)
plot(pcaplot$Air.h~pcaplot$Landcovertype,col=colori)
# Test the correlation
summary(lm(pcaplot$Air.h~pcaplot$Landcovertype))
# Present the correlation

# Soil h
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Plot to visualise
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(pcaplot$Landcovertype)
plot(pcaplot$Soil.h~pcaplot$Landcovertype,col=colori)
# Test the correlation
summary(lm(pcaplot$Soil.h~pcaplot$Landcovertype))
# Not present the correlation but not different among land cover types

# TN
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Plot to visualise
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(pcaplot$Landcovertype)
plot(pcaplot$TN...~pcaplot$Landcovertype,col=colori)
# Test the correlation
summary(lm(pcaplot$TN...~pcaplot$Landcovertype))
# Present the correlation with land cover types

# AP
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Plot to visualise
colori<-c("green","orange","#ff46a2", "yellow","turquoise")
names(colori)<-levels(pcaplot$Landcovertype)
plot(pcaplot$AP.mg.kg.~pcaplot$Landcovertype,col=colori)
# Test the correlation
summary(lm(pcaplot$AP.mg.kg.~pcaplot$Landcovertype))
# Not present the correlation and different among land cover types

#### Plots for some environmental parameters ####
# Air T
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Write a vector with them
Letters<- c("A","A","B","BC","C")
# Create a dataframe with them
BoxplotLetters <- data.frame(Landcovertype = unique(pcaplot$Landcovertype), Letters = Letters)
# Check
BoxplotLetters
# Merge the data frames
AirTplot <- merge(pcaplot, BoxplotLetters, by = "Landcovertype")
# Calculate label y-positions above the boxplots
# Find the upper whisker for each boxplot
upper_whiskers <- aggregate(Air.T ~ Landcovertype, data = AirTplot, FUN = function(x) boxplot.stats(x)$stats[5])
AirTplot <- merge(AirTplot, upper_whiskers, by = "Landcovertype", suffixes = c("", "_upper"))
#Calculate the position for the letters
AirTplot$label_y <- AirTplot$Air.T_upper+1
# Plot
AirTemperaturePlot<-ggplot(AirTplot, aes(x = Landcovertype, y = Air.T)) +
  geom_boxplot(aes(fill = Landcovertype)) +
  geom_point(aes(fill = Landcovertype), cex = 2, alpha = 0.5, colour = "black", pch = 21, stroke = 1) +
  geom_text(aes(y = label_y, label = Letters), size = 5) +  # Use merged data, correct y, and Letters column
  labs(x = "Landcover", y = "Air temperature (°C )") +
  theme_classic()+
  theme(axis.title.y = element_text(size=16))

# Air h
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Write a vector with them
Letters<- c("A","A","B","BC","C")
# Create a dataframe with them
BoxplotLetters <- data.frame(Landcovertype = unique(pcaplot$Landcovertype), Letters = Letters)
# Check
BoxplotLetters
# Merge the data frames
Airhplot <- merge(pcaplot, BoxplotLetters, by = "Landcovertype")
# Calculate label y-positions above the boxplots
# Find the upper whisker for each boxplot
upper_whiskers <- aggregate(Air.h ~ Landcovertype, data = Airhplot, FUN = function(x) boxplot.stats(x)$stats[5])
Airhplot <- merge(AirTplot, upper_whiskers, by = "Landcovertype", suffixes = c("", "_upper"))
#Calculate the position for the letters
Airhplot$label_y <- Airhplot$Air.h_upper+3
# Plot
AirhumidityPlot<-ggplot(Airhplot, aes(x = Landcovertype, y = Air.h)) +
  geom_boxplot(aes(fill = Landcovertype)) +
  geom_point(aes(fill = Landcovertype), cex = 2, alpha = 0.5, colour = "black", pch = 21, stroke = 1) +
  geom_text(aes(y = label_y, label = Letters), size = 5) +  # Use merged data, correct y, and Letters column
  labs(x = "Landcover", y = "Air humidity (%)") +
  theme_classic()+
  theme(axis.title.y = element_text(size=16))

# Soil T plot
pcaplot<-read.table("PCA1.csv",h=T,sep=",")
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("Grassland","2 years old", "10 years old", "24 years old", "Remnant"))
# Write a vector with them
Letters<- c("A","A","B","BC","C")
# Create a dataframe with them
BoxplotLetters <- data.frame(Landcovertype = unique(pcaplot$Landcovertype), Letters = Letters)
# Check
BoxplotLetters
# Merge the data frames
SoilTplot <- merge(pcaplot, BoxplotLetters, by = "Landcovertype")
# Calculate label y-positions above the boxplots
# Find the upper whisker for each boxplot
upper_whiskers <- aggregate(Soil.T ~ Landcovertype, data = SoilTplot, FUN = function(x) boxplot.stats(x)$stats[5])
SoilTplot <- merge(SoilTplot, upper_whiskers, by = "Landcovertype", suffixes = c("", "_upper"))
#Calculate the position for the letters
SoilTplot$label_y <- SoilTplot$Soil.T_upper+1
# Plot
SoilTemperaturePlot<-ggplot(SoilTplot, aes(x = Landcovertype, y = Soil.T)) +
  geom_boxplot(aes(fill = Landcovertype)) +
  geom_point(aes(fill = Landcovertype), cex = 2, alpha = 0.5, colour = "black", pch = 21, stroke = 1) +
  geom_text(aes(y = label_y, label = Letters), size = 5) +  # Use merged data, correct y, and Letters column
  labs(x = "Landcover", y = "Soil temperature (°C )") +
  theme_classic()+
  theme(axis.title.y = element_text(size=16))

# Plot together with one legend
# Extract the legend
legend <- cowplot::get_legend(AirhumidityPlot) # using cowplot package
# Remove legend from others
AirTemperaturePlot <- AirTemperaturePlot + theme(legend.position = "none")
SoilTemperaturePlot<- SoilTemperaturePlot + theme(legend.position = "none")
# Arrange the plots in one row
plots_row <- plot_grid(AirhumidityPlot + theme(legend.position = "none"), AirTemperaturePlot , SoilTemperaturePlot, nrow = 1) # remove legend from plot1
final_plot <- plot_grid(plots_row, legend, nrow = 1, rel_widths = c(7, 0.8)) # add legend to the right
print(final_plot)
