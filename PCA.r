# Load the dataset
pcaplot<-read.table("PCA.csv",h=T,sep=",")
# Remove the grassland
pcaplot<- pcaplot[pcaplot$Landcovertype != "Grassland", ]
# Transform into factor
pcaplot$Landcovertype<- factor(pcaplot$Landcovertype , levels = c("2 years old", "10 years old", "24 years old", "Remnant"))
pcaplot$Hill_Side<-as.factor(pcaplot$Hill_Side)
# Check
summary(pcaplot)
# PCA Analysis
res.pca <- PCA(pcaplot[, 5:17], # Quantitative variables
               scale.unit = TRUE, # Important: Standardize the variables
               graph = FALSE) # Set to TRUE for basic plots
# Print summary of PCA results
summary(res.pca)
# See all results
var_contrib<-res.pca$var$contrib
res.pca$var$contrib
# Order contributions for PC1
ordered_contrib_pc1<- data.frame(
  Variable = rownames(var_contrib), # Extract row names as a column
  PC1 = var_contrib[, "Dim.1"]
)
ordered_contrib_pc1 <- ordered_contrib_pc1[order(ordered_contrib_pc1$PC1, decreasing = TRUE),]
rownames(ordered_contrib_pc1) <- NULL #Remove row names
print(ordered_contrib_pc1)
# Order contributions for PC2
ordered_contrib_pc2<- data.frame(
  Variable = rownames(var_contrib), # Extract row names as a column
  PC2 = var_contrib[, "Dim.2"]
)
ordered_contrib_pc2<- ordered_contrib_pc2[order(ordered_contrib_pc2$PC2, decreasing = TRUE),]
rownames(ordered_contrib_pc2) <- NULL #Remove row names
print(ordered_contrib_pc2)
# Order contributions for PC3
ordered_contrib_pc3 <- data.frame(
  Variable = rownames(var_contrib), # Extract row names as a column
  PC3 = var_contrib[, "Dim.3"]
)
ordered_contrib_pc3 <- ordered_contrib_pc3[order(ordered_contrib_pc3$PC3, decreasing = TRUE),]
rownames(ordered_contrib_pc3) <- NULL #Remove row names
print(ordered_contrib_pc3)
# Display ordered contributions for all PCs in a data frame
all_contributions<-cbind(ordered_contrib_pc1,ordered_contrib_pc2,ordered_contrib_pc3)
all_contributions
# Visualize the contribution
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
# Eigenvalues/Variances
eig.val <- get_eigenvalue(res.pca)
print(eig.val)
# Visualize eigenvalues (screen plot)
fviz_eig(res.pca)
# Visualize individuals (samples)
fviz_pca_ind(res.pca,
             col.ind = pcaplot$Landcovertype, # Color by Species (factor)
             palette = c("orange","#ff46a2", "yellow","turquoise"),
             addEllipses = T, # Add concentration ellipses
             ellipse.level = 0.95,
             repel = TRUE,     # Avoid text overlapping
)
# Visualize variables
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contribution to the PCs
             gradient.cols = c("#00AFBB", "#E7B800", "red"),
             repel = TRUE     # Avoid text overlapping
)
# Biplot (individuals and variables)
fviz_pca_biplot(res.pca, 
                col.var = "#646464", repel = TRUE,
                col.ind = pcaplot$Landcovertype,
                palette = c("orange","#ff46a2", "yellow","turquoise"),
                addEllipses = TRUE, label = "var", mean.point = FALSE,
                ellipse.type = 'confidence', ellipse.level=0.98,
                legend.title = "Land covergbnml",
                ggtheme = theme_minimal())
