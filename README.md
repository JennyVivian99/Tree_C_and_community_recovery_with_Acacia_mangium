# Code for analysis of tree community, above-ground biomass of standing trees and bulk topsoil organic carbon

This code is part of [INSERT PUBLICATION WHEN DONE] and was created for the analysis of the tree community, aboveground biomass and soil nutrients of different _Acacia mangium_ plantations, in comparison to reference states of _Imperata cylindrica_ grasslands and remnant forests all located in the Philippines.
Data collection methods are described in [PUBLICATION].

Analyses are organised in separate folders in which a brief description is provided (README) along with the dataset used.

Below are reported the required packages and useful references for analysis understanding and results interpretation.

## Required packages to run the analyses:
```
library(MuMIn)
library(vegan)
library(devtools)
library(pairwiseAdonis)
library(devtools)
library(httr)
library(iNEXT)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(sp)
library(nlme)
library(gstat)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(fossil)
library(FactoMineR) # For PCA with factors
library(factoextra) # For PCA visualization
library(Hmisc) # For correlation
library(PerformanceAnalytics) # For correlation visualisation
library(corrplot) # For correlation visualisation
library(multcompView) # To visualise letters over boxplots in ggplot
library(gridExtra) # To put plots of ggplot in one row
library(cowplot) # To put plots of ggplot in one row
library(scales) # To see default colors of ggplot
library(indicspecies)
```
# Useful references:

## For variogram generation:
- https://www.uprm.edu/ccs-cisac/performing-variogram-in-/#:~:text=Step%201%3A%20Prepare%20the%20Data,input%20and%20output%20file%20paths.&text=Data%20is%20comprised%20of%203,(y%20coordinates)%2C%20Col
## For spatial models:
- https://www.flutterbys.com.au/stats/tut/tut8.4a.html
## For Kruskal-Wallis test
- https://bookdown.org/thomas_pernet/Tuto/non-parametric-tests.html
## For Post-hoc analyses in spatial model
- https://cran.r-project.org/web/packages/emmeans/vignettes/models.html#quickref
- https://cran.r-project.org/web/packages/emmeans/emmeans.pdf
## Models comparisons using performance package                                         
- https://easystats.github.io/performance/reference/model_performance.html
## Hill's number resources
- https://cran.r-project.org/web/packages/iNEXT/index.html
- https://besjournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F2041-210X.12613&file=mee312613-sup-0001-AppendixS1.pdf
- https://sites.google.com/view/chao-lab-website/software/inext
- https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12613
- https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-0133.1
- Tutorial: https://www.youtube.com/watch?app=desktop&v=bUFdei_zl88
## For analyses interpretation see also 
- https://stats.libretexts.org/Bookshelves/Applied_Statistics/Natural_Resources_Biometrics_(Kiernan)/10%3A_Quantitative_Measures_of_Diversity_Site_Similarity_and_Habitat_Suitability/10.01%3A_Introduction__Simpsons_Index_and_Shannon-Weiner_Index
## For asymptotic understanding: 
- https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-0133.1                                   
- https://ericmarcon.github.io/entropart/articles/extrapolation.html#:~:text=Thus%2C%20the%20asymptotic%20estimators%20of,a%20function%20of%20sample%20size.
## For pairwise adonis installation
```
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
```
## For Pielou index see
- https://www.rpubs.com/roalle/mres_2019
## For R2 evaluation in lmm see
- https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf
## To calculate and see correlations
- https://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
## For indicspecies interpretation
- https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/isa/
- https://www.digitalocean.com/community/tutorials/predict-function-in-r
- https://cran.r-project.org/web/packages/indicspecies/vignettes/IndicatorSpeciesAnalysis.html#the-function-indicators  
# For simpler analysis (contribution of species in community composition) 
- Vegan's help file on the simper function is pretty clear, as reported in https://www.researchgate.net/post/Can_SIMPER_vegan_R_analysis_be_done_on_binary_distance_matrices_particularly_those_containing_ASV_data#:~:text=SIMPER%20reads%20in%20a%20community,is%20computed%20during%20the%20procedure
# For interpretation simper analysis (contribution of each species to community composition) 
- https://kaiw3.github.io/Multivariate_Data_Tutorial_Core/docs/#section4 

# Summary of the datasets used and located in relative folders
```
read.table("Heightcomparison.csv",h=T,sep=",")
read.csv("TreeSpecies2.csv", h=T)
read.table("CarbonDistribution.csv",h=T,sep=",")
read.table("GoodPaired_analyses_DBH_BiomassNoCoconut2.csv",h=T,sep=",")
read.table("BasalAreaPerHaPerPlot.csv",h=T,sep=",")
read.table("Small_stems.csv",h=T,sep=",")
read.table("BiomassContributionClean.csv",h=T,sep=",")
read.table("TreeSpeciesLandcover.csv",sep=',',h=T)
read.table("NativeExoticDifference.csv",h=T,sep=",")
read.table("AcaciaMangiumRelativeBiomassContribution.csv",h=T,sep=",")
read.table("Soil_data.csv",h=T, sep=",")
read.csv("TreeSpecies.csv", h=T, sep=",")
read.table("PCA.csv",h=T,sep=",")
read.table("PCA1.csv",h=T,sep=",")
read.table("BiomassContributionPlot.csv",h=T,sep=",")
read.table("AcaciaMangiumRelativeBiomassContributionForVisualisation.csv",h=T,sep=",")
read.table("BiomassContributionNoDeadTrees.csv",h=T,sep=",")
read.table("C_comparisons.csv",h=T,sep=",")
```
