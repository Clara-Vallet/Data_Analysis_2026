#################################################################################################################################################
#######################################################         PCA PLOT        #################################################################
#################################################################################################################################################


########################################################## Loading The Packages #################################################################

# install if necessary
# install.packages(c("ggrepel", "factoextra", "FactoMineR", "psych"))

library(ggrepel)                                                  # text label
library(factoextra)                                               # PCA visualisation 
library(FactoMineR)                                               # PCA functions
library(psych)                                                    # bartlett test 

bg_col="white"

########################################################## Data Preparation #####################################################################

# read csv table 
data_file <- "data/full_data.csv"                                 # precise path where table is
df <- readr::read_csv(data_file, show_col_types = FALSE)          # silence data type of columns

# date preparation
df <- df %>%
  mutate(
    year     = as.integer(format(as.Date(date_obs), "%Y")),       # extract year based on date
    period   = floor((year - 2001) / 3) * 3 + 2001,               # group into 3-year periods
    period_f = factor(period)                                     # convert to factor for y axis ordering
  ) %>%
  filter(year >= 2001, year <= 2026)                              # keep relevant time 

pca_data <- df %>%
  filter(period == 2022) %>%
  select(species, elevation, annual_tmax_2019, NDVI_Mean_Summer_2019, annual_prec_2019) %>%
  drop_na()

# rename 
pca_matrix <- pca_data %>%
  select(-species) %>%
  rename(
    Altitude = elevation,
    Tmax     = annual_tmax_2019,
    NDVI     = NDVI_Mean_Summer_2019,
    Precipitation = annual_prec_2019
)



################################################################# PCA ###########################################################################

ACP <- PCA(pca_matrix, scale.unit = TRUE)                         # standardize variables as different units

# justification PCA
#  altitude, Tmax, NDVI and Precipitation are quantitative variables 
# individual observations 
# goal is to summarise ecological niche in minimum dimensions



########################################################### Normality Check #####################################################################

# visual normality check per variables as sample size > 5000
par(mfrow = c(1, 4))                                              # the 3 plots side by side
for (var in colnames(pca_matrix)) {                               # loop visual distribution per variable
  hist(pca_matrix[[var]],                   
       main  = paste("Distribution —", var),                      # main title
       xlab  = var,                                                
       col   = "#d9d9d9", border = "white")                     # color format
}
par(mfrow = c(1, 1))                                              # reset layout

# analysis normality
# elevation: most observations between 0 and 500m with a long tail on higher elevations
# Tmax: peak at 15°C with tail on lower values 
# NDVI: peak at 0.7 with tail on low values 
# precipitation: peak at 100mm with a distribution more or less symmetric but slight right tail 
# visual non-normal distribution of variables data but PCA remains relevant because very large sample, no extreme and do not require normality 



######################################################### Correlation Matrix ####################################################################

# check correlations between variables 
COR <- cor(pca_matrix)
round(COR, digits = 2)                                            # round number to 2 digits     

#               Altitude  Tmax  NDVI Precipitation
# Altitude          1.00 -0.22  0.10          0.07
# Tmax             -0.22  1.00  0.07         -0.42
# NDVI              0.10  0.07  1.00         -0.04
# Precipitation     0.07 -0.42 -0.04          1.00

# analysis 
# no variable need to be removed because no pair exceed r = 0.9
# all variables are enoughly independent to keep all of them



############################################################# Eigen Values ######################################################################

# kaiser criterion
round(ACP$eig, digits = 2)

#        eigenvalue percentage of variance cumulative percentage of variance
# comp 1       1.50                  37.62                             37.62
# comp 2       1.09                  27.25                             64.87
# comp 3       0.86                  21.46                             86.33
# comp 4       0.55                  13.67                            100.00

# analysis 
# PC1 and PC2 can be retained as being >1 explaining 64.9 total variance
# reflexion to add PC3 as it could explained in the end 86 variance even if PC3<1

# scree plot 
fviz_eig(ACP,
         addlabels = TRUE,
         barfill   = "#d9d9d9",
         barcolor  = "grey50",
         linecolor = "#1C3EAA") +
  labs(title = "Scree plot — variance explained per axis") +
  theme_classic()

# analysis 
# no clear elbow characteristic of independent dataset 
# no single axis dominate but slight angle between PC2 and PC3 could support keeping only the two first factor



######################################################## Absolute Contribution ##################################################################

# which variables contribute most to each axis ?
round(ACP$var$contrib, digits = 2)

#               Dim.1 Dim.2 Dim.3 Dim.4
# Altitude      13.75 34.46 43.07  8.71
# Tmax          47.13  0.32  0.01 52.54
# NDVI           0.58 61.54 36.36  1.53
# Precipitation 38.53  3.69 20.56 37.22

# analysis 
# PC1 mostly explained by Tmax and precipitation (thermal/moisture gradient)
# pC2 mostly explained by NDVI and altitude (vegetation/altitude gradient)
# pc3 also explained by altitude and NDVI like pc2 so good choice not to keep it



######################################################## Relative Contribution ##################################################################

# how well is each observation represented on each axis ?
# cannot be do with round(ACP$ind$cos2, digits = 2) with 10 000 individuals so use per species

# mean COS2 per species
print(cos2_df <- as.data.frame(ACP$ind$cos2) %>%                  # matrix CO2 individuals into dataframe
  mutate(species = pca_data$species) %>%                          # add species column back that was previously removed
  group_by(species) %>%                                           # gather each species
  summarise(                                                      # collapse all individuals of a species into one row
    mean_cos2_PC1  = round(mean(Dim.1), 3),                       # average representation on PC1
    mean_cos2_PC2  = round(mean(Dim.2), 3),                       # average representation on PC2
    mean_cos2_F1F2 = round(mean(Dim.1 + Dim.2), 3))               # average total representation on PC1/PC2
)

#   species                   mean_cos2_PC1 mean_cos2_PC2 mean_cos2_F1F2
#   Coccinella septempunctata         0.317         0.317          0.633
#   Harmonia axyridis                 0.351         0.342          0.693

# analysis 
# C. septempunctata: 0.317 PC1 and 0.317 PC2 so symetrical representation
# H. axyridis : 0.351 PC1 et 0.342 PC2 so almost symetrical representation also and sightly better represented
# species are both represented similarly on the two axis 
# 0.633 and 0.693 are coherent with explained variance previously found (65%) moderate but okay

# proportion of individuals well represented per species 
print(cos2_raw <- as.data.frame(ACP$ind$cos2) %>%                 # matrix CO2 individuals into dataframe
  mutate(species = pca_data$species) %>%                          # add species column back that was previously removed %>%
  group_by(species) %>%                                           # gather each species
  summarise(                                                      # collapse all individuals of a species into one row
    pct_well_PC1  = round(mean(Dim.1 > 0.5) * 100, 1),            # % of individuals with cos2 > 0.5 on PC1
    pct_well_PC2  = round(mean(Dim.2 > 0.5) * 100, 1),            # % of individuals with cos2 > 0.5 on PC2
    pct_well_F1F2 = round(mean(Dim.1 + Dim.2 > 0.5) * 100, 1)     # % of individuals well represented on PC1PC2 
))

#   species                   pct_well_PC1 pct_well_PC2 pct_well_F1F2
#   Coccinella septempunctata         27.4         28.8          70.5
#   Harmonia axyridis                 33           31.7          79.1

# analysis
# 70.5% for C. septempunctata and 79.1% for H. axyridis individuals are good representation 
# interpretation is valid for the majority of observations



######################################################### Add Species Ellipse ###################################################################

# spcies added projected afterward without influencing ACP axes
ACP_sup <- PCA(
  cbind(pca_matrix, species = pca_data$species),                  # add species column to the numeric matrix   
  quali.sup  = 5,                                                 # qualitative column species
  scale.unit = TRUE,                                              # standardise variables
)



########################################################### Variable Circle #####################################################################

# direction and strenght of each variable on PC1 and PC2
fviz_pca_var(ACP_sup)

# analysis 
# arrows pointing in the same direction = positively correlated variables
# arrows pointing in opposite directions = negatively correlated variables
# arrow length = quality of representation (longer = better represented)
# Tmax points to the left & Precipitation points right (negatively correlated) =  contribution axis PC1
# biological explainaition: warmer zones are drier
# NDVI points to the up & altitude points up/right (positively correlated) =  contribution axis PC2 (bit PC1 for altitude)
# biological explainaition: PC2 separates observations found in rich and moderately elevated habitats from those in low-elevation and less vegetated environments



###################################################### graphical representation #################################################################

# PCA graph
graphC_pca <- fviz_pca_biplot(ACP_sup,
                col.ind     = pca_data$species,                   # colour by species
                palette     = c("#FFD700", "#1C3EAA"),        # yellow = invasive & blue = native
                geom.ind    = "point",                            # show points only 
                pointsize   = 0.8,                                # small points 
                alpha.ind   = 0.3,                                # transparent points
                addEllipses = TRUE,                               # ellipses per species 
                col.var     = "black",                            # variable arrows
                label = "none",
                repel       = TRUE) +                             # supplementary variable colour
  theme_classic() +
  theme(legend.position = "none") +
  labs(title = NULL)                                              # remove title

# save graph 
ggsave(
  filename = "data/grap_analysis/graphC_pca.png",                 # path where stock graph
  plot = graphC_pca,                     
  width = 10, height = 7, dpi = 300                               # figure resolution and dimension 
)



############################################################### cluster #########################################################################

# hierarchical clustering on PCA 
CAH <- HCPC(ACP, nb.clust  = 4)                                   # chosen number of clusters (did 3 but 4 result are ecologically more relevant)

# visualisation cluster on PC1/PC2
fviz_cluster(CAH,                                                 # project cluster on PCA axis
             geom      = "point",                                 # points only
             pointsize = 0.8,                                     # small point 
             alpha     = 0.3,                                     # point transparency
             repel     = TRUE) +                                  # non overlapping name
  theme_classic() +
  labs(title = "CAH — cluster map on PCA space")                  # title

# cluster description
CAH$desc.var$quanti

# analysis cluster 1 (warm dry low land habitat)
# Tmax: significantly warmer (14.6°C vs 13.6°C overall)
# Precipitation: significantly drier  (84mm vs 99mm overall)
# Altitude: significantly lower  (472m vs 640m overall)
# NDVI: significantly less vegetated (0.65 vs 0.68 overall)

# analysis cluster 2 (productive mid high habitat)
# Tmax: slightly above average  (13.9°C vs 13.6°C)
# Altitude: significantly strongly above average (1294m vs 640m overall)
# NDVI: significantly higher (0.78 vs 0.68 overall)
# Precipitation: slightly drier

# analysis cluster 3 (moist low land habitat)
# Precipitation: significantly wetter (123mm vs 99mm overall)
# NDVI: significantly slightly above average (0.70 vs 0.68)
# Altitude: significantly below average (518m vs 640m)
# Tmax: significantly slightly cooler (13.3°C vs 13.6°C)

# analysis cluster 4 (cold wet alpine refuge)
# Tmax: strongly below average (5.2°C vs 13.6°C overall)
# Precipitation: significatnly strongly above average (143mm vs 99mm overall)
# Altitude: significatnly above average (937m vs 640m overall)
# NDVI: significatnly below average (0.62 vs 0.68)

# add cluster assignment to the dataset 
pca_data_clust <- pca_data %>%                                    
  mutate(cluster = CAH$data.clust$clust)                          # add cluster label to each occurences

# number species occurence per cluster
pca_data_clust %>%
  group_by(cluster, species) %>%                                 # count observation per group
  summarise(n = n(), .groups = "drop") %>%                       # number observation per group
  group_by(cluster) %>%                                          # regroup by cluster only for percentage
  mutate(                                                        # total per cluster and % per species
    total = sum(n),                                              # total observations in cluster
    pct   = round(n / total * 100, 1)                            # % of each species within cluster
  ) %>%
  arrange(cluster, desc(pct))                                    # dominant species first

#   cluster species                       n total   pct
#   1       Harmonia axyridis          1287  1734  74.2
#   1       Coccinella septempunctata   447  1734  25.8
#   2       Coccinella septempunctata   311   510  61  
#   2       Harmonia axyridis           199   510  39  
#   3       Harmonia axyridis           500   867  57.7
#   3       Coccinella septempunctata   367   867  42.3
#   4       Coccinella septempunctata   134   211  63.5
#   4       Harmonia axyridis            77   211  36.5

# analysis cluster 1 (warm dry low land habitat)
# H. axyridis 74.2% vs C. septempunctata: 25.8% 
# H. axyridis has successfully colonised the Swiss Plateau

# analysis cluster 2 (productive mid high habitat)
# H. axyridis 39.0% vs C. septempunctata: 39.0%
# native species remains present at higher altitude but H. axyridis already present at 39% 
# suggest ongoing competitive pressure even at mid-elevation

# analysis cluster 3 (moist low land habitat)
# H. axyridis 39.0% vs C. septempunctata: 42.3
# most competitive habitat (transitional zone)

# analysis cluster 4 (cold wet alpine refuge)
# H. axyridis 36.5% vs C. septempunctata: 63.5%
# native species remains dominant in the harshest conditions (thermal refuge)


