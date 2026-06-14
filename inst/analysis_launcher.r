#################################################################################################################################################
######################################################      SCRIPT LOADING      #################################################################
#################################################################################################################################################


############################################################### Background ######################################################################

# Harmonia axyridis is an invasive species coming from Asia introduced in Europe in the 2000s as a biological control species
# Coccinella septempunctata is the most dominant native ladybird in the entire Switzerland
# H. axyridis have been reported to compete native ladibugs through resource competition, predation and chemical defence
# it could be therefore interesting to investigate the extent to which this competitive pressure influence a potential niche displacement
# to do so, we could use geographic occurence data combined with environmental data (full_data)



########################################################### Research Question ###################################################################

# Does the establishment of Harmonia axyridis in Switzerland drive niche displacement in Coccinella septempunctata ?



########################################################### Analysis Structure ##################################################################

# STEP 1 (interactive plot)
# exploration visually of occurences on an interactive map before any statistical graph

source("src/1_interactive_plot.R")  

# interpretation
# overall more invasive ladybird 
# native ladybug appears domiannt in higher areas 
# logical as it is a generalist species compared to the invasive specialist
# variables seem coherent with localisation observation



# STEP 2 (occurence map)
# visualization of the previous interactive map but staticlly
# I tried to add the interactive map on the ggdraw but not possible
# allow to observe geographic distribution

source("src/2_occurence_map.R")

# interpretation similar to the previous one
# H. axyridis (invasive, yellow) dominates low elevated areas while C. septempunctata (blue, native) is more dispersed



# STEP 3 (ridgeline chart)
# visualisation of altitude, Tmax, NDVI and prediction temporarily
# across 3-year periods from 2001 to 2024 because impossible to obtain all year in y
# I did stat tests to obtain significative diffferences but did not succeed to add them on the graph without making it too poorly readable
# allow to see how ecological niche of each species varied accross time since arrival of invasive ladybug
source("src/3_ridgeline_chart.R")

# interpretation 
# only C. septempunctata is present before 2007 consistent with arrival
# from 2010 all variables show significant differences 
# altitude: H. axyridis becomes more and more dominant at low altitude and C. septempunctata distribution shifts to higher altitudes
# Tmax: H. axyridis imediatly occupied warmer zones while C. septempunctata distribution shifts to lower T
# NDVI summer: H. axyridis peaks high NDVI consistent with the plateau while C. septempunctata shows broader NDVI range consistent with species generalist
# precipiration: H. axyridis becomes more and more dominant at low altitude with less precipitation and C. septempunctata distribution shifts to higher precipitation 
# ATTENTION:
# altitude is a fixed geographic variable but Tmax, NDVI and Precipitation are from 2019 data (last available year in CHELSA)
# these variables therefore reflect the GEOGRAPHIC NICHE of observation zones and not actual climate at each period
# so if H. axyridis appears in high-Tmax zones from 2007 onward, it means it colonised geographically warmer areas and not that 2007 was warmer



# STEP 4 (PCA)
# Principal Component Analysis on variables Altitude, Tmax, NDVI, Precipitation for 2022–2024
# allow quantify niche differences and not than evolution over time 
# choose this time range to have the current and more advance results analysis
# I performed assumption check + PCA + CAH. but only PCA on final plot because easier to read

source("src/4_PCA.R")

# interpretation detailed is in the concerned script
# PC1 (37.6%) = thermal-moisture gradient (Tmax + Precipitation)
# PC2 (27.3%) = vegetation-altitude gradient (NDVI + Altitude)
# ellipses overlap confiming invasive slowly taking over native niche
# centroids are significantly offset showing invasive shifted towards warmer & drier conditions on PC1
# CAH identified 4 ecological clusters 
# not logical that invasive niche is bigger because normally should be less generalist but can be explianed by the fact that
# netive disappeared from some areas due to competition so less dispersed as more present in refuge (invasive won areas so bigger circle)
# also two times more invasive
# IMPORTANT: for further analysis, go to the script detailed
# LIMITATION: climate variables from 2019 used becasue that what I downloaded but observation since 2022
# I wanted to download 2022 but chelsea did not want to ... 
# however prec and Tmax temperature between 2022 and 2019 quite similar donc okay 
# and both species compared under identical climate conditions so relative differences might remain valid
# absolute values should be interpreted with caution
# if i had more time (I thought we could give version the 19th) I would have download data for 2022



# STEP 5 (combined plot)
# final production
# 3 graphs (map, PCA, ridgleline chart) assembled
# allow to obtain overview response to the research question
# each panel addresses one dimension of the research question :
# A = where (spatial)
# B = in what environment (ecological niche)
# C = since when and how (temporal niche dynamics)

source("src/5_combined_plot.R")




########################################################## Overall Result ##################################################################

# occurrence map (step 2) shows clear spatial concentration of H. axyridis on the plateau
# CAH (step 4) confirms cluster 1 (warm dry low-altitude) dominated by H. axyridis 
# ridgeline (step 3) shows H. axyridis consistently peaking at low altitude ,high Tmax  and low precipitation from 2007 onward
# consistent with Kenis et al. (2020) finding dominance of H. axyridis in lowland habitats

# before 2007 C. septempunctata occupied more or less full environmental gradient
# from 2010 differences detected across all variables
# progressive restriction to cooler wetter higher-elevation habitats cold alpine refuge 
# confirmed with CAH cluster 4 dominated by C. septempunctata consistent with competitive displacement
# however invasive present in cluster 4 so either cluster 4 can be divided in two allowing to obtain real clear thermal refuge fir native or 
# either no real strict thermal refuge without invasive
# consistent with elevation data (step B) showing lower median altitude for H. axyridis significant (Wilcxon test in  matrix launcher)
# wider altitudinal range for C. septempunctata (generalist behavior)
# PCA ellipses overlap confirming substantial niche sharing between the two species

# invasion followed classic homogeneisation
# H. axyridis rapidly colonised productive and favourable habitats where prey are abondant
# as the invasive is more efficient (defence, ...) in the same niche the competitive dominance in that habitats have progressively restricted
# C. septempunctata to kind of refuges at higher altitude in colder wetter environments where H. axyridis tolerance is less advantageous
# temporal analysis reveals displacement was not immediate but progressive consistent with time required for population growth and expansion
# climate data show temperature and elevation important so warming trend between 1980 and 2019 may have facilitated H. axyridis expansion
# by making ancient marginal habitats more suitable
# could be predicted that increase in temperature will further eliminate  thermal refuges currently used by C. septempunctata if this species do not find other refuge 
# or defence mehanissms
