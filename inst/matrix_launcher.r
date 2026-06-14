###########################################################################################################################
#######                                           Script Loading                                                    #######
###########################################################################################################################


################################## Script: Download of GBif & iNat Observations ###########################################

source("src/A_gbif_iNat.r")       

# script notes
# final output table from the script is 'full_data'
# View(full_data)
# species - coordinates - observation date - source 
# data point limited to human observation and cancel exact same occurence iNat/gBif 
# check up 1 with visualisation distribution of GBIF points on the map of Switzerland for each species
# check up 2 with visualisation distribution of iNaturalist points on the map of Switzerland for each species
# check up 3 with visualisation distribution of iNaturalist and GBIF points on the map of Switzerland for both species
# check up are saved in the file 'graph_data' in data

# check point 1,2 and 3 interpretation
# enough data point for both that cover the switzerland with higher concentration in the plateau which is coherent
# more observation for H. axyridis (invasive) coherent as endangering C. septempunctata by eating their larvae
# C. septempunctata more present in alpins areas coherent with generalist behavior compared to H. axyridis (refuge)
# observation points appear globaly coherent with literature



##################################### Script: Download of Elevation Data ###################################################

source("src/B_elevation.r")

# script notes
# the final output table from the script is 'full_data_elev'
# View(full_data_elev)
# // elevation - terrain type
# terrain type allows to compare distribution area montain/plateau between the two species
# check up 1 with species distribution by elevation 
# check up 2 with proportions of each species by terrain type 
# check up are saved in the file 'graph_elevation' in data

# check point 1 interpretation (elevation_by_species.png)
# data ranging from ~ 200m (low elevation Switzerland) to 3000m (mountain) consistent with swiss topography
# fewer outlier points above 2000m as expected due to harsh conditions
# more native ladybird outliers logical as it is a generalist species compared to the invasive specialist
# lower median for Harmonia axyridis and wider range for Coccinella septempunctata
# test analysis by curiosity W = 19501862, p-value < 2.2e-16
# highly significant difference so the species do not share the same altitudinal niche 
# logical and consistent with the literrature (Kenis et al., 2020)

# check point 2 interpretation (terrain_type_by_species.png)
# dominance of the invasive species on the plateau consistent with the literature (strong invasive competition)
# superiority of the native species in the mountains (refuge) consistent as it is a more generalist species
# everything appears consistent (Kenis et al., 2020)


################################### Script: Download of Ecosystemic Data ##################################################

source("src/C_ecosystem.r")

# script notes
# the final output table from the script is 'full_data_elev_meta_eco'
# View(full_data_elev_meta_eco)
# eco value // temperature type - humidity - vegetation - landscape - RGB system - ecosystem - colour
# check up 1 with number of observations per climate category
# check up 2 with comparison of elevations across climate categories 
# check up are saved in the file 'graph_ecosystem' in data

# check point 1 interpretation (observations_by_climate.png)
# predominance of 'Cool Temperate Moist' consistent with Switzerland i.e. plateau and mid-altitude
# high human density in these areas hence high observation count, peak justified and consistent
# higher density of Harmonia axyridis in 'Warm Temperate' and 'Cool Temperate' zones
# consistent with the invasion and dominance of the invasive ladybird in the shared niche
# few observations in 'polar moist' (rare) and 'boreal moist' (not present in Switzerland)
# consistent as extreme zones for ladybirds and few observers
# more native ladybirds in cold zones logical as it is a generalist species compared to the invasive specialist
# see graph produced during data analysis and interpretation

# check point 2 interpretation (elevation_by_climate.png)
# warm temperate moist match with low altitudes (< 500m) which is consistent with tplateau
# cool temperate moist match with 500-1500m which is consistent with pre-alpine and colline znes
# polar moist match with 2000-3000m which is consistent with alpine and subalpine zones
# boreal moist almost absent which is consistent with its near-absence in Switzerland
# separation between low (warm/cool temperate) and high climates (polar) ecologically coherent
# ecosystem raster is validated



##################################### Script: Download of Satellite Data ##################################################

source("src/D_satellite.r")

# script notes
# the final output table from the script is 'full_data_elev_meta_eco_sat'
# View (full_data_elev_meta_eco_sat)
# // mean NDVI winter 2019 - mean NDVI spring 2019 - mean NDVI summer 2019 - mean NDVI autumn 2019 - NDVI July 1980//2019
# 1980 chosen as start of  Harmonia axyridis invasion to have conditions at the beginning of the invasion
# 2019 chosen as year available (data) with maximum observation ; also representative year to have condition during invasion
# 2019 is approximately middle between 1980 and 2050 (useful for climate data)
# mean NDVI 2019 per season just by curiosity to check evolution of NDVI over one year
# NDVI selected for only july (pic NDVI) from 1980 to 2019 to do evolution of conditions invasion but NASA gave me only from 2000
# only chose NDVI because it is the most relevant for my research project (LAI and EVI are similar to NDVI)
# perhaps could have done nocturn light to verify proxy of human presence (normally more invasive)
# check up 1 with positioning of observation points on the 24 NDVI rasters
# check up 2 with observation points coloured by NDVI per season
# check up 3 with NDVI distribution by climate per season
# check up 4 with NDVI july from 2000 to 2019
# check up are saved in the file 'graph_sat' in data

# check point 1 interpretation (ndvi_observation_points.png)
# observation points appear to be positioned at the same location on each map and within the borders
# overlay of points on the raster visually successful

# check point 2 interpretation (ndvi_by_season_map.png)
# observation points appear to be positioned at the same location on each map and within the borders
# NDVI globally minimum in winter and maximum consistent visually as more vegetation in summer
# low NDVI at mountain observation points (even lower in winter) consistent as snow-covered

# check point 3 interpretation (ndvi_distribution_by_climate_season.png)
# value range respected, from -1 to 1 for NDVI
# one cluster around 0 (Boreal Moist and Polar Moist) corresponding to high altitudes
# vegetation index (NDVI) close to zero logical as presence of snow and minerals but rarely vegetation
# one cluster around 0.6 (Warm Temperate Moist and Cool Temperate Moist) corresponding to mid-altitudes (plateau)
# vegetation index (NDVI) close to 1 logical as presence of forests and agricultural areas
# shift of vegetation peaks across seasons consistent
# peak (Boreal Moist and Polar Moist) in autumn lower as slow senescence
# peak (Boreal Moist and Polar Moist) in summer lower as snow has melted
# peak (Warm Temperate Moist and Cool Temperate Moist) +/- constant density (anthropisation and green space maintenance)
# everything appears consistent

# check point 3 interpretation (ndvi_july_by_year_boxplot.png)
# stable NDVI july values across years consistent with vegetation stability on the plateau
# 2019 consistent
# everything appears consistent



#################################### Script: Download of Climate Data #####################################################

source("src/E_climate.r")

# script notes
# the final output table from the script is 'full_data_elev_meta_eco_sat_temp_prec'
# // max temp/prec 2019 - seasonal mean temp/prec 2019 - mean temp 2050 - max temp/prec 1980 - seasonal mean 1980
# 2050 chosen as year to have condition nearer of the end of invasion 
# possibility to compare evolution of condition during invasion 
# less good (3 dates) compared to having each year from 1980 to 2019 for the NDVI but it is okay 
# only precipitation and temperature chosen as other conditions available (wind, ...) not interesting for my research topic 
# check up 1 with temperature-elevation correlation 2019
# check up 2 with temperature-elevation correlation 1980
# check up 3 with temperature-elevation correlation 2050
# check up 4 with precipitation-elevation correlation 2019
# check up 5 with precipitation-elevation correlation 1980
# check up 6 with precipitation-elevation correlation 2050
# check up are saved in the file 'graph_climat' in data

# check up 1 interpretation (tmax2019_vs_elevation.png)
# negative trend curve fully consistent as temperature decreases with altitude
# dense cluster at 500m consistent as many observations on the plateau (residential area)
# scatter at altitude logical as more microclimates than on plateau (e.g. mountain slopes)
# no incoherent outliers (summary(full_data_elev_meta_eco$annual_tmax_2019) gives min: -3 and max: 18 as annual mean)
# everything appears consistent

# check up 2 interpretation (tmax2019_vs_tmax1980.png)
# for the same location maximum temperature in 2019 is higher than in 1980, consistent with climate warming
# everything appears consistent

# check up 3 interpretation (tmax2050_vs_elevation.png)
# negative trend curve fully consistent as temperature decreases with altitude
# dense cluster at 500m consistent as many observations on the plateau (residential area)
# scatter at altitude logical as more microclimates than on plateau (e.g. mountain slopes)
# no outliers (summary(full_data_elev_meta_eco_temp$annual_tas_2050) gives min: -5 and max: 14 as annual mean)
# everything appears consistent

# check up 4 interpretation (prec2019_vs_elevation.png)
# positive trend curve (more precipitation at altitude) consistent with orographic effect
# dense cluster at 500m consistent as many observations on the plateau (residential area)
# scatter at altitude logical as more microclimates than on plateau (e.g. mountain slopes)
# no incoherent outliers (summary(full_data_elev_meta_eco_temp_prec$annual_prec_2019))
# everything appears consistent

# check up 5 interpretation (prec1980_vs_elevation.png)
# similar to 2022
# everything appears consistent

# check up 6 interpretation (prec2050_vs_elevation.png)
# positive trend curve consistent with orographic effect 
# dense cluster at 300-800m consistent as many observations on the plateau (human residence area)
# scatter at altitude logical as highly variable in mountains 
# outliers at high altitude with elevated precipitation (~250mm at 3000m) consistent with humid alpine zones 
# values range 80-250mm consistent with monthly swiss precipitation
# everything appears consistent



######################################################## CSV ##############################################################

write.csv(
  full_data_elev_meta_eco_sat_temp_prec,
  file = "data/full_data.csv",
  row.names = FALSE                                               # avoid adding index column
)



################################################# References Used #########################################################

# Kenis, M., Nacambo, S., Van Vlaenderen, J., Zindel, R., & Eschen, R. (2020).
# Long term monitoring in Switzerland reveals that Adalia bipunctata strongly declines in response to Harmonia axyridis invasion. 
# Insects, 11(12), 883.