###########################################################################################################################
#######                               Adding Satellite Data To The Dataset                                          #######
###########################################################################################################################


############################################## Package Installation ########################################################

# download packages if necessary
# install.packages('luna', repos='https://rspatial.r-universe.dev')
# install.packages('appears', 'MODIStsp')
# install.packages(c("appeears", "terra", "sf", "rnaturalearth", "ggplot2", "dplyr","viridis", "tidyr"))


# load necessary packages
library(luna)                                                     # work with satellite data
# library(MODIStsp)                                               # MODIS download
# library(appeears)                                               # interface to interact with NASA data
library(terra)                                                    # raster manipulation of satellite images
library(sf)                                                       # modern cartography
library(rnaturalearth)                                            # country mapping
library(ggplot2)                                                  # create graphs
library(dplyr)                                                    # data frame manipulation
library(tidyr)                                                    # data organiser
library(viridis)                                                  # colour gradient


############################################ Configuration And Setup ######################################################

# file path management
BASE_DIR <- "./data"                                              # indicates key folder is in the 'data' tab
MODIS_DIR <- file.path(BASE_DIR, "modis")                         # 'modis' folder within 'data'

# avoid duplicates at each script launch
if (!dir.exists(BASE_DIR)) dir.create(BASE_DIR)                   # checks that 'BASE_DIR' is absent before creating it
if (!dir.exists(MODIS_DIR)) dir.create(MODIS_DIR)                 # checks that 'MODIS_DIR' is absent before creating it

# create file to save graph of this part
dir.create("data/graph_sat", showWarnings = FALSE)  


############################################ Preparing The Switzerland Map ################################################

# retrieve Swiss borders
switzerland_sf <- ne_countries(                                      
    scale = "medium",                                             # medium map precision
    returnclass = "sf",                                           # output format 'Simple Features'
    country = "switzerland"                                       # target country (Switzerland)
)

# save for AppEEARS (NASA)
st_write(                                                         # 'sf' function saves spatial object to disk
    switzerland_sf,                                               # object to save
    file.path(BASE_DIR, "switzerland.geojson"),                   # path to save location
    delete_dsn = TRUE                                             # deletes old file to avoid duplicates
)

# visual check
plot(                                                             # create the graph
    st_geometry(switzerland_sf),                                  # extract geometric shape
    col = "lightgray",                                            # background colour of the country
    main = "Target Area: Switzerland"                             # main title
)



############################################ Downloading NASA Data ########################################################

# step 1: create an account on the NASA AppEEARS website to submit an AREA request
# step 2: upload the .data/switzerland.geojson file
# step 3: fill in 'select product' with MOD13Q1.061 and choose the 'layer' NDVI
# step 4: choose the date range for the data
# step 5: choose GeoTIFF format and Geographic (GPS with latitude and longitude)
# step 6: download the NDVI results and save them in .data/appeears_manual_download



########################################### Processing The NASA Raster Data ###############################################

# check the download
manual_tif <- list.files(MODIS_DIR,                               # function to browse MODIS_DIR
    pattern = "NDVI..*\\.tif$",                                   # retrieve only .tif files
    full.names = TRUE)
if(length(manual_tif) == 0) {                                     # check if raster is present in MODIS_DIR
  stop("No .tif file found.")                                     # display error message if no .tif file found
}

# load the data
ndvi_raster <- rast(manual_tif)                                   # load the first .tif file from the list
switzerland_vect <- vect(switzerland_sf)                          # convert Swiss map from 'sf' to 'terra' format

# projection
switzerland_vect <- project(                                      # recalculate coordinates to match raster format (terra)
  switzerland_vect,                                               # object to transform
  crs(ndvi_raster)                                                # object with target coordinate format
)

# crop
ndvi_switzerland <- mask(                                         # disappearance around cookie cutter
  crop(ndvi_raster, switzerland_vect),                            # rough rectangular crop
  switzerland_vect                                                # Swiss cookie cutter
)



################################################ Value Extraction #########################################################

# convert to spatial points
points_vect <- vect(full_data_elev_meta_eco,                      # table usable by 'terra' functions
  geom = c("longitude", "latitude"),                              # coordinate columns X and Y
  crs = "EPSG:4326")                                              # GPS code
points_vect <- project(                                           # position points coordinates on raster
  points_vect, crs(ndvi_switzerland)
)
  
# extraction
ndvi_values <- terra::extract(ndvi_switzerland, points_vect)      # satellite value of geographic data
  


#####################################################  CHECK UP  #########################################################

# display observation points on the 24 rasters from 2019
png("data/graph_sat/ndvi_observation_points.png", width = 1200, height = 1200)
par(mfrow = c(5, 5))
for (i in 1:nlyr(ndvi_switzerland)) {
  plot(ndvi_switzerland[[i]], main = names(ndvi_switzerland)[i])
  points(points_vect, col = "red", pch = ".", cex = 0.1)
}
dev.off()

# interpretation
# observation points appear to be positioned at the same location on each map and within the borders
# overlay of points on the raster visually successful



############################################# Grouping NDVI By Season #####################################################

# merge with the original table
ndvi_values_clean <- ndvi_values[, -1]                            # remove ID column

# extract to sort 2019 files by season
filenames <- basename(manual_tif)                                 # remove access path and keep only name
doy_strings <- regmatches(filenames,                              # extract from name
  regexpr("doy\\d{7}", filenames))                                # only doyXXXXXXX
codes_propres <- gsub("doy", "", doy_strings)                     # only the digits after doy
dates <- as.Date(codes_propres, format = "%Y%j")                  # %Y reads 2019, %j reads day number since 01.01.2019
months <- as.numeric(format(dates, "%m"))                         # isolate month number

# function to define seasons
get_season <- function(m) {                                       # get_season becomes function to sort files
  if (m %in% c(3, 4, 5)) return("Spring")                         # if (m == 3 | m == 4 | m == 5) then spring
  if (m %in% c(6, 7, 8)) return("Summer")                         # if (m == 6 | m == 7 | m == 8) then summer
  if (m %in% c(9, 10, 11)) return("Autumn")                       # if (m == 9 | m == 10 | m == 11) then autumn
  return("Winter")}                                               # otherwise winter
seasons <- sapply(months, get_season)                             # takes month values from files and applies function

# transform into column name with season
colnames(ndvi_values_clean) <- paste0(seasons)                    # rename columns with associated season name

# loop to obtain mean NDVI per season
saisons <- c("Winter", "Spring", "Summer", "Autumn")              # define seasons
moyennes <- data.frame(row = 1:nrow(ndvi_values_clean))           # new results table with row as reference
for (s in saisons) {                                              # loop that does the work
  cols_indices <- grep(s, colnames(ndvi_values_clean))            # grep finds column with season name
  moyennes[[paste0("Mean_", s)]] <-                               # calculate mean per row
  rowMeans(ndvi_values_clean[, cols_indices], na.rm = TRUE)       # remove missing values limiting mean calculation
}

# add to final table
full_data_elev_meta_eco_sat <- cbind(
  full_data_elev_meta_eco, moyennes [, -1])                       # '-1' to remove ID
View(full_data_elev_meta_eco_sat)                                 # columns successfully added

# rename to better understanding

full_data_elev_meta_eco_sat <- full_data_elev_meta_eco_sat %>%
  rename(
    NDVI_Mean_Winter_2019 = Mean_Winter,
    NDVI_Mean_Spring_2019 = Mean_Spring,
    NDVI_Mean_Summer_2019 = Mean_Summer,
    NDVI_Mean_Autumn_2019 = Mean_Autumn
  )

# round to 3 decimals
full_data_elev_meta_eco_sat$NDVI_Mean_Winter_2019 <- round(full_data_elev_meta_eco_sat$NDVI_Mean_Winter_2019, 3)
full_data_elev_meta_eco_sat$NDVI_Mean_Spring_2019 <- round(full_data_elev_meta_eco_sat$NDVI_Mean_Spring_2019, 3)
full_data_elev_meta_eco_sat$NDVI_Mean_Summer_2019 <- round(full_data_elev_meta_eco_sat$NDVI_Mean_Summer_2019, 3)
full_data_elev_meta_eco_sat$NDVI_Mean_Autumn_2019 <- round(full_data_elev_meta_eco_sat$NDVI_Mean_Autumn_2019, 3)

#####################################################  CHECK UP  #########################################################

# transform data into long format
data_long <- full_data_elev_meta_eco_sat %>%
  pivot_longer(                                                   # switch to long format
    cols = starts_with("NDVI_Mean_"),                             # filter to find columns
    names_to = "Saison",                                          # create new season column
    values_to = "NDVI"                                            # numeric value
)

# observation points coloured by NDVI per season
g9<-ggplot() +
theme_void() +                                                    # white background
geom_sf(data=switzerland_sf,fill="white",color="black") +         # map of Switzerland
geom_point(data = data_long,                                      # observation points
  aes(x = longitude, y = latitude, color = NDVI),                 # graph axes and point colouring
  shape = 16, size = 1, alpha = 0.9) +                            # round shape
facet_wrap(~Saison) +                                             # faceting
scale_color_viridis_c(option = "viridis", 
direction = 1, na.value = "transparent", name = "NDVI") +         # colour gradient similar to raster
labs(
    title = "Mean NDVI Evolution By Season",                      # title
    subtitle = "Points Overlaid On Swiss Borders"                 # subtitle
)

ggsave(
  filename = "data/graph_sat/ndvi_by_season_map.png",
  plot = g9,
  width = 12, height = 10, dpi = 300
)

# interpretation
# observation points appear to be positioned at the same location on each map and within the borders
# NDVI globally minimum in winter and maximum consistent visually as more vegetation in summer
# low NDVI at mountain observation points (even lower in winter) consistent as snow-covered



#####################################################  CHECK UP  #########################################################

# NDVI distribution by climate per season
g10<-ggplot(data_long, aes(x = NDVI, fill = Climate_Re)) +             # graph axes
  geom_density(alpha = 0.5, adjust = 3) +                         # observation points
  facet_wrap(~Saison) +                                           # visualisation for each season
  labs(
    title = "NDVI Distribution By Climate And Season",            # main title
    x = "NDVI",                                                   # x axis title
    y = "Density",                                                # y axis title
    fill = "Climate Type"       
  ) + theme_minimal()                                             # clean style
 
ggsave(
  filename = "data/graph_sat/ndvi_distribution_by_climate_season.png",
  plot = g10,
  width = 12, height = 10, dpi = 300
)

# interpretation
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



###############################################  NDVI July 1980-2019  ####################################################

# file path 
BASE_DIR_BIS <- "./data"                                          # indicates key folder is in the 'data' tab
MODIS_DIR_BIS <- file.path(BASE_DIR_BIS, "modis-2")               # 'modis' folder within 'data'

# avoid duplicates at each script 
if (!dir.exists(BASE_DIR_BIS)) dir.create(BASE_DIR_BIS)           # checks that 'BASE_DIR' is absent before creating it
if (!dir.exists(MODIS_DIR_BIS)) dir.create(MODIS_DIR_BIS)         # checks that 'MODIS_DIR' is absent before creating it

# all tif files across all years
all_tif <- list.files(MODIS_DIR_BIS,                             
    pattern = "NDVI..*\\.tif$",                                   # only .tif files
    full.names = TRUE)                                            # full path

# extract dates 
all_filenames  <- basename(all_tif)                               # keep only name
date_strings   <- regmatches(all_filenames,                       
  regexpr("\\d{8}(?=T000000)", all_filenames,                     # find the 8 digits before T000000
  perl = TRUE))                                                   # lookahead
all_dates      <- as.Date(date_strings, format = "%Y%m%d")        # convert YYYYMMDD to date
all_years      <- as.numeric(format(all_dates, "%Y"))             # isolate year number

# all files are july rasters
july_tif   <- all_tif                                             # all files are july
july_years <- all_years                                           # corresponding years

# check number good files
message("July rasters found: ", length(july_tif), " files covering years: ", 
        paste(july_years, collapse = ", "))



############################################# Extract NDVI July Per Year ##################################################

# load + crop each july raster then extract values
july_ndvi_list <- list()                                          # results per year

for (j in seq_along(july_tif)) {                                  # loop each july raster
  
  yr <- july_years[j]                                             # current year
  message("--- Processing July NDVI for year: ", yr, " ---")

  # load raster
  ndvi_july_r <- rast(july_tif[j])                                # load july raster

  # project switzerland
  switzerland_vect_j <- project(vect(switzerland_sf),             # swiss borders 
    crs(ndvi_july_r))

  # crop to switzerland
  ndvi_july_ch <- mask(                                           # cut
    crop(ndvi_july_r, switzerland_vect_j),                        # rectangular crop
    switzerland_vect_j)                                           # swiss borders

  # project observation points
  points_vect_j <- project(                                       # align points with raster
    vect(full_data_elev_meta_eco,                                 # observation points
      geom = c("longitude", "latitude"),                          # coordinate columns
      crs = "EPSG:4326"),                                         # standard GPS code
    crs(ndvi_july_ch))

  # extract NDVI values for each observation point
  ndvi_july_vals <- terra::extract(ndvi_july_ch, points_vect_j)   # one value per point

  # store with year label
  july_ndvi_list[[paste0("NDVI_July_", yr)]] <- ndvi_july_vals[, 2] # keep only last one july of the year (no mean)

}

# combine all years into one data frame
july_ndvi_df <- as.data.frame(july_ndvi_list)                     # one column per year

# round to 3 decimals
july_ndvi_df <- round(july_ndvi_df, 3)                            # clean values

# add to final table
full_data_elev_meta_eco_sat <- cbind(                             # merge with base table
  full_data_elev_meta_eco_sat, july_ndvi_df)                      

# check
View(full_data_elev_meta_eco_sat)                                 # july NDVI columns added



#####################################################  CHECK UP  #########################################################

# transform to long format for visualisation
data_july_long <- full_data_elev_meta_eco_sat %>%
  pivot_longer(                                                   # long format
    cols = starts_with("NDVI_July_"),                             # filter july columns but only that so okay
    names_to = "Year",                                            # new year column
    values_to = "NDVI_July"                                      
  ) %>%
  mutate(Year = gsub("NDVI_July_", "", Year))                     # keep only year number

# NDVI july evolution by year
g12 <- ggplot(data_july_long, aes(x = Year, y = NDVI_July, fill = Year)) +
  geom_boxplot(outlier.shape = NA) +                             # hide outliers 
  geom_jitter(width = 0.2, alpha = 0.03, size = 0.3,             # individual observation 
    color = "black") +
  labs(
    title = "NDVI July Distribution By Year",                     # main title
    x = "Year",                                                   # x axis title
    y = "NDVI"                                                    # y axis title
  ) +
  theme_minimal() +                                               # clean style
  theme(legend.position = "none")                                 # remove legend 

ggsave(
  filename = "data/graph_sat/ndvi_july_by_year_boxplot.png",
  plot = g12,
  width = 12, height = 7, dpi = 300
)

# interpretation
# stable NDVI july values across years consistent with vegetation stability on the plateau
# 2019 consistent
# everything appears consistent