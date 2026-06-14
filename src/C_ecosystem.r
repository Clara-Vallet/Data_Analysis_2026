###########################################################################################################################
#######                             Adding Ecosystemic Data to the Dataset                                          #######
###########################################################################################################################


############################################# Package Download #############################################################

# install if necessary
# install.packages(c("raster", "rnaturalearth", "ggplot2", "sf"))

library(rnaturalearth)                                            # access maps
library(ggplot2)                                                  # create graphs and maps
library(raster)                                                   # manipulate geographic data
library(sf)                                                       # manipulate vector data



######################################### Download The Ecosystem Raster ###################################################

# create file to save graph of this part
dir.create("data/graph_ecosystem", showWarnings = FALSE)   

# path to the ecosystemic data file
file_path <- "./data/WorldEcosystem.tif"                          # GeoTIFF format

# load the raster file into R
ecosystem_raster <- raster(file_path)                             # numerically coded data

# display raster properties
print(ecosystem_raster)                                           # resolution, extent, coordinate system, ...

# simple global ecosystem map
plot(ecosystem_raster, main = "Original Ecosystem Raster")        # world map with coloured numeric scale



###########################################  Restrict Data To Switzerland   ###############################################

# retrieve Swiss borders
Switzerland <- ne_countries(                                      
    scale = "medium",                                             # medium map precision
    returnclass = "sf",                                           # output format 'Simple Features'
    country = "switzerland"                                       # target country (Switzerland)
)   

# visualise the loaded Swiss borders
plot(st_geometry(Switzerland), main = "Swiss Borders")            # extract polygon only from 'sf'

# crop the raster to a rectangle around Switzerland
r2 <- crop(ecosystem_raster, extent(Switzerland))                 # reduces data size

# mask out irrelevant data
ecosystem_switzerland <- mask(r2, Switzerland)                    # pixels located only within Swiss borders

# display the final result for the study area
plot(ecosystem_switzerland,                                       # raster restricted to Switzerland
  main = "Ecosystem Raster Restricted to Switzerland"             # title
)                                                                 



##############################  Convert Species Coordinates Into Spatial Points   #########################################

# display first rows to check columns
head(full_data_elev)                                              # longitude & latitude present

# transform data into a spatial object
spatial_points <- SpatialPoints(
  coords = full_data_elev[, c("longitude", "latitude")],          # select coordinate columns
  proj4string = CRS("+proj=longlat +datum=WGS84")                 # coordinate system (standard GPS)
)

# align GPS system and raster
spatial_points_aligned <- spTransform(spatial_points, crs(ecosystem_switzerland)) 

# overlay points on the raster map
plot(ecosystem_switzerland,                                       # raster data restricted to Switzerland
      main = "Species Occurrences on Ecosystem Map"               # title
)                                                                 # display raster to add occurrences on top
plot(spatial_points,                                              # GPS data
      add = TRUE,                                                 # keep raster map underneath
      pch = 16,                                                   # filled circle style
      cex = 1.2,                                                  # size of occurrence points
      col = "#b50072"                                           # colour of occurrence points
)                                                                 



#############################  Keep Only The Ecosystemic Data For The Occurrences   ######################################

# pixel value for each observation point
eco_values <- raster::extract(                                    # specifically uses 'extract' from raster
  ecosystem_switzerland, spatial_points                           # raster data, GPS data of occurrences
)

# check the first extracted values
head(eco_values)                                                  # numeric code

# add data to the base table
full_data_elev_eco <- data.frame(full_data_elev, eco_values)      # merge extracted column with base table

# check that the new column is present
head(full_data_elev_eco)                                          # column added successfully
View(full_data_elev_eco)                                          # presence of NA

# check where the NAs are located
points_na <- subset(full_data_elev_eco, is.na(eco_values))        # isolate points that returned NA
plot(ecosystem_switzerland, main="NA Coordinates (Red)")          # title
plot(spatial_points, add=TRUE, col="blue", pch=16, cex=0.5)       # all observations
points(points_na$longitude, points_na$latitude, col="red")        # NAs in red are on the borders

# remove NAs?
sum(is.na(eco_values)) / length(eco_values) * 100                 # loss of 2.9% of the data okay

# remove data as loss is acceptable and points are in areas with many observations
full_data_elev_eco <- full_data_elev_eco %>%
  filter(!is.na(eco_values))

# check NAs
sum(is.na(full_data_elev_eco$eco_values))                         # NA=0



########################  Add Ecosystemic Metadata For Occurrences To The Base Table   ###################################

# load file providing correspondence between number and ecosystem
metadata_eco <- read.delim("./data/WorldEcosystem.metadata.tsv")  

# check the metadata file
head(metadata_eco)                                                # code, temperature, moisture, landcover, ...

# final merge
full_data_elev_meta_eco <- merge(                                 # combine base data table + eco and metadata
  full_data_elev_eco,                                             # via common column
  metadata_eco,
  by.x = "eco_values",                                            # name of common column in table (eco_value)
  by.y = "Value"                                                  # name of corresponding column in metadata (value)
)

                              
# inspect the final table with actual climate names
View(full_data_elev_meta_eco)                                    
full_data_elev_meta_eco <- full_data_elev_meta_eco%>%             # delete column 'ecovalue'
select(-eco_values)


######################################################  CHECK UP  ########################################################

# number of observations per climate category
g6<-ggplot(full_data_elev_meta_eco,aes(x=Climate_Re,fill=species))+ 
  geom_bar(position = "fill") +                                   # bars not side by side 
  labs(
    title = "Number of Observations Per Species By Climate",      # main title
    x = "Climate Category",                                       # ecosystem type
    y = "Number of Observations"                                  # occurrences
  ) +
  scale_fill_manual(                                              # choose colours for points
    values = c("Harmonia axyridis" = "#f08800",                 # orange for Harmonia axyridis (invasive)
    "Coccinella septempunctata" = "#b50072"),                   # pink for Coccinella septempunctata (native)
    name = "Species",                                             # legend title for colours
  ) +
  theme_minimal()                                                 # clean graph style

# save 
ggsave(
  filename = "data/graph_ecosystem/observations_by_climate.png",
  plot = g6,
  width = 10, height = 7, dpi = 300
)

# check point analysis
# predominance of 'Cool Temperate Moist' consistent with Switzerland i.e. plateau and mid-altitude
# high human density in these areas hence high observation count, peak justified and consistent
# higher density of Harmonia axyridis in 'Warm Temperate' and 'Cool Temperate' zones
# consistent with the invasion and dominance of the invasive ladybird in the shared niche
# few observations in 'polar moist' (rare) and 'boreal moist' (not present in Switzerland)
# consistent as extreme zones for ladybirds and few observers
# more native ladybirds in cold zones logical as it is a generalist species compared to the invasive specialist
# see graph produced during data analysis and interpretation



######################################################  CHECK UP  ########################################################

# comparison of elevations across climate categories
g7<-ggplot(full_data_elev_meta_eco, aes(x = elevation, 
  fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +                         # density curve
  labs(
    title = "Elevation Distribution By Climate",                  # main title
    x = "Elevation (m)",                                          # x axis title
    y = "Density"                                                 # y axis title
  ) +  theme_minimal()                                            # clean graph style

ggsave(
  filename = "data/graph_ecosystem/elevation_by_climate.png",
  plot = g7,
  width = 10, height = 7, dpi = 300
)

# interpretation
# warm temperate moist match with low altitudes (< 500m) which is consistent with tplateau
# cool temperate moist match with 500-1500m which is consistent with pre-alpine and colline znes
# polar moist match with 2000-3000m which is consistent with alpine and subalpine zones
# boreal moist almost absent which is consistent with its near-absence in Switzerland
# separation between low (warm/cool temperate) and high climates (polar) ecologically coherent
# ecosystem raster is validated
