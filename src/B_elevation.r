###########################################################################################################################
#######                               Adding Elevation Data to the Dataset                                          #######
###########################################################################################################################


############################################# Package Download #############################################################

# install if necessary
# install.packages(c("raster", "rnaturalearth", "ggplot2", "elevatr"))

library(rnaturalearth)                                            # country mapping
library(ggplot2)                                                  # create graphs
library(raster)                                                   # spatial analysis (read and manipulate rasters)
library(elevatr)                                                  # download elevation data

# tell R to calculate on a flat surface
sf_use_s2(FALSE)                                                  # avoid error message



############################################  Download the Switzerland Map   ###############################################

# download swiss map
Switzerland <- ne_countries(                                      
    scale = "medium",                                             # level of detail for borders
    returnclass = "sf",                                           # format
    country = "switzerland"                                       # target country
)   



########################################  Download Elevation Data   ########################################################

# create file to save graph of this part
dir.create("data/graph_elevation", showWarnings = FALSE)   

# download elevation satellite image covering switzerland
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)      # z is the zoom level (resolution)

# visualize elevation raster
plot(elevation_switzerland)                                       # color look correctly situated



##############################  Convert Species Coordinates Into Spatial Points   #########################################

# check composition of the species matrix
head(full_data)                                                   # species/latitude/longitude/date/source/year

# convert coordinates into spatial points
spatial_points <- SpatialPoints(
  coords = full_data[, c("longitude", "latitude")],               # extract columns of interest from table
  proj4string = CRS("+proj=longlat +datum=WGS84")                 # standard code for GPS coordinates
)

# visualize occurrences on the map of Switzerland
plot(elevation_switzerland,                                       # raster data limited to Switzerland
      main = "Species Occurrences on Ecosystem Map"               # title
)                                                                 # display raster to add occurrences on top
plot(spatial_points,                                              # GPS data
      add = TRUE,                                                 # keep raster map underneath
      pch = 16,                                                   # filled circle style
      cex = 1.2,                                                  # size of occurrence points
      col = "#b50072"                                           # colour of occurrence points
)                             



###############################  Keep Only the Elevation Data For the Occurrences   #######################################

# where is the ladybird x what elevation at that position
elevation <- raster::extract(
  elevation_switzerland,                                          # elevation raster
  spatial_points,                                                 # targets (ladybird GPS points)
)                                                                 # preserves value order (GPS point/elevation order)

# correspondence check point
identical(crs(elevation_switzerland), crs(spatial_points))        # TRUE (match)

# add elevation data to the initial table
full_data_elev <- data.frame(                                     # merge the two tables
  full_data,                                                      # base table
  elevation = elevation                                           # elevation data
)

# check NA values
sum(is.na(full_data_elev$elevation))                              # all points received an elevation value

# global check of the table after adding elevation data
summary(full_data_elev)                                           # elevation has correct numeric values (min, max, ...)

# visualise the table
View(full_data_elev)                                              # everything looks correct (base table + elevation)



#####################################################  CHECK UP  ###########################################################

# species distribution by elevation
g4<-ggplot(full_data_elev,                                        # new table with elevation column
      aes(x = species,y = elevation,fill = species)) +            # define graph axes
  geom_boxplot() +                                                # boxplot
  labs(title = "Elevation distribution by species",               # main title
       y = "Elevation (metres)", x = "Species") +                 # axis titles
  scale_fill_manual(                                              # choose colours for points
    values = c("Harmonia axyridis" = "#f08800",                 # orange for Harmonia axyridis (invasive)
    "Coccinella septempunctata" = "#b50072"),                   # pink for Coccinella septempunctata (native)
    name = "Species",                                             # legend title for colours
  ) +
  theme_minimal()                                                 # clean graph style

# save to 'graph_elevation' file
ggsave(
  filename = "data/graph_elevation/elevation_by_species.png",
  plot = g4,
  width = 10, height = 7, dpi = 300
)

# check point analysis
# data ranging from ~ 200m (low elevation Switzerland) to 3000m (mountain) consistent with swiss topography
# fewer outlier points above 2000m as expected due to harsh conditions
# more native ladybird outliers logical as it is a generalist species compared to the invasive specialist
# lower median for Harmonia axyridis and wider range for Coccinella septempunctata

# check for a real difference just by curiosity
wilcox.test(elevation ~ species, data = full_data_elev)           # non-normal distribution

# test analysis
# W = 19501862, p-value < 2.2e-16
# highly significant difference
# the species do not share the same altitudinal niche 
# logical and consistent with the literrature (Kenis et al., 2020)



##############################  Mountain/plateau Distinction Useful For Future Analysis  ##################################

# create the category column
full_data_elev$terrain_type <- ifelse(
  full_data_elev$elevation < 1000, "Plateau", "Mountain")          # 1000m is a random threshold that i chose

# convert to "factor" so R treats it as categories
full_data_elev$terrain_type <- as.factor(full_data_elev$terrain_type)

# quick check
View(full_data_elev) 
full_data_elev$elevation  <- round(full_data_elev$elevation,  3)  # round elevation to 3 decimal                                              # column added successfully
full_data_elev$latitude   <- round(full_data_elev$latitude,   3)  # round latitude to 3 decimal 
full_data_elev$longitude  <- round(full_data_elev$longitude,  3)  # round longitude to 3 decimal 


#####################################################  CHECK UP  ###########################################################

# proportions of each species by terrain type (mountain/plateau)
g5<-ggplot(full_data_elev, aes(x = terrain_type, fill = species)) +
  geom_bar(position = "dodge") +                                  # "position = dodge" places bars side by side 
  labs(title = "Number of observations by terrain type",
       y = "Number of observations",
       x = "Terrain type",
       fill = "Species") +
  scale_fill_manual(values = c("Harmonia axyridis" = "#f08800", 
                               "Coccinella septempunctata" = "#b50072")) +
  theme_minimal()

# save to 'graph_elevation' file
ggsave(
  filename = "data/graph_elevation/terrain_type_by_species.png",
  plot = g5,
  width = 10, height = 7, dpi = 300
)

# check point interpretation
# dominance of the invasive species on the plateau consistent with the literature (strong invasive competition)
# superiority of the native species in the mountains (refuge) consistent as it is a more generalist species
# everything appears consistent (Kenis et al., 2020)