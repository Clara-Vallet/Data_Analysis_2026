###########################################################################################################################
#######                                Combining iNaturalist and GBif Data                                          #######
###########################################################################################################################


############################################# Package Download #############################################################

# install if necessary
# install.packages(c("rgbif", "rnaturalearth", "ggplot2", "rinat","raster", "dplyr", "sf"))

# packages loading 
library(rgbif)                                                    # access to GBIF data
library(rnaturalearth)                                            # country map
library(ggplot2)                                                  # create graphs
library(rinat)                                                    # access to iNaturalist data
library(raster)                                                   # spatial analysis
library(dplyr)                                                    # data manipulation
library(sf)                                                       # modern cartography

# tell R to calculate as if everything were on a flat surface
sf_use_s2(FALSE)                                                  # avoids error messages


#####################################################  Map: Switzerland   ##################################################

# download Switzerland map
Switzerland <- ne_countries(
  scale = "medium",                                               # level of detail
  returnclass = "sf",                                             # format
  country = "Switzerland"                                         # targeted country
)

# preliminary visualisation
print(
ggplot(data = Switzerland) +                                      
  geom_sf(fill = "grey95", color = "black") +                     # map background in grey & border in black
  theme_classic())                                                # clean graph without grid lines



################################################## Processing Loop #########################################################

# list of species to treat
liste <- c("Coccinella septempunctata", "Harmonia axyridis")      # choice of species to study
resultats <- list()                                               # place to store the data

# create file to save graph of this part
dir.create("data/graph_data", showWarnings = FALSE)               

# processing loop
for (nom in liste) {                                              # create a loop
  
  message("--- In progress: ", nom, " ---")


# processing for GBIF data
  gbif_raw <- occ_data(                                           # download GBIF data
    scientificName = nom,                                         # one species at a time from predefined list
    hasCoordinate = TRUE,                                         # filter only occurrences with geographic data
    limit = 5000,                                                 # max number of occurrences to download (5000)
    basisOfRecord = "HUMAN_OBSERVATION",                          # avoid museum specimens for example
    country = "CH")$data                                          # select only occurences swiss

# transform GBIF points into spatial objects
  data_spatiale_gbif <- st_as_sf(
    gbif_raw,                                                     # raw data with points also going beyond borders
    coords = c("decimalLongitude", "decimalLatitude"),            # point coordinates
    crs = 4326                                                    # standard GPS format (similar to the map)
  )

# keep only GBIF points within the Sswiss borders
  data_suisse_gbif <- st_intersection(data_spatiale_gbif, Switzerland)

# visualize GBIF points on the map of switzerland
  g1<- ggplot(data = Switzerland) +                               # create map background (Switzerland)
  geom_sf(fill = "grey95", color = "black") +                     # map background in grey & border in black
  geom_sf(                                                        # add occurrence points to the map
    data = data_suisse_gbif,                                      # data source
    size = 3,                                                     # point size
    shape = 21,                                                   # circle point with border
    fill = "#b50072",                                           # point colour
    color = "white"                                               # point border colour
  ) +
  labs(                                                           # add titles
    title= paste("Distribution", nom, "(Gbif)"),                  # main title
    x = "Longitude",                                              # x axis title
    y = "Latitude"                                                # y axis title
  ) +
  theme_classic() 

# save in data file 'graph'
  ggsave(
    filename = paste0("data/graph_data/", 
    gsub(" ", "_", nom), "_GBIF.png"),                            # path
    plot = g1,                                                    # plot to save
    width = 10, height = 7, dpi = 300                             # dimension and resolution
  )

# create the GBIF data frame
  coords_gbif <- st_coordinates(data_suisse_gbif)                 # extract GPS coordinates
  data_gbif <- data.frame(
    species   = nom,                                              # species name
    latitude  = coords_gbif[, "Y"],                               # latitude (X)
    longitude = coords_gbif[, "X"],                               # longitude (Y)
    date_obs  = as.Date(data_suisse_gbif$eventDate),              # simple date (D-M-Y) without time
    source    = "gbif"                                            # data source (GBIF)
  )

# check the GBIF data frame
  summary(data_gbif)

# process iNaturalist data
  inat_raw <- get_inat_obs(
    maxresults = 5000,                                            # limit the number of occurrences downloaded
    query = nom,                                                  # species name
    place_id = "switzerland",                                     # restrict located in Switzerland
)

# transform iNaturalist points into spatial objects
  data_spatiale_inat <- st_as_sf(
  inat_raw,                                                       # data going also beyond borders
  coords = c("longitude", "latitude"),                            # point coordinates
  crs = 4326)                                                     # standard GPS format (similar to the map)

# keep only iNaturalist points within the swiss borders
  data_suisse_inat <- st_intersection(data_spatiale_inat, Switzerland)   

# visualize iNaturalist points on the map of Switzerland                                                           
  g2<-ggplot(data = Switzerland) +                                  # create the map background
  geom_sf(fill = "grey95", color = "black") +                     # map background in grey & border in black
  geom_sf(                                                        # add occurrence points to the map
    data = data_suisse_inat,                                      # data source
    size = 3,                                                     # point size
    shape = 21,                                                   # circle point with border
    fill = "#ff6cc9",                                           # point colour
    color = "white"                                               # point border colour
  ) +
  labs(                                                           # add titles
    title=paste("Distribution", nom, "(iNaturalist)"),            # main title
    x = "Longitude",                                              # x axis title
    y = "Latitude")+                                              # y axis title
 theme_classic()                                                  # clean graph without gridlines
  

# save to data file 'graph'
  ggsave(
    filename = paste0("data/graph_data/", 
    gsub(" ", "_", nom), "_iNat.png"),                            # path
    plot = g2,                                                    # plot to save
    width = 10, height = 7, dpi = 300                             # dimension and resolution
  )

# create the iNaturalist data frame
  coords_inat <- st_coordinates(data_suisse_inat)                 # extract GPS coordinates
  data_inat <- data.frame(
    species   = nom,                                              # species name
    latitude  = coords_inat[, "Y"],                               # latitude (X)
    longitude = coords_inat[, "X"],                               # longitude (Y)
    date_obs  = as.Date(data_suisse_inat$observed_on),            # simple date (D-M-Y) without time
    source    = "inat"                                            # data source (iNat)
)

# check GBIF data frame
  summary(data_inat)

# store species data frames in 'resultats'
  resultats[[nom]] <- rbind(data_gbif, data_inat)

  message("--- Done: ", nom, " ---")
}



########################################## Combine Into One Large Data Frame ###############################################

# create final data frame by combining all 
full_data <- bind_rows(resultats)                                 # 'bind_rows' (row) VS 'merge' (add column)

# check the global data frame
head(full_data)                                                   # display the first 6 rows of the data frame
table(full_data$source, useNA = "ifany")                          # count N(gbif)=7337, N(iNat)=4190  & NA=0

# check species
table(full_data$species)                                          # no parasitic species (e.g. butterfly)

# check dates
summary(full_data$date_obs)                                       # wide time range (1880-2026) for temporal study

# remove observations with missing dates to avoid analysis issues (N=210)
full_data <- full_data %>% filter(!is.na(date_obs))
summary(full_data$date_obs)                                       # NA=0

# remove duplicates as GBIF often syncs data with iNat (N=10)
full_data_unique <- full_data %>%
  distinct(species,latitude,longitude,date_obs,.keep_all = TRUE)
table(full_data$source, useNA = "ifany")                          # count N(gbif)=7129, N(iNat)=4188  & NA=0




#####################################################  CHECK UP  ###########################################################

# plot GBIF & iNat observations in Switzerland                                                          
g3<-ggplot(data = Switzerland) +                                  # create the map background (Switzerland)
  geom_sf(fill = "grey95", color = "black") +                     # map background in grey & border in black
  geom_point(                                                     # add occurrence points to the map
    data = full_data,                                             # data source
    aes(x = longitude, y = latitude, fill = species),             # points on longitude/latitude axes
    size = 3,                                                     # point size
    shape = 21,                                                   # circle point with border
    color = "white",                                              # point border colour
    alpha = 0.8
  ) +
  scale_fill_manual(                                              # choose colours for iNat & GBIF points
    values = c("Harmonia axyridis" = "#f08800",                 # orange for Harmonia axyridis
    "Coccinella septempunctata" = "#b50072"),                   # pink for Coccinella septempunctata
    name = "Species",                                             # legend title for colours
  ) +
  labs(                                                           # add titles
    title="Distribution Coccinella septempunctata & Harmonia axyridis (GBIF & iNat)",# main title
    x = "Longitude",                                              # x axis title
    y = "Latitude",                                               # y axis title
  ) +
  theme_classic()                                                 # clean graph style without gridlines

# save to 'graph'
ggsave(
   filename = "data/graph_data/Combined_distribution.png",        # path
    plot = g3,                                                    # plot to save
    width = 10, height = 7, dpi = 300                             # dimension and resolution
  )