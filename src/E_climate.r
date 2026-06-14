###########################################################################################################################
#######                              Adding Climate Data To The Dataset                                             #######
###########################################################################################################################



##############################################   Package Installation   ###################################################

# complicated installation of the chelsa package from platform and not CRAN
# devtools::install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
# install.packages("devtools", "terra")

# load packages
library(Rchelsa)                                                  # climate data
library(terra)                                                    # read & transform climate data
library(dplyr)

# install if necessary
# install.packages("lubridate") 
library(lubridate)                                                # select years only from YYYY-MM-DD



####################################   Maximum Temperature 2019, Season 2019 And 1980   ##################################

# create file to save graph of this part
dir.create("data/graph_climat", showWarnings = FALSE)  

# species to process
liste_especes <- unique(full_data_elev_meta_eco_sat$species)

# processing loop
for (sp in liste_especes) {
  
  message("--- In progress: ", sp, " ---")


# isolate data for the current species
  data_sp <- full_data_elev_meta_eco_sat[full_data_elev_meta_eco_sat$species==sp,]
  
# create the point cloud
  pts_v <- terra::vect(                                           # transform table into spatial object
    data_sp,                              
    geom = c("longitude", "latitude"),                            # GPS coordinates
    crs = "EPSG:4326")                                            # standard GPS code

# create point coordinates
  coords <- as.data.frame(
    terra::geom(pts_v)[, c("x", "y")]                             # extract latitude/longitude columns
  )   
 
# extract chelsa data for 2019 (high observation density)
tmax_r <- getChelsa(                                              # mean temperature per month over one year
  var       = "tasmax",                                           # specifically requests maximum temperature
  coords    = coords,
  startdate = as.Date("2019-01-01"),                              # 2019 as first available year with most observations
  enddate   = as.Date("2019-12-31"),   
  dataset   = "chelsa-monthly"                                    # monthly mean (more precise than annual)
)

# clean matrix to calculate mean temperature per month
tmax_mat <- as.matrix(tmax_r[,                                    # grid
    setdiff(                                                      # remove only the time name
        names(tmax_r), "time"),                                   # list of column names from chelsa results
        drop = FALSE])                                            # safety to keep correct structure

# calculate mean per point
tmax_mean_k <- colMeans(tmax_mat,                                 # mean over 12 months per point i.e. per ladybird
na.rm = TRUE)                                                     # safety to ignore if no data for a month

# inject results at the correct location in the final table
full_data_elev_meta_eco_sat$annual_tmax_2019[full_data_elev_meta_eco_sat$species == sp] <- (tmax_mean_k - 273.15)

# extract chelsa data for 1980 (start of invasion)
tmax_s <- getChelsa(                                              # mean temperature per month over one year
  var       = "tasmax",                                           # specifically requests maximum temperature
  coords    = coords,
  startdate = as.Date("1980-01-01"),                              # 1980 as start of invasion
  enddate   = as.Date("1980-12-31"),   
  dataset   = "chelsa-monthly"                                    # monthly mean (more precise than annual)
)

# clean matrix to calculate mean temperature per month
tmax_mas <- as.matrix(tmax_s[,                                    # grid
    setdiff(                                                      # remove only the time name
        names(tmax_s), "time"),                                   # list of column names from chelsa results
        drop = FALSE])                                            # safety to keep correct structure

# calculate mean per point
tmax_mean_c <- colMeans(tmax_mas,                                 # mean over 12 months per point i.e. per ladybird
na.rm = TRUE)                                                     # safety to ignore if no data for a month

# inject results at the correct location in the final table
full_data_elev_meta_eco_sat$annual_tmax_1980[full_data_elev_meta_eco_sat$species == sp] <- (tmax_mean_c - 273.15)

# calculate for winter (assumes order corresponds to month)
full_data_elev_meta_eco_sat$Winter_Tmax_2019[                      # winter
  full_data_elev_meta_eco_sat$species == sp] <- (rowMeans(tmax_mat[,# mean of rows 12, 1 and 2
  c(12, 1, 2)], na.rm = TRUE) - 273.15)                           # subtraction as values are in kelvin

# calculate for spring (assumes order corresponds to month)
full_data_elev_meta_eco_sat$Spring_Tmax_2019[                     # spring
  full_data_elev_meta_eco_sat$species == sp] <- (rowMeans(tmax_mat[,# mean of rows 3, 4, 5
  c(3, 4, 5)], na.rm = TRUE) - 273.15)                            # subtraction as values are in kelvin

# calculate for summer (assumes order corresponds to month)
full_data_elev_meta_eco_sat$Summer_Tmax_2019[                     # summer
  full_data_elev_meta_eco_sat$species == sp] <- (rowMeans(tmax_mat[,# mean of rows 6, 7, 8
  c(6, 7, 8)], na.rm = TRUE) - 273.15)                            # subtraction as values are in kelvin

# calculate for autumn (assumes order corresponds to month)
full_data_elev_meta_eco_sat$Automne_Tmax_2019[                    # autumn
  full_data_elev_meta_eco_sat$species == sp] <- (rowMeans(tmax_mat[,# mean of rows 9, 10, 11
  c(9, 10, 11)], na.rm = TRUE) - 273.15)                          # subtraction as values are in kelvin

  cat("Tmax processing completed for:", sp, "\n")

}

# check the table
full_data_elev_meta_eco_sat -> full_data_elev_meta_eco_sat_temp
View(full_data_elev_meta_eco_sat_temp)                             # columns successfully added



#####################################################  CHECK UP  #########################################################

# temperature-elevation correlation
png("data/graph_climat/tmax2019_vs_elevation.png", width = 1000, height = 700)
plot(full_data_elev_meta_eco_sat_temp$elevation,                   # correlation
full_data_elev_meta_eco_sat_temp$annual_tmax_2019,
     main = "Check: Temperature Vs Elevation",                     # main title
     xlab = "Elevation (m)", ylab = "Tmax 2019 (°C)",              # axis titles
     pch = 16, col = rgb(0,0,0,0.3))                               # visualisation of observation points
abline(lm(annual_tmax_2019 ~ elevation,                            # add line representing correlation trend
data = full_data_elev_meta_eco_sat_temp), col="red")               # red line
dev.off()

# interpretation
# negative trend curve fully consistent as temperature decreases with altitude
# dense cluster at 500m consistent as many observations on the plateau (residential area)
# scatter at altitude logical as more microclimates than on plateau (e.g. mountain slopes)
# no incoherent outliers (summary(full_data_elev_meta_eco$annual_tmax_2019) gives min: -3 and max: 18 as annual mean)
# everything appears consistent



#####################################################  CHECK UP  #########################################################

# comparison 1980 vs 2019
png("data/graph_climat/tmax2019_vs_tmax1980.png", width = 1000, height = 700)
plot(full_data_elev_meta_eco_sat_temp$annual_tmax_1980,            # correlation
full_data_elev_meta_eco_sat_temp$annual_tmax_2019,
     main = "Temporal Stability: 1980 Vs 2019",                    # main title
     xlab = "Tmax 1980", ylab = "Tmax 2019")                       # axis titles
abline(0, 1, col="blue")                                           # status quo
dev.off()

# interpretation
# for the same location maximum temperature in 2019 is higher than in 1980, consistent with climate warming
# everything appears consistent



##############################################   Future Temperature 2050   ################################################

# processing loop by species
liste_especes <- unique(full_data_elev_meta_eco_sat_temp$species)  # species list

for (sp in liste_especes) {
  
  message("--- Future 2050 processing for: ", sp, " ---")          # message to display to track progress
  
# isolate species coordinates
data_sp <- full_data_elev_meta_eco_sat_temp[                       # retrieve data for the relevant species
  full_data_elev_meta_eco_sat_temp$species == sp, ]
coords_t <- data.frame(lon = data_sp$longitude,                    # retrieve lat/long coordinates
lat = data_sp$latitude)
  
# prepare list for the 12 months
tas_fut_list <- vector("list", 12)
  
# download monthly data for 2050
  for (m in 1:12) {                                                # loop to process each month
    tas_m <- getChelsa(                                            # retrieve temperature data
      var     = "tas",                                             # mean air temperature
      coords  = coords_t,                                          # coordinates
      date    = as.Date(sprintf("2050-%02d-01", m)),               # m=1 creates 2050-01-01 ... m=12 creates 2050-12-01
      dataset = "chelsa-climatologies", 
      ssp     = "ssp126",                                          # scenario where humanity reduces emissions (greenest)
      forcing = "MPI-ESM1-2-HR")                                   # future calculation
    tas_fut_list[[m]] <- as.numeric(                               # take predicted temperature (numeric)
      tas_m[1, -which(names(tas_m) == "time")])}                   # remove time column which is not needed

  
# calculate annual mean
tas_fut_mat <- do.call(cbind, tas_fut_list)                        # matrix GPS points (rows) x months (columns)
tas_fut_annual_c <- rowMeans(tas_fut_mat) - 273.15                 # convert to celsius
  
# inject into main table
full_data_elev_meta_eco_sat_temp$annual_tas_2050[
  full_data_elev_meta_eco_sat_temp$species==sp]<-tas_fut_annual_c
  
  cat("Processing completed for:", sp, "\n")                       # short completion message to track progress
}

# quick check
View(full_data_elev_meta_eco_sat_temp)                             # column successfully added



#####################################################  CHECK UP  #########################################################

# temperature-elevation correlation
png("data/graph_climat/tmax2050_vs_elevation.png", width = 1000, height = 700)
plot(full_data_elev_meta_eco_sat_temp$elevation,                       # correlation
full_data_elev_meta_eco_sat_temp$annual_tas_2050,
     main = "Check: Temperature Vs Elevation",                     # main title
     xlab = "Elevation (m)", ylab = "Tmax 2050 (°C)",              # axis titles
     pch = 16, col = rgb(0,0,0,0.3))                               # visualisation of observation points
abline(lm(annual_tas_2050 ~ elevation,                             # add line representing correlation trend
data = full_data_elev_meta_eco_sat_temp), col="red")                   # red line
dev.off()

# interpretation
# negative trend curve fully consistent as temperature decreases with altitude
# dense cluster at 500m consistent as many observations on the plateau (residential area)
# scatter at altitude logical as more microclimates than on plateau (e.g. mountain slopes)
# no outliers (summary(full_data_elev_meta_eco_temp$annual_tas_2050) gives min: -5 and max: 14 as annual mean)
# everything appears consistent



#################################################   Precipitation 2019   #################################################

# species to process
liste_especes <- unique(full_data_elev_meta_eco_sat_temp$species)

# processing loop
for (sp in liste_especes) {
  
  message("--- In progress: ", sp, " ---")


# isolate data for the current species
  data_sp <- full_data_elev_meta_eco_sat_temp[full_data_elev_meta_eco_sat_temp$species==sp,]

# create the point cloud
  prec <- terra::vect(                                            # transform table into spatial object
    data_sp,                              
    geom = c("longitude", "latitude"),                            # GPS coordinates
    crs = "EPSG:4326")                                            # standard GPS code
 
# create point coordinates
  prec_coords <- as.data.frame(
    terra::geom(prec)[, c("x", "y")]                              # extract latitude/longitude columns
  )   

# extract precipitation data
prec_r <- getChelsa(
  var       = "pr",                                               # requests precipitation data
  coords    = prec_coords,                                        # GPS points of native ladybirds
  startdate = as.Date("2019-01-01"),                              # date choice justified in temperature script
  enddate   = as.Date("2020-01-01"),
  dataset   = "chelsa-monthly"                                    # monthly mean (more precise than annual)
)

# clean matrix to calculate monthly mean
prec_mat <- as.matrix(prec_r[,                                    # grid
    setdiff(                                                      # remove only the time name
        names(prec_r), "time"),                                   # list of column names from chelsa results
        drop = FALSE])                                            # safety to keep correct structure

# calculate mean per point
prec_mean <- colMeans(prec_mat,                                   # mean over 12 months per point i.e. per ladybird
na.rm = TRUE)                                                     # safety to ignore if no data for a month

# inject results at the correct location in the final table
full_data_elev_meta_eco_sat_temp$annual_prec_2019[full_data_elev_meta_eco_sat_temp$species == sp] <- prec_mean

# extract chelsa data for 1980 (start of invasion)
prec_s <- getChelsa(                                              # mean temperature per month over one year
  var       = "pr",                                               # specifically requests maximum temperature
  coords    = prec_coords,
  startdate = as.Date("1980-01-01"),                              # 1980 as start of invasion
  enddate   = as.Date("1980-12-31"),   
  dataset   = "chelsa-monthly"                                    # monthly mean (more precise than annual)
)

# clean matrix to calculate mean temperature per month
prec_mas <- as.matrix(prec_s[,                                    # grid
    setdiff(                                                      # remove only the time name
        names(prec_s), "time"),                                   # list of column names from chelsa results
        drop = FALSE])                                            # safety to keep correct structure

# calculate mean per point
prec_mean_c <- colMeans(prec_mas,                                 # mean over 12 months per point i.e. per ladybird
na.rm = TRUE)                                                     # safety to ignore if no data for a month

# inject results at the correct location in the final table
full_data_elev_meta_eco_sat_temp$annual_prec_1980[full_data_elev_meta_eco_sat_temp$species == sp] <- prec_mean_c

# calculate for winter (assumes order corresponds to month)
full_data_elev_meta_eco_sat_temp$Winter_prec_2019[                    # winter
  full_data_elev_meta_eco_sat_temp$species == sp] <- 
  (rowMeans(prec_mat[,c(12, 1, 2)], na.rm = TRUE))                # mean of rows 12, 1 and 2

# calculate for spring (assumes order corresponds to month)
full_data_elev_meta_eco_sat_temp$Spring_prec_2019[                    # spring
  full_data_elev_meta_eco_sat_temp$species == sp] <- 
  (rowMeans(prec_mat[,c(3, 4, 5)], na.rm = TRUE))                 # mean of rows 3, 4, 5

# calculate for summer (assumes order corresponds to month)
full_data_elev_meta_eco_sat_temp$Summer_prec_2019[                    # summer
  full_data_elev_meta_eco_sat_temp$species == sp] <- 
  (rowMeans(prec_mat[,c(6, 7, 8)], na.rm = TRUE))                 # mean of rows 6, 7, 8

# calculate for autumn (assumes order corresponds to month)
full_data_elev_meta_eco_sat_temp$Automne_prec_2019[                   # autumn
  full_data_elev_meta_eco_sat_temp$species == sp] <-   
  (rowMeans(prec_mat[, c(9, 10, 11)], na.rm = TRUE))              # mean of rows 9, 10, 11

  cat("Tmax processing completed for:", sp, "\n")
}

# check the table
full_data_elev_meta_eco_sat_temp -> full_data_elev_meta_eco_sat_temp_prec
View(full_data_elev_meta_eco_sat_temp_prec)

# round columns to 3 decimal (long but okay)
full_data_elev_meta_eco_sat_temp_prec$annual_tmax_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$annual_tmax_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$annual_tmax_1980  <- round(full_data_elev_meta_eco_sat_temp_prec$annual_tmax_1980,  3)
full_data_elev_meta_eco_sat_temp_prec$annual_tas_2050   <- round(full_data_elev_meta_eco_sat_temp_prec$annual_tas_2050,   3)
full_data_elev_meta_eco_sat_temp_prec$Winter_Tmax_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$Winter_Tmax_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$Spring_Tmax_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$Spring_Tmax_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$Summer_Tmax_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$Summer_Tmax_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$Automne_Tmax_2019 <- round(full_data_elev_meta_eco_sat_temp_prec$Automne_Tmax_2019, 3)
full_data_elev_meta_eco_sat_temp_prec$annual_prec_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$annual_prec_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$annual_prec_1980  <- round(full_data_elev_meta_eco_sat_temp_prec$annual_prec_1980,  3)
full_data_elev_meta_eco_sat_temp_prec$Winter_prec_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$Winter_prec_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$Spring_prec_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$Spring_prec_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$Summer_prec_2019  <- round(full_data_elev_meta_eco_sat_temp_prec$Summer_prec_2019,  3)
full_data_elev_meta_eco_sat_temp_prec$Automne_prec_2019 <- round(full_data_elev_meta_eco_sat_temp_prec$Automne_prec_2019, 3)



#####################################################  CHECK UP  #########################################################

# precipitation-elevation correlation
png("data/graph_climat/prec2019_vs_elevation.png", width = 1000, height = 700)
plot(full_data_elev_meta_eco_sat_temp_prec$elevation,                  # correlation
full_data_elev_meta_eco_sat_temp_prec$annual_prec_2019,
     main = "Check: Precipitation Vs Elevation",                   # main title
     xlab = "Elevation (m)", ylab = "Prec 2019 (mm)",              # axis titles
     pch = 16, col = rgb(0,0,0,0.3))                               # visualisation of observation points
abline(lm(annual_prec_2019 ~ elevation,                            # add line representing correlation trend
data = full_data_elev_meta_eco_sat_temp_prec), col="red")              # red line
dev.off()

# interpretation
# positive trend curve (more precipitation at altitude) consistent with orographic effect
# i.e. mountains force moist air masses to rise, cool and condense
# dense cluster at 500m consistent as many observations on the plateau (residential area)
# scatter at altitude logical as more microclimates than on plateau (e.g. mountain slopes)
# no incoherent outliers (summary(full_data_elev_meta_eco_temp_prec$annual_prec_2019))
# everything appears consistent



#####################################################  CHECK UP  #########################################################

# temperature-elevation correlation
png("data/graph_climat/prec1980_vs_elevation.png", width = 1000, height = 700)
plot(full_data_elev_meta_eco_sat_temp_prec$elevation,              # correlation
full_data_elev_meta_eco_sat_temp_prec$annual_prec_1980,
     main = "Check: Precipitation Vs Elevation",                   # main title
     xlab = "Elevation (m)", ylab = "Prec 1980 (mm)",              # axis titles
     pch = 16, col = rgb(0,0,0,0.3))                               # visualisation of observation points
abline(lm(annual_prec_2019 ~ elevation,                            # add line representing correlation trend
data = full_data_elev_meta_eco_sat_temp_prec), col="red")          # red line
dev.off()

# interpretation fully similar to 2019
# everything appears consistent



###########################################   Future Precipitation 2050   ################################################

# processing loop by species
liste_especes <- unique(full_data_elev_meta_eco_sat_temp_prec$species)  # species list

for (sp in liste_especes) {
  
  message("--- Future 2050 processing for: ", sp, " ---")          # track progress
  
# isolate species coordinates
data_sp_p <- full_data_elev_meta_eco_sat_temp_prec[                # retrieve data for the relevant species
  full_data_elev_meta_eco_sat_temp_prec$species == sp, ]
coords_t_p <- data.frame(lon = data_sp_p$longitude,                # retrieve lat/long coordinates
lat = data_sp_p$latitude)
  
# prepare list for the 12 months
pr_fut_list <- vector("list", 12)
  
# download monthly data for 2050
  for (m in 1:12) {                                                # loop to process each month
    pr_m <- getChelsa(                                             # retrieve temperature data
      var     = "pr",                                              # mean prec
      coords  = coords_t_p,                                        # coordinates
      date    = as.Date(sprintf("2050-%02d-01", m)),               # m=1 creates 2050-01-01 ... m=12 creates 2050-12-01
      dataset = "chelsa-climatologies", 
      ssp     = "ssp126",                                          # scenario where humanity reduces emissions (greenest)
      forcing = "MPI-ESM1-2-HR")                                   # future calculation
    pr_fut_list[[m]] <- as.numeric(                                # take predicted temperature (numeric)
      pr_m[1, -which(names(pr_m) == "time")])}                     # remove time column which is not needed

  
# calculate annual mean
pr_fut_mat <- do.call(cbind, pr_fut_list)                          # matrix GPS points (rows) x months (columns)
pr_fut_annual_c <- rowMeans(pr_fut_mat)                           
  
# inject into main table
full_data_elev_meta_eco_sat_temp_prec$annual_prec_2050[
  full_data_elev_meta_eco_sat_temp_prec$species==sp]<-pr_fut_annual_c
  
  cat("Processing completed for:", sp, "\n")                       # short completion message to track progress
}

# quick check
View(full_data_elev_meta_eco_sat_temp_prec)                        # column successfully added
full_data_elev_meta_eco_sat_temp_prec$annual_prec_2050  <- round(full_data_elev_meta_eco_sat_temp_prec$annual_prec_2050,  3)



#####################################################  CHECK UP  #########################################################

# prec-elevation correlation
png("data/graph_climat/prec2050_vs_elevation.png", width = 1000, height = 700)
plot(full_data_elev_meta_eco_sat_temp_prec$elevation,              # correlation
full_data_elev_meta_eco_sat_temp_prec$annual_prec_2050,
     main = "Check: Precipitation Vs Elevation",                   # main title
     xlab = "Elevation (m)", ylab = "Precipitation",               # axis titles
     pch = 16, col = rgb(0,0,0,0.3))                               # visualisation of observation points
abline(lm(annual_prec_2050 ~ elevation,                            # add line representing correlation trend
data = full_data_elev_meta_eco_sat_temp_prec), col="red")          # red line
dev.off()

# positive trend curve consistent with orographic effect 
# dense cluster at 300-800m consistent as many observations on the plateau (human residence area)
# scatter at altitude logical as highly variable in mountains 
# outliers at high altitude with elevated precipitation (~250mm at 3000m) consistent with humid alpine zones 
# values range 80-250mm consistent with monthly swiss precipitation
# everything appears consistent