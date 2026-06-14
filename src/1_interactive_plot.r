#################################################################################################################################################
##################################################      INTERACTIVE LEAFLET MAP      ############################################################
#################################################################################################################################################


############################################################# Loading The Packages #############################################################

# install if necessary
# install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "leaflet", "htmlwidgets"))

library(tidyverse)                                                                  # data manipulation
library(sf)                                                                         # spatial data manipulation
library(rnaturalearth)                                                              # country map polygons
library(rnaturalearthdata)                                                          # geographic data for rnaturalearth
library(leaflet)                                                                    # interactive web
library(htmlwidgets)                                                                # export interactive map



############################################################# Loading The Data #################################################################

data_file <- "data/full_data.csv"                                                   # path to file
df        <- readr::read_csv(data_file, show_col_types = FALSE)                     # silence column type message

# species colour palette
pal <- colorFactor(
  palette = c(
    "Harmonia axyridis"         = "#FFD700",                                      # yellow = invasive
    "Coccinella septempunctata" = "#1C3EAA"                                       # royal blue = native
  ),
  domain = df$species                                                               # possible values of the variable
)

############################################################# Switzerland Map ##################################################################

switzerland <- ne_countries(                                                       # load Swiss polygon from Natural Earth database
  country     = "Switzerland",                                                     # targeted country
  scale       = "medium",                                                          # level of detail
  returnclass = "sf"                                                               # sf spatial format
)

swiss_bbox <- st_bbox(switzerland)                                                 # plot limits



############################################################# Tooltip Construction #############################################################

df <- df %>%
  mutate(
    popup = paste0(
      "<b><i>", species, "</i></b><br>",                                           # species name in bold italic
      "<hr style='margin:4px 0; border-color:#ddd'>",                              # horizontal separator
      "<b>Altitude :</b> ",elevation, " m<br>",                                    # elevation in meters
      "<b>Temperature maximal 2019 :</b> ",round(annual_tmax_2019, 1), " °C<br>",  # max temperature
      "<b>Annual Precipitation 2019 :</b> ",round(annual_prec_2019,0)," mm<br>",   # precipitation
      "<b>Summer NDVI 2019 :</b> ", round(NDVI_Mean_Summer_2019, 3), "<br>",       # vegetation index
      "<b>Landform :</b> ", Landforms, "<br>",                                     # terrain type
      "<b>Land cover :</b> ", Landcover, "<br>",                                   # land cover type
      "<b>Date :</b> ", date_obs, "<br>",                                          # observation date
      "<b>Source :</b> ", toupper(source), "<br>",                                 # data source in uppercase
      "<b>Coordinates :</b>", round(latitude, 3),"°N,",round(longitude, 3),"°E"    # coordinates
    )
)



############################################################# Map Construction #################################################################

# interactive map construction
carte <- leaflet(df) %>%                                                           # map

  addProviderTiles(provider = providers$CartoDB.DarkMatter, group = "Dark") %>%    # dark 
  addProviderTiles(provider = providers$OpenTopoMap,  group = "Topo") %>%          # topographic
  addProviderTiles(provider = providers$Esri.WorldImagery,group = "Satellite") %>% # satellite 

  addPolygons(
    data        = switzerland,
    fillColor   = "transparent",                                                    # border only
    color       = "#ffffff",                                                      # white border
    weight      = 1.2,                                                              # thickness (pix)
    fillOpacity = 0                                                                 # transparent fill
  ) %>%

  addCircleMarkers(
    data        = df %>% filter(species == "Harmonia axyridis"),                    # invasive species only
    lng         = ~longitude,                                                       # longitude 
    lat         = ~latitude,                                                        # latitude 
    radius      = 5,                                                                # circle (pix)
    color       = "rgba(0,0,0,0.4)",                                              # border (half transparent)
    weight      = 0.8,                                                              # thickness
    fillColor   = "#FFD700",                                                      # yellow intérieur
    fillOpacity = 0.8,
    label       = ~lapply(popup, htmltools::HTML),                                  # HTML 
    group       = "Harmonia axyridis (invasive)"                                    # layer group 
  ) %>%

  addCircleMarkers(
    data        = df %>% filter(species == "Coccinella septempunctata"),            # same but for native species 
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 5,
    color       = "rgba(0,0,0,0.4)",
    weight      = 0.8,
    fillColor   = "#1C3EAA",                                                      # blue fill
    fillOpacity = 0.8,
    label       = ~lapply(popup, htmltools::HTML),                                  
    group       = "Coccinella septempunctata (native)"
  ) %>%

  addLegend(
    position = "bottomright",                                                      # legend position
    pal      = pal,                                                                # colour 
    values   = ~species,                                                           # fit species
    title    = "<b>Species</b>",                                                   # legend title
    opacity  = 0.9
  ) %>%

  addLayersControl(
    baseGroups    = c("Dark", "Topo", "Satellite"),                                # map one by one
    overlayGroups = c(
      "Harmonia axyridis (invasive)",
      "Coccinella septempunctata (native)"
    ),
    options = layersControlOptions(collapsed = FALSE)                             # always visible
  ) %>%

  setView(lng = 8.3, lat = 46.8, zoom = 8)                                        # focs on Switzerland


############################################################# Export ###########################################################################

# create file to save graph of this part
dir.create("data/grap_analysis", showWarnings = FALSE)   

saveWidget(
  widget        = carte,
  file          = "data/grap_analysis/coccinelles_leaflet.html",                  # path
  selfcontained = FALSE,                                                          # HTML + resource folder
  title         = "Coccinella vs Harmonia — Interactive map Switzerland"
)

browseURL("data/grap_analysis/coccinelles_leaflet.html")                          # open map in the default browser
