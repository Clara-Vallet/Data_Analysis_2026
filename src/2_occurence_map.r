#################################################################################################################################################
#######################################################      OCCURENCE MAP      #################################################################
#################################################################################################################################################


########################################################## Loading The Packages #################################################################

# install if necessary
# install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "cowplot", "ggridges", "viridis"))
# install.packages(c("tidyterra"))

# packages loading
library(tidyverse)                                                # data manipulation and ggplot2
library(sf)                                                       # modern cartography
library(rnaturalearth)                                            # country map
library(rnaturalearthdata)                                        # geographic data for rnaturalearth
library(cowplot)                                                  # combine ggplots with ggdraw()
library(ggridges)                                                 # ridgeline plots
library(viridis)                                                  # colour gradients
library(tidyterra)                                                # plotting raster tiles
library(maptiles)        

# tell R to calculate as if everything were on a flat surface
sf_use_s2(FALSE)                                                  # avoids error messages



############################################################ Loading The Data ##################################################################

# read csv table 
data_file <- "data/full_data.csv"                                 # precise path where table is
df <- readr::read_csv(data_file, show_col_types = FALSE)          # silence data type of columns


# download Switzerland map
switzerland <- ne_countries(
  scale       = "medium",                                         # level of detail
  returnclass = "sf",                                             # spatial format
  country     = "Switzerland"                                     # targeted country
)

# bounding box around Switzerland for plot limits
swiss_bbox <- st_bbox(switzerland)

# white mask outside Switzerland
masque <- st_difference(
  st_as_sfc(st_bbox(c(xmin = 5.5, xmax = 11, ymin = 45.5, ymax = 48.5), crs = 4326)),
  st_union(switzerland)
)

# species colour palette
species_cols <- c(
  "Harmonia axyridis"         = "#FFD700",                      # yellow = invasive
  "Coccinella septempunctata" = "#1C3EAA"                       # royal blue = native
)



############################################################ Map Construction ###################################################################

# download satellite tiles
tiles <- get_tiles(
  switzerland,
  provider = "Esri.WorldImagery",                                 # satellite provider
  zoom     = 8                                                    # zoom level
)

# convert to greys (easthetic choice)
luminance  <- 0.299 * tiles[[1]] + 0.587 * tiles[[2]] + 0.114 * tiles[[3]]
tiles_bw   <- c(luminance, luminance, luminance)                  # replicate on 3 channels (R=G=B)
names(tiles_bw) <- c("red", "green", "blue")                      # name channels 


# creation of the occurence map 
graphA_map <- ggplot() +
  geom_spatraster_rgb(data = tiles_bw, alpha = 0.55) +            # satellite raster as background (grey satellite map from Terra)
  geom_sf(data = switzerland, fill = NA, colour = "black",        # draw Switzerland on the satellite background
  linewidth = 0.5) +                                              # thickness of the border
  geom_point(                                                     # occurence points
    data  = df,
    aes(x = longitude, y = latitude, colour = species),           # geographic coordinates with different color previously defined (blue/yellow)
    size  = 0.7, alpha = 0.7) +                                   # visual of point
  geom_sf(data = masque, fill = "white", colour = NA) +           # white mask that cover everything outside the country
  scale_colour_manual(
    values = species_cols,
    labels = c(
      "Harmonia axyridis"         = expression(italic("Harmonia axyridis")),
      "Coccinella septempunctata" = expression(italic("Coccinella septempunctata"))
    ),
    name = NULL                                                   # no legend title
  ) +
   guides(
    colour = guide_legend(
      override.aes = list(size = 5, alpha = 1)                    # bigger points in legend only 
    )
  ) +
  coord_sf(
    xlim = c(swiss_bbox["xmin"] - 0.1, swiss_bbox["xmax"] + 0.1), # fix map striclty to the country
    ylim = c(swiss_bbox["ymin"] - 0.1, swiss_bbox["ymax"] + 0.1),
    expand = FALSE
  ) +
  theme_void() +                                                  # remove axes and grill
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = c(-0.3, 0.95),                             # top left 
    legend.justification = c(0, 1),                               # anchor top left corner of legend
    legend.text      = element_text(size = 12, face = "italic"),  # bigger italic species names
    legend.key.size  = unit(0.8, "cm"),                           # bigger legend dots
    legend.key       = element_rect(fill = "transparent")
  )

# save map 
ggsave(
  filename = "data/grap_analysis/graphA_map.png",                 # path where stock graph
  plot = graphA_map,                     
  width = 10, height = 7, dpi = 300                               # figure resolution and dimension 
)
