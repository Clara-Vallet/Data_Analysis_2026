#################################################################################################################################################
#######################################################        FINAL PLOT        ################################################################
#################################################################################################################################################


################################################################# Packages ######################################################################

# install if necessary
# install.packages(c("cowplot", "ggtext"))

library(cowplot)                                                  # combine ggplots with ggdraw()
library(ggtext)                                                   # rich text and textbox in ggplot2

# read csv table 
data_file <- "data/full_data.csv"                                 # precise path where table is
df <- readr::read_csv(data_file, show_col_types = FALSE)          # silence data type of columns


############################################################## Shared Legend ####################################################################

# legend contruction
legend_plot <- ggplot(
    df,
    aes(x = elevation, y = elevation, fill = species, color = species)) +                           # x & y does not care 
  geom_point(shape = 21, size = 4) +                                                                # filled circle with border
  scale_fill_manual(
    values = species_cols, name = NULL,                                                             # no legend title
    labels = c(                                                                                     # title species
      "Harmonia axyridis"         = expression(italic("Harmonia axyridis")),                        # name in italic
      "Coccinella septempunctata" = expression(italic("Coccinella septempunctata"))                 # name in italic
    )) +
    scale_color_manual(
    values = species_cols, name = NULL,                                                 
    labels = c(
      "Harmonia axyridis"         = expression(italic("Harmonia axyridis") ),
      "Coccinella septempunctata" = expression(italic("Coccinella septempunctata"))
    )) +
  theme(
    legend.position   = "bottom",                                                                   # legend at the bottom
    legend.key        = element_rect(fill = "transparent"),                                         # no box around legend keys
    legend.background = element_rect(fill = "white", color = NA)                                    # match figure background
  )

shared_legend <- get_legend(legend_plot)                                                            # extract legend 




############################################################ Figure Legend Text #################################################################

# justified text legend
legende_text <- ggplot() +
  geom_textbox(                                                                                   
    aes(x = 0.5, y = 0,
        label = "<b>Figure 1.</b> <i>Harmonia axyridis</i> influence in Switzerland on niche displacement of <i>Coccinella septempunctata</i>.
A. Gbif and iNat ccurrence points of <i>H. axyridis</i> (n = 7499) and <i>C. septempunctata</i> (n = 2829) across Switzerland from 2001 to 2024. 
B. Principal Component Analysis of ecological niche in 2019. Variables are Altitude, Tmax in 2019, summer NDVI and Precipitation in 2019. Ellipses represent 95% confidence intervals. Arrows indicate variable loadings.
C. Temporal dynamics of altitudinal, thermal, vegetation and precipitation niches across 3-year periods (2001–2024). Tmax and Precipitation are 2019 data. Each ridge represents the kernel density distribution of one species per period. Wilcoxon test. ns= non significant. *** = p<0.001"),
    width     = unit(0.76, "npc"),                                                                # text box width
    hjust     = 0.5,                                                                              # horizontality
    vjust     = 1,                                                                                # verticality
    box.color = NA,                                                                               # no border around box
    fill      = NA,                                                                               # no background 
    size      = 4,                                                                                # writting size
    family    = "sans"                                                                            # writting type
  ) +
  scale_x_continuous(limits = c(0, 1.2), expand = c(0, 0)) +                                      # x range
  scale_y_continuous(limits = c(-1, 0),  expand = c(0, 0)) +                                      # y range
  theme_void() +                                                                                  # no axes 
  theme(plot.margin = margin(0, 0, 0, 0))                                                         # no marges



################################################################ Final Plot #####################################################################

# combined garph
figure_finale <- ggdraw(xlim = c(0,1.5), ylim = c(-0.84,1.88)) +                                   # blank page

  draw_plot(graphA_map, x = 0.05, y = 0.65, width = 0.9, height = 0.9) +                         # occurrence map position
  draw_plot(graphC_pca, x = 0.85, y = 0.62, width = 0.55, height = 0.88) +                         # PCA graph position
  draw_plot(graphB_ridgeline, x = 0.05, y = -0.42, width = 1.4, height = 0.9) +                    # ridgeline position
draw_plot(legende_text, x = -0.03, y = -0.9, width = 1.88, height = 0.38) +

  draw_label(                                                                                      # add label A.
    "A.",
    x = 0.05, y = 1.6, hjust = 0, vjust = 1,                                                       # position    
    size = 14, fontface = "bold",                                                                  # format
    fontfamily = "sans", color = "black"                                                           # format
  ) +

  draw_label(                                                                                      # add label B
    "B.",
    x = 0.85, y = 1.6, hjust = 0, vjust = 1,                    
    size = 14, fontface = "bold",
    fontfamily = "sans", color = "black"
  ) +

  draw_label(                                                                                      # add label C.
    "C.",
    x = 0.05, y = 0.55, hjust = 0, vjust = 1,                   
   size = 14, fontface = "bold",
    fontfamily = "sans", color = "black"
  ) +

  draw_label(
  toupper("Does the Establishment of Harmonia axyridis in Switzerland Drive Niche Displacement in Coccinella septempunctata?"),
  x = 0.05, y = 1.77, hjust = 0, vjust = 1,
  size = 14, fontface = "bold",
  fontfamily = "sans", color = "black"
  ) +

  draw_label(                                                                                     # title PCA                                      
    "altitude",
    x = 1.13, y = 1.45, hjust = 0, vjust = 1,                   
   size = 11, 
    fontfamily = "sans", color = "black"
  ) +

  draw_label(                                                                                      
    "summer NDVI",
    x = 1, y = 1.54, hjust = 0, vjust = 1,                   
   size = 11, 
    fontfamily = "sans", color = "black"
  ) +

  draw_label(                                                                                     
    "precipitation",
    x = 1.2, y = 1, hjust = 0, vjust = 1,                   
   size = 11, 
    fontfamily = "sans", color = "black"
  ) +

  draw_label(                                                                                     
    "Tmax",
    x = 0.9, y = 1.23, hjust = 0, vjust = 1,                   
   size = 11, 
    fontfamily = "sans", color = "black"
  ) 


# save combined plot
ggsave(
  filename = "figure_finale.png",                                                                 # path
  plot     = figure_finale,
  width    = 14,                                                                                  # width (add to make sure correctfully visualize)
  height   = 10,                                                                                  # height (add to make sure correctfully visualize)
  dpi      = 300,                                                                                 # quality resolution
  bg       = bg_col                                                                               # background colour 
)


