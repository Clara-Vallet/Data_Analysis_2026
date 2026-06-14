#################################################################################################################################################
######################################################      RIDGELINE CHART      ################################################################
#################################################################################################################################################


########################################################## Loading The Packages #################################################################

# install if necessary:
# install.packages("ggridges")

library(ggridges)                                                 # ridgeline density plots

# read csv table 
data_file <- "data/full_data.csv"                                 # precise path where table is
df <- readr::read_csv(data_file, show_col_types = FALSE)          # silence data type of columns


########################################################## Data Preparation #####################################################################

# date preparation
df <- df %>%
  mutate(
    year     = as.integer(format(as.Date(date_obs), "%Y")),       # extract year based on date
    period   = floor((year - 2001) / 3) * 3 + 2001,              # group into 3-year periods
    period_f = factor(period)                                     # convert to factor for y axis ordering
  ) %>%
  filter(year >= 2001, year <= 2026)                              # keep relevant time range

# long format for ridgeline plot
ridge_data_long <- df %>%
  select(species, period_f,
         elevation, annual_tmax_2019, NDVI_Mean_Summer_2019, annual_prec_2019) %>% # select only relevant variables
  rename(                                                         # rename for clarity
    Altitude      = elevation,                                    # elevation in meters
    Tmax          = annual_tmax_2019,                             # annual maximum temperature 2019
    NDVI          = NDVI_Mean_Summer_2019,                        # mean summer NDVI 2019
    Precipitation = annual_prec_2019                              # annual precipitation 2019
  ) %>%
  pivot_longer(                                                   # stack variables into one column
    cols      = c(Altitude, Tmax, NDVI, Precipitation),           # columns to stack
    names_to  = "variable",                                       # new column for variable names
    values_to = "value"                                           # new column for variable values
  ) %>%
  mutate(
    variable = factor(variable,                                   # fix display order
    levels = c("Altitude", "Tmax", "NDVI", "Precipitation"))      # left to right
  )


########################################################## Wilcoxon Significance Tests ##########################################################

# wilcoxon test for each period x variable combination no normality assumption because non normal data (see graph check up first part)
wilcox_results <- ridge_data_long %>%
  group_by(period_f, variable) %>%
  summarise(
    p_value = tryCatch(                                            # because some date only one species
      wilcox.test(
        value[species == "Harmonia axyridis"],
        value[species == "Coccinella septempunctata"]
      )$p.value,
      error = function(e) NA                                      # return NA if test cannot be performed 
    ),
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      is.na(p_value)  ~ "",                                       # no test possible = no star displayed
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )


########################################################## Ridgeline Construction ###############################################################

graphB_ridgeline <- ggplot(
    data = ridge_data_long,
    aes(x = value, y = period_f, fill = species)                  # x = variable value, y = time period
  ) +
  geom_density_ridges(                                            # ridgeline format
    alpha     = 0.65,                                             # transparency of ridge fill — allows overlap visibility
    scale     = 1.2,                                              # vertical overlap between time period ridges
    colour    = "white",                                          # ridge outline colour
    linewidth = 0.3                                               # ridge outline thickness
  ) +
  geom_text(                                                      # significance stars
    data        = wilcox_results,
    aes(x = Inf, y = period_f, label = stars),                    # Inf = far right of each facet panel
    inherit.aes = FALSE,
    size        = 3.2,                                            # star text size
    hjust       = 1.2,                                            # slightly inside right edge
    colour      = "grey40"                                        # star colour
  ) +
  scale_fill_manual(values = species_cols, guide = "none") +      # predefined species colours — no legend
  facet_wrap(~ variable, scales = "free_x", nrow = 1) +          # one panel per variable, independent x axis
  theme_classic(base_family = "sans") +                           # clean theme
  theme(
    axis.title.y     = element_blank(),                           # remove y axis title (time periods self-explanatory)
    axis.title.x     = element_blank(),                           # remove x axis title (facet headers serve as labels)
    axis.text.y      = element_text(size = 8, colour = "grey40"), # year labels
    axis.text.x      = element_text(size = 7, colour = "grey50"), # value axis tick labels
    strip.text       = element_text(size = 9, family = "sans"),   # facet header style
    strip.background = element_blank(),                           # no box around facet headers
    plot.background  = element_rect(fill = "white", color = NA),  # figure background colour
    panel.background = element_rect(fill = "white", color = NA),  # panel background colour
    axis.line        = element_line(colour = "black"),            # axis line colour
    axis.ticks       = element_line(colour = "grey70"),           # tick mark colour
    panel.spacing    = unit(1.5, "lines"),                        # space between facets
    legend.position  = "none"                                     # no legend
  )

# save graph 
ggsave(
  filename = "data/grap_analysis/graphB_ridgeline.png",           # output path
  plot     = graphB_ridgeline,
  width    = 10, height = 7, dpi = 300                            # dimensions and resolution
)
