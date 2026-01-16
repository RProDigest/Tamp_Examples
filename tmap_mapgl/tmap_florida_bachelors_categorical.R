#############################################################
# Mapping Bachelor's Degree Attainment in Miami-Dade County
# Using tidycensus and tmap (v4) with Custom Groupings
# Author: RProDigest
# Date: 2026 Jan 16
#############################################################



# 1. Load Required Libraries------------------------

library(tidycensus)
library(tidyverse)
library(sf)
library(classInt)
library(tmap)


# You will need a Census API key. If you don't have one, you can sign up 
# at the Census Bureau http://api.census.gov/data/key_signup.html
# Once you have your key, run the following line once to install it:
# census_api_key("YOUR_KEY_HERE", install = TRUE)
# Then restart R for the changes to take effect.


# 2. Get and Clean Data --------------------------

miami <- get_acs(
  geography = "tract",
  variables = "DP02_0068P",
  state = "FL",
  county = "Miami-Dade",
  year = 2023,
  geometry = TRUE
) %>%
  st_transform(4326) %>%
  drop_na(estimate) # Remove NAs

# 3. Create Custom Groups -----------------------------------

breaks <- classIntervals(miami$estimate, n = 5, style = "quantile")$brks

miami <- miami %>%
  mutate(
    group_label = cut(
      estimate, 
      breaks = breaks, 
      include.lowest = TRUE,
      labels = c(
        paste0("< ", round(breaks[2], 2)),
        paste0(round(breaks[2], 2), " - ", round(breaks[3], 2)),
        paste0(round(breaks[3], 2), " - ", round(breaks[4], 2)),
        paste0(round(breaks[4], 2), " - ", round(breaks[5], 2)),
        paste0(round(breaks[5], 2), "+")
      )
    )
  )

# 3. Build the Map ------------------------------------

tmap_mode("view")

tm_shape(miami) +
  tm_polygons(
    fill = "group_label",
    
    
    fill.scale = tm_scale_categorical(values = "viridis"),
    
    
    fill.legend = tm_legend(
      title = "Bachelor's Degree (%)", 
      position = tm_pos_in("left", "top") 
    ),
    
    fill_alpha = 0.7,
    col = "white",    # Border color
    lwd = 0.5,        # Border width
    popup.vars = c("Estimate" = "estimate")
  ) +
  
  
  tm_facets(by = "group_label", as.layers = TRUE) +
  
  
  tm_view(control.position = c("left", "top")) # Layer Control (Checkboxes) Top-Left



