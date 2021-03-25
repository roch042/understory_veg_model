library(sf)
library(here)
library(tidyverse)
library(fs)
library(leaflet)

# shp_loc_folder <- "D:/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output" 
shp_loc_folder <- "//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output"
shp_prefix <- "Unit_56_73A"

res <- dir_ls(shp_loc_folder, glob = "*.shp") %>% 
  tibble(fname = .) %>%
  mutate(data = map(fname, read_sf)) %>%
  unnest(data) %>%
  st_as_sf() 
