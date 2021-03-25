# load libraries ####
library(sf)
library(here)
library(tidyverse)


# input ####
shp_location <- "U:/GIS_IDFG.gdb" 
shp_name <- "Hunt_GameUnit"
unit_flt <- c("43","48")
output <- "//hqwildstat/D$/Fine scale vegetation analysis/idahoveg_database/GMU_output_tables/"
output_name <- "Unit_43_48"

# Read in source shapefile ####
# lyrs_shp_location <- sf::st_layers(dsn = shp_location) # show feature layers
shp <- sf::st_read(dsn = shp_location, layer = shp_name)


# Filter to portion of shapefile ####
shp_flt <- shp %>%
  dplyr::filter(NAME %in% unit_flt) %>%
  sf::st_union()


# Write to shapefile ####
sf::st_write(shp_flt, paste(output,output_name,".shp",sep=""))


