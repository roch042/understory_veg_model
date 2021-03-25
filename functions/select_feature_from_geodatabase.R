library(sf)
library(here)
library(tidyverse)

source_geo_dsn <- "U:/GIS_IDFG.gdb" # geodatabase with IDFG layers to filter/manipulate
flt_geo_dsn <- "//hqwildstat/D$/Fine scale vegetation analysis/idahoveg_database/GMU.gdb" # geodatabase to store filtered results
flt_layer_name <- "Hunt_GameUnit"
flt_object_name <- c(43,48)
output_folder <- "//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/" 
output_name <- "Unit_43_48"

# lists feature layers available
lyrs_source_geo_dsn <- sf::st_layers(dsn = source_geo_dsn)

# reads in the feature layer you want to filter to
flt_lyr <- sf::st_read(dsn = source_geo_dsn, layer = flt_layer_name) %>%
  dplyr::filter(NAME %in% flt_object_name) %>%
  dplyr::rename(GMU_Shape_Length = Shape_Length,
                GMU_Shape_Area = Shape_Area,
                GMU_Shape = Shape)

# export to shapefile
sf::st_write(obj=flt_lyr, paste(output_folder,output_name,".shp",sep=""))
