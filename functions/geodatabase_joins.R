library(sf)
library(here)
library(tidyverse)

source_geo_dsn <- "D:/Fine scale vegetation analysis/dbases_4modeling/blank_polys100k.gdb"
# source_geo_dsn <- "//hqwildstat/D$/Fine scale vegetation analysis/dbases_4modeling/blank_polys100k.gdb"
flt_geo_dsn <- "D:/Fine scale vegetation analysis/idahoveg_database/GMU.gdb"
# flt_geo_dsn <- "//hqwildstat/D$/Fine scale vegetation analysis/idahoveg_database/GMU.gdb"
output_folder <- "D:/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output" 
# output_folder <- "//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output" 
flt_layer_name <- "Unit_56_73A"

# lists feature layers available
lyrs_source_geo_dsn <- sf::st_layers(dsn = source_geo_dsn)
lyrs_flt_geo_dsn <- sf::st_layers(dsn = flt_geo_dsn) 

# reads in the feature layer you want to filter to
flt_lyr <- sf::st_read(dsn = flt_geo_dsn, layer = flt_layer_name) %>%
  dplyr::rename(GMU_Shape_Length = Shape_Length,
                GMU_Shape_Area = Shape_Area,
                GMU_Shape = Shape)

# spatial intersect q41112e1_100k_tbl_gmu
source_lyrs <- lyrs_source_geo_dsn$name
system.time(
  for(i in 1:length(source_lyrs)){
    # i <- which(source_lyrs == "q41112e1_100blk")
    print(paste("reading in feature layer",source_lyrs[i]))
    source_lyr <- sf::st_read(dsn = source_geo_dsn, layer = source_lyrs[i])
    print(paste("intersecting",source_lyrs[i],"with",flt_layer_name))
    inters <- sf::st_intersection(flt_lyr,source_lyr)
    if(nrow(inters)!=0){
      sf::st_write(inters, paste(output_folder,"/",flt_layer_name,"_intersect_",source_lyrs[i],".shp",sep=""))
    }
  }
)

# merge together


