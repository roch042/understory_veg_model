library(sf)
library(here)
library(tidyverse)

source_geo_dsn <- "//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_select_polygons.gdb" # geodatabase with IDFG layers to filter/manipulate
flt_layer_name <- c('q44116a1_100k_join_gmu_centroid','q46115e1_100k_join_gmu_centroid','q46115a1_100k_join_gmu_centroid','q47116a1_100k_join_gmu_centroid','q44116e1_100k_join_gmu_centroid','q46116a1_100k_join_gmu_centroid','q46116e1_100k_join_gmu_centroid','q47115a1_100k_join_gmu_centroid')
csv_join <- readr::read_csv("//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/results/species/presence/text/Units_6_10A_32A_5shrubspecies.csv")
output_folder <- "//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/results/species/presence/text/" 
output_name <- "ShrubSpeciesPresence_"

# lists feature layers available
lyrs_source_geo_dsn <- sf::st_layers(dsn = source_geo_dsn)

RESULTS <- list()
for (i in 1:length(flt_layer_name)){
  
  # reads in the feature layer you want to select from
  sel_lyr <- sf::st_read(dsn = source_geo_dsn, layer = flt_layer_name[i]) %>%
    dplyr::select(QuadPoly_ID, quad, POINT_X, POINT_Y)
  
  sel_lyr_join <- sel_lyr %>%
    dplyr::inner_join(csv_join)
  
  RESULTS[[i]] <- sel_lyr_join
  
}

RESULTS_reproj <- RESULTS
for (i in 1:length(flt_layer_name)){
RESULTS_reproj[[i]] <- st_transform(RESULTS[[i]], crs = 4326)
}

RESULTS_reproj_df <- list()
for (i in 1:length(flt_layer_name)){
  RESULTS_reproj_df[[i]] <- as.data.frame(RESULTS_reproj[[i]])
}

RESULTS_csv <- dplyr::bind_rows(RESULTS_reproj_df)

RESULTS_csv_coordinates <- list()
for (i in 1:length(flt_layer_name)){
  RESULTS_csv_coordinates[[i]] <- as.data.frame(st_coordinates(RESULTS_reproj[[i]]))
}
RESULTS_csv_coordinates <- dplyr::bind_rows(RESULTS_csv_coordinates)

RESULTS_csv_final <- dplyr::bind_cols(as.data.frame(RESULTS_csv),RESULTS_csv_coordinates) %>%
  dplyr::select(-c(POINT_X, POINT_Y, Shape))

# export to csv
readr::write_csv(RESULTS_csv_final,"Units_6_10A_32A_5shrubspecies_with_coordinates.csv")

# export to shapefile
sf::st_write(obj=flt_lyr, paste(output_folder,output_name,".shp",sep=""))

