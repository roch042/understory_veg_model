require(rgdal)
require(sp)
require(raster)
require(here)
require(tidyverse)
require(gridExtra)
require(grid)
require(viridis)

# Read in function
source(here(file.path("functions","make_centroid_map.R")))

# Spatial Points 
# unit <- readr::read_csv("C:\\Users\\eroche\\Documents\\Fine scale vegetation analysis\\idahoveg_database\\GMU_output_tables\\Units_56_73A.txt")
# unit <- readr::read_csv("D:\\Fine scale vegetation analysis\\understory_veg_model\\spatial\\GMU_output_tables\\Units_56_73A.txt")
load("D:/Fine scale vegetation analysis/understory_veg_model/data/Units_6_10A_32A_pred_cov_dbf_all.RData")

# Text files
folder <- "species" #c("genus","species","subspecies")
type <- c("presence","percent_cover") # percent_cover, presence
title <- "Units_6_10A_32A"

for(f in 1:length(folder)){
  for(t in 1:length(type)){
    
    mod_files <- list.files(here(file.path("results",folder[f],type[t],"text")))
    mod_files <- mod_files[which(grepl(title,mod_files))]
    
    for(i in 1:length(mod_files)){
      title <- gsub(".csv","",mod_files[i])
      if(title %in% gsub(".tif","",list.files(here(file.path("results",folder[f],type[t],"maps"))))){
        next
      } else {
        tiff(filename = here(file.path("results",folder[f],type[t],"maps",paste(title,".tif",sep=""))),
             width = 14, height = 8.5, unit = "in", res = 200)
        make_centroid_map(unit=dbf_all, file_name= mod_files[i], folder=folder[f], type=type[t])
        dev.off()
      }
      gc()
    } #i
    
  } #t
} #f