# load libraries ####
library(sf)
library(here)
library(tidyverse)


# input ####
dbf_location <- "//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_output_tables" 
dbf_name <- "Units_1_6_10A"
quads <- c("q46115e1|q46115a1|q48116a1|q48116e1|q47116a1|q46116a1|q46116e1|q47115a1|q48117e1|q48117a1")
# list all files
files <- list.files(dbf_location)[grepl(".dbf",list.files(dbf_location)) & !grepl(".xml",list.files(dbf_location)) & grepl("100k",list.files(dbf_location)) & grepl(quads,list.files(dbf_location))]


# combine exported dbf files
dbf_list <- list()
for (i in 1:length(files)){
  file <- foreign::read.dbf(paste(dbf_location,"/",files[i],sep=""))
  dbf_list[[i]] <- file
}
dbf_all <- dplyr::bind_rows(dbf_list)

dbf_output_location <- "//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/data" 
# save the combined dbf files
save(dbf_all,file=paste(dbf_output_location,paste("/",dbf_name,"_pred_cov_dbf_all.RData",sep=""),sep=""))
# foreign::write.dbf(dbf_all,paste(dbf_location,"pred_cov_.dbf"))
