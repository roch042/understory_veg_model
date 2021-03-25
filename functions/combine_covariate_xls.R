# load libraries ####
library(sf)
library(here)
library(tidyverse)


# input ####
dbf_location <- "//hqwildstat/D$/Fine scale vegetation analysis/idahoveg_database/GMU_output_tables" 
dbf_name <- "Unit_43_48"
quads <- c("q43114a1|q43114e1|q43115a1|q43115e1")
# list all files
files <- list.files(dbf_location)[grepl(".dbf",list.files(dbf_location)) & !grepl(".xml",list.files(dbf_location)) & grepl("100k",list.files(dbf_location)) & grepl(quads,list.files(dbf_location))]


# combine exported dbf files
dbf_list <- list()
for (i in 1:length(files)){
  file <- foreign::read.dbf(paste(dbf_location,"/",files[i],sep=""))
  dbf_list[[i]] <- file
}
dbf_all <- dplyr::bind_rows(dbf_list)


# save the combined dbf files
save(dbf_all,file=paste(dbf_location,paste("/",dbf_name,"_pred_cov_dbf_all.RData",sep=""),sep=""))
# foreign::write.dbf(dbf_all,paste(dbf_location,"pred_cov_.dbf"))