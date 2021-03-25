# load libraries ####
library(sf)
library(here)
library(tidyverse)


# input ####
csv_location <- "//hqwildstat/D$/Fine scale vegetation analysis/dbases_4modeling/IDveg_polygon100kcsv" 
csv_name <- "q42112a1"
quads <- c("q42112a1")
# list all files
files <- list.files(csv_location)[grepl(".csv",list.files(csv_location)) & grepl(quads,list.files(csv_location))]


# combine csv files
csv_list <- list()
for (i in 1:length(files)){
  file <- readr::read_csv(paste(csv_location,"/",files[i],sep=""))
  csv_list[[i]] <- file
}
csv_all <- dplyr::bind_rows(csv_list)


# save the combined dbf files
save(csv_all,file=paste(csv_location,paste("/",csv_name,"_pred_cov_dbf_all.RData",sep=""),sep=""))
# foreign::write.dbf(dbf_all,paste(dbf_location,"pred_cov_.dbf"))