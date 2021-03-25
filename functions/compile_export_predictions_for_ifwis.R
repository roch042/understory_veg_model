# load libraries
library(here)
library(tidyverse)

# database connections
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  database = "IFWIS_Master",
  uid = "ShinyUserInternal", # "ShinyUserInternal",
  pwd = "hurt seven sat pupil", # "hurt seven sat pupil",
  server = "164.165.105.241",
  port = "1433")

# load data
load(here(file.path("data","TAXA_translations.RData")))

# input
folder_level <- "species" # genus, species, subspecies
title <- "q42112a1" # title of the model run (usually the spatial area you are predicting over)

file_folder_presence <- here(file.path("results",folder_level,"presence","text"))
file_folder_percentcover <- here(file.path("results",folder_level,"percent_cover","text"))

files_presence <- list.files(file_folder_presence)[which(grepl(title, list.files(file_folder_presence)))]
files_percentcover <- list.files(file_folder_percentcover)[which(grepl(title, list.files(file_folder_percentcover)))]

files <- files_presence[which(files_presence %in% files_percentcover)]

X <- list()

# for(i in 1:length(files)){
for(i in 1:10){
    
  file_title <- files[i] %>%
    gsub(paste(title,"_",sep=""),"",.) %>%
    gsub("_NA_NA.csv","",.) %>%
    gsub("_NA.csv","",.) %>%
    gsub("_"," ",.)
  
  TaxonID <- TAXA_translations %>%
    dplyr::filter(ModelCode2 == file_title) %>%
    dplyr::select(TaxonID) %>%
    unlist() %>%
    c()
  
  x_pres <- readr::read_csv(file.path(file_folder_presence,files[i]),
                            col_types = cols(
                              QuadPoly_ID = col_character(),
                              Present = col_double(),
                              Prob = col_double()
                            )) %>%
    dplyr::select(QuadPoly_ID, Prob) %>%
    dplyr::rename(Presence = Prob)
  x_cov <- readr::read_csv(file.path(file_folder_percentcover,files[i]),
                           col_types = cols(
                             QuadPoly_ID = col_character(),
                             Prob = col_double()
                           )) %>%
    dplyr::select(QuadPoly_ID, Prob) %>%
    dplyr::rename(Abundance = Prob)
  
  X[[i]] <- dplyr::left_join(x_pres,x_cov) %>%
    dplyr::rename(QuadPolyID = QuadPoly_ID) %>%
    dplyr::mutate(TaxonID = TaxonID) %>%
    dplyr::select(QuadPolyID, TaxonID, Presence, Abundance)

}

X <- dplyr::bind_rows(X)

data.table::fwrite(X,"Example.csv")
# readr::write_csv(X,"Example.csv")