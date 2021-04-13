# # # # # # # # # # # # # # # # # # # # # 
## Exploring Metadata creation with R ##
## Date: 4/13/2021
## By: Rob Ritson
# # # # # # # # # # # # # # # # # # # # #
#Load Library
#install.packages("dataMeta")
library(dataMeta) #package for creating and appending a data dictionary to an R dataset

#Load Data
load("C:/Users/rritson/Documents/cpnwh_data.RData") #data file

##Building a Data Dictionary## 
#Create Linker data frame
names<-colnames(cpnwh.data) #name of variables in dataframe
var_type<-c(0,1,0,0,0,1,0,0,0) #list of variable type, value 1 or 0; 1 is for variables with list of options, 0 for range of values (numbers)
var_desc<-c("Unique identifier of observation","Name of Herbarium housing the speciman","Year the speciman was collected",
            "Latitudinal coordinate of location in decimal degress","Longitudinal coordinate of location in decimal degrees",
            "Full taxonomic name of the speciman","Taxonomic Serial Number of the Genus the speciman belongs to",
            "Taxonomic Serial Number of the Species the speciman belongs to","Taxonomic Serial Number of the Infraspecies the speciman belongs to") #string describing what each variable represents
linker<-build_linker(cpnwh.data,variable_description = var_desc, variable_type = var_type) #build linker data frame

#Build dictionary
dict<-build_dict(my.data = cpnwh.data, linker = linker, option_description = NULL, prompt_varopts = F) #build data dictionary from linker

##Add dictionary as data attribute
data_desc <-"This data set contains the location and taxonomic identification of every vascular plant record collected in Idaho as reported by the Consortium of Pacific Northwest Herbaria." #describe data set
cpnwh.metadata <- incorporate_attr(my.data = cpnwh.data, data.dictionary = dict, main_string = data_desc)
attributes(cpnwh.metadata)$author <- "Robert Ritson, Research Associate (WMI)"
attributes(cpnwh.metadata)

##Save Appended Dataset as .rds
dataMeta::save_it(cpnwh.metadata, name_of_file = "Complete Dataset")

