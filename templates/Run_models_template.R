# load libraries ####
require(tidyverse)
require(stringr)
require(here)
require(caret)
require(pROC)
require(rpart)
require(ranger)
require(caTools)
require(skimr)

# load function ####
source(here(file.path("functions","veg_data.R")))
source(here(file.path("functions","veg_train_models.R")))
source(here(file.path("functions","propveg_train_models.R")))

# load data ####
load(here(file.path("data","FieldDataPoints_QuadPolyID.RData"))) # index of PlotKeys and QuadPolyIDs/quads
load(here(file.path("data","Covariates_QuadPolyID.RData"))) # covariate values by QuadPolyID
load(here(file.path("data","FORAGE.RData"))) # species listed as forage species (deer, elk, and sage grouse)
load(here(file.path("data","ModelCode_List.RData"))) # all species (genus, species, subspecies)
load(here(file.path("data","EcoCodes.RData"))) # idaho divided up into Bailey's ecoregions

fielddata <- read.csv(here(file.path("data","FieldData","FieldData_14Sept2020.csv")))

# function to format data and then train model ####

Model <- Forage_Models_SubSpecies # change this
folder <- "SubSpecies" # change this
type <- "presence" # percent_cover, presence
# selections <- which(Forage_Models_Species$ModelCodeTitle %in% c("Vaccinium membranaceum"))

for (i in 1:nrow(Model)){
# for(i in selections){
  file_title <- paste(Model$Genus[i],"_",Model$Species[i],"_",Model$SubSpecies[i],".RData",sep="")
  # if (file_title %in% list.files(here(file.path("models",folder,type)))){
  #   next
  # } else {
    tryCatch({
      print(paste("iteration number ",i," of ",nrow(Model),":",sep=""))
      
      # format data ####
      print("formatting data")
      DAT <- veg_data(genus = Model$Genus[i],
                      species = Model$Species[i],
                      subspecies = Model$SubSpecies[i],
                      fielddata = fielddata)
      
      # train models ####
      print("training models")
      if(type == "presence") {
        RES <- veg_train_models(DAT=DAT)
      } else {
        if(type == "percent_cover"){
          RES <- propveg_train_models(DAT=DAT)
        }
      }
      
      # save training models ####
      print("exporting models")
      if(is.na(Model$Species[i]) & is.na(Model$SubSpecies[i])){
        save(list=c("RES","DAT"),file=here(file.path("models","genus",type,file_title)))
      } else {
        if(!is.na(Model$Species[i]) & is.na(Model$SubSpecies[i])){
          save(list=c("RES","DAT"),file=here(file.path("models","species",type,file_title))) 
        } else {
          save(list=c("RES","DAT"),file=here(file.path("models","subspecies",type,file_title))) 
        }
      }
    }, error=function(e){})
  # }
}

