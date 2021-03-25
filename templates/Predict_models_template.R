# arguments from git bash ####
args <- commandArgs()
print(args)

# load libraries ####
print("load libraries...")
require(tidyverse)
require(stringr)
require(here)
require(caret)
require(pROC)
require(rpart)
require(ranger)
require(caTools)
require(skimr)

# load functions ####
print("load functions...")
source(here(file.path("functions","veg_pred_models.R")))
source(here(file.path("functions","propveg_pred_models.R")))

# load covariates for prediction ####
print("load covariate data...")
load(here(file.path("data","Prepare_Covariates_Prediction.RData")))

# generate predictions ####
title <- "Units_1_6_10A"
folder <- args[7] # folder
type <- args[8] # percent_cover, presence
mod_files <- list.files(here(file.path("models",folder,type)))
# mod_files <- c("Vaccinium_membranaceum_NA.RData")
pos <- as.numeric(args[6]) # 6th position in args is the iteration number
# mod <- list.files(here(file.path("models",folder,type)))[pos]
mod <- mod_files[pos]
# print(mod_files)
# print(args)
# print(pos)
# print(mod)
# print(paste(title,mod,sep="_"))

print("check to see if model already run...")
if(paste(title,mod,sep="_") %in% list.files(here(file.path("results",folder,type)))){
  print("model already predicted from")
} else {
  print("start model prediction...")
  if(type == "presence"){
  pred <- veg_pred_models(pred_data_ids = pred_data_ids, 
                          pred_data = pred_data, 
                          mod_file = mod,
                          miss_rows = miss_rows) 
  } else {
    if(type == "percent_cover"){
      pred <- propveg_pred_models(pred_data_ids = pred_data_ids, 
                                  pred_data = pred_data, 
                                  mod_file = mod,
                                  miss_rows = miss_rows) 
    }
  }
  gc()
  print("save model prediction...")
  save(pred,file=here(file.path("results",folder,type,paste(title,mod,sep="_"))))
  readr::write_csv(pred$pred,path=here(file.path("results",folder,type,"text",paste(title,paste(gsub(".RData","",mod),"csv",sep="."),sep="_"))))
  print(paste("completed iteration",pos,sep=" "))
  rm(pred); gc()
}

