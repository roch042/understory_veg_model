# load libraries ####
lapply(c("tidyverse","stringr","here","caret","pROC","rpart","ranger","caTools","skimr"),require,character.only=T)

# load function ####
lapply(list(here(file.path("functions","run_models_function.R"),
            here(file.path("functions","veg_data.R"),
            here(file.path("functions","veg_train_models.R"),
            here(file.path("functions","propveg_train_models.R")), source)

# load data ####
load(here(file.path("data","FieldDataPoints_QuadPolyID.RData"))) # index of PlotKeys and QuadPolyIDs/quads
load(here(file.path("data","Covariates_QuadPolyID.RData"))) # covariate values by QuadPolyID
load(here(file.path("data","FORAGE.RData"))) # species listed as forage species (deer, elk, and sage grouse)
load(here(file.path("data","ModelCode_List.RData"))) # all species (genus, species, subspecies)
load(here(file.path("data","EcoCodes.RData"))) # idaho divided up into Bailey's ecoregions

# function to format data and then train model ####
fielddata <- read.csv(here(file.path("data","FieldData","FieldData_26Oct2020.csv")))
run.model(fielddata = fielddata)
