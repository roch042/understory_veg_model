# Load Packages ####
require(rgdal)
require(tidyverse)
require(stringr)

# load function
source(here(file.path("functions","assign_ecoregions.R")))

# Load the Field Data ####
load(here(file.path("data","FieldDataPoints_QuadPolyID.RData")))

EcoCodes <- assign_ecoregions(FieldDataPoints_QuadPolyID)

save(EcoCodes,file=here(file.path("data","EcoCodes.RData")))
