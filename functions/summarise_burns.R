library(tidyverse)
library(here)

load(here(file.path("data","Covariates_QuadPolyID.RData")))

tsf_summary <- Covariates_QuadPolyID %>%
  dplyr::group_by(tsf) %>%
  dplyr::summarise(n = length(unique(QuadPoly_ID)))

ff_summary <- Covariates_QuadPolyID %>%
  dplyr::group_by(ff) %>%
  dplyr::summarise(n = length(unique(QuadPoly_ID)))