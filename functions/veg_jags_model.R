veg_jags_model <- function(
  sel.genus = "Artemisia",
  sel.species = "tridentata",
  sel.subspecies = "vaseyana",
  set.wd = set.wd){
  
  setwd(set.wd)
  
  # load libraries
  require(tidyverse)
  require(stringr)
  require(jagsUI)
 
  # load function
  source(file.path("functions","veg_jags_data.R"))
  source(file.path("functions","sel_jags_cov.R"))
  source(file.path("functions","jags_pseudo_priors_model.R"))
  source(file.path("functions","jags_pred_model.R"))
  source(file.path("functions","bkwdsel_jags_cov.R"))

  # source("C:/Users/eroche/Documents/Fine scale vegetation analysis/veg_jags_model/functions/jags_pred_model.R")
  
  #~~~~~~~~~~~~~~~~#
  # Format Data ####
  #~~~~~~~~~~~~~~~~#
  
  # run the function to make the jags data
  list2env(
    veg_jags_data(filepath = "data/FieldData.RData",
                  genus = sel.genus,
                  species = sel.species,
                  subspecies = sel.subspecies),
    environment())
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Jags Model - PseudoPriors ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 
  # pseudo.priors.model.jags <- jags_pseudo_priors_model(jags.data = jags.data,
  #                                  ni = 30000,
  #                                  nt = 3,
  #                                  nb = 10000,
  #                                  nc = 3)
  # 
  # 
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Select Covariates with Acceptable Indicator Variable Values ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 
  # jags.data.sel <- sel_jags_cov(ind.value = 0.3,
  #                               jags.model.output = pseudo.priors.model.jags,
  #                               jags.model.data = jags.data)
  # 
  # 
  # 
  
  
  
  if( !exists("error") )
  {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # Select Covariates - Backwards AIC Selection ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    bkwdsel_results <- bkwdsel_jags_cov(selection.data = jags.data_train)
    jags.data.sel <- bkwdsel_results$jags.data.sel
    jags.data.sel[["PlotEcoCode"]] <- NULL
    jags.data.sel[["PtsEcoCode"]] <- NULL
    jags.data.sel[["df.cov.pres"]] <- NULL
    jags.data.sel[["df.cov.cover"]] <- NULL
    
    #~~~~~~~~~~~~~~~#
    # Jags Model ####
    #~~~~~~~~~~~~~~~#
    
    results <- jags_pred_model(jags.data.sel = jags.data.sel,
                               ni = 40000,
                               nt = 6,
                               nb = 10000,
                               nc = 3)
    
    setwd(file.path(set.wd,"results"))
    save(list=c("results","bkwdsel_results"),file=paste(sel.genus,sel.species,sel.subspecies,"results",stringr::str_c(Sys.Date(),".RData",sep=""),sep="_"))
  } else {
    setwd(file.path(set.wd,"results"))
    save("error",file=paste(sel.genus,sel.species,sel.subspecies,"ERROR",stringr::str_c(Sys.Date(),".RData",sep=""),sep="_"))
  }
  
}