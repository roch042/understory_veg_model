sel.genus = "Artemisia"
sel.species = "tridentata"
sel.subspecies = NA
# set.wd = c("D:\\Fine scale vegetation analysis\\veg_jags_model")
set.wd = c("C:\\Users\\eroche\\Documents\\Fine scale vegetation analysis\\veg_jags_model")

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
source(file.path("functions","bkwdsel_predict.R"))

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Select Covariates - Backwards AIC Selection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
eco_codes <- unique(LpiCov$ECOCODE)
bkwdsel_results <- list()

for (i in 1:length(eco_codes)){
  
  Plot_EcoIndex <- which(jags.data$PlotEcoCode == eco_codes[i])
  Pts_EcoIndex <- which(jags.data$PtsEcoCode == eco_codes[i])
  
  eco.jags.data <- list(Obs_Plot = jags.data$Obs_Plot[Plot_EcoIndex],
                        Obs_Pt = jags.data$Obs_Pt[Pts_EcoIndex],
                        PlotPolyIndex = jags.data$PlotPolyIndex[Plot_EcoIndex],
                        PtsPolyIndex = jags.data$PtsPolyIndex[Pts_EcoIndex],
                        PlotEcoCode = jags.data$PlotEcoCode[Plot_EcoIndex],
                        PtsEcoCode = jags.data$PtsEcoCode[Pts_EcoIndex],
                        n.polygon = length(unique(c( jags.data$PlotPolyIndex[Plot_EcoIndex], jags.data$PtsPolyIndex[Pts_EcoIndex] ))),
                        n.plots = length( Plot_EcoIndex ),
                        n.pts = length( Pts_EcoIndex ),
                        cov.pres = jags.data$cov.pres,
                        cov.cover = jags.data$cov.cover,
                        PlotSize = jags.data$PlotSize[Plot_EcoIndex],
                        n.covariates.pres = jags.data$n.covariates.pres,
                        n.covariates.cover = jags.data$n.covariates.cover)
  
  if( any(eco.jags.data$Obs_Plot > 0) | any(eco.jags.data$Obs_Pt > 0) ){
    bkwdsel_results[[i]] <- bkwdsel_jags_cov(selection.data = eco.jags.data)
  } else {
    next
  }
  
}

bkwdsel_results_all <- bkwdsel_jags_cov(selection.data = jags.data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Compare Test data and model predictions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

p_all <- bkwdsel_predict(selection.data = jags.data_test,
                model = bkwdsel_results_all)


plot(p_all$pred.prop ~ p_all$test.prop)
accuracy <- table(p_all$pred.pres, p_all$test.pres)
sum(diag(accuracy))/sum(accuracy)

jags.data.sel <- bkwdsel_results$jags.data.sel


#~~~~~~~~~~~~~~~#
# Jags Model ####
#~~~~~~~~~~~~~~~#

results <- jags_pred_model(jags.data.sel = jags.data.sel,
                           ni = 40000,
                           nt = 6,
                           nb = 10000,
                           nc = 3)

setwd(file.path(set.wd,"results"))
save(list=c("results","bkwdsel_results"),file=stringr::str_c(sel.genus,sel.species,sel.genus,"results",stringr::str_c(Sys.Date(),".RData",sep=""),sep="_"))
