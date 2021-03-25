# load libraries
require(tidyverse)
require(jagsUI)

options(scipen=999)

# load the results
load("~/Fine scale vegetation analysis/veg_jags_model/Artemisia_tridentata_Artemisia_results_2018-06-11.RData")

# covariate list
cover_cov <- names(bkwdsel_results$jags.data.sel$cov.cover)
pres_cov <- names(bkwdsel_results$jags.data.sel$cov.pres)

# covariate parameters
beta_cover <- results$summary[stringr::str_detect(row.names(results$summary),"cover"),] %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(covariate_name=c("Intercept",cover_cov)) %>%
  dplyr::select(covariate_name,everything())
beta_pres <- results$summary[stringr::str_detect(row.names(results$summary),"pres"),] %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(covariate_name=c("Intercept",pres_cov)) %>%
  dplyr::select(covariate_name,everything())

# model matrix
matrix_cover <- data.frame(Intercept=1,bkwdsel_results$jags.data.sel$cov.cover)
matrix_pres <- data.frame(Intercept=1,bkwdsel_results$jags.data.sel$cov.pres)

# logit
logit_cover <- as.matrix(matrix_cover) %*% as.matrix(beta_cover$mean)
logit_pres <- as.matrix(matrix_pres) %*% as.matrix(beta_pres$mean)

# prediction
pred_cover <- plogis(logit_cover)
pred_pres <- plogis(logit_pres)

# probability of presence
prob_cover <- pred_cover*pred_pres

# comparing to observed data
prob_cover <- data.frame(PolyIndex=1:length(prob_cover),prob_cover=prob_cover)
obs_cover_plot <- data.frame(Obs_Plot=bkwdsel_results$jags.data.sel$Obs_Plot,PlotPolyIndex=bkwdsel_results$jags.data.sel$PlotPolyIndex) %>%
  dplyr::group_by(PlotPolyIndex) %>%
  dplyr::summarise(Mean_Obs = mean(Obs_Plot,na.rm=TRUE))
obs_cover_pts <- data.frame(Obs_Pt=bkwdsel_results$jags.data.sel$Obs_Pt,PtsPolyIndex=bkwdsel_results$jags.data.sel$PtsPolyIndex) %>%
  dplyr::group_by(PtsPolyIndex) %>%
  dplyr::summarise(Mean_Obs = sum(Obs_Pt,na.rm=TRUE)/n())
c1 <- prob_cover %>%
  dplyr::left_join(obs_cover_plot,by=c("PolyIndex"="PlotPolyIndex")) %>%
  dplyr::filter(!is.na(Mean_Obs))
c2 <- prob_cover %>%
  dplyr::left_join(obs_cover_pts,by=c("PolyIndex"="PtsPolyIndex"))  %>%
  dplyr::filter(!is.na(Mean_Obs))
compare <- dplyr::bind_rows(c1,c2)

hist(prob_cover)

