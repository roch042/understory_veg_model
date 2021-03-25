bkwdsel_jags_cov <- function(selection.data = jags.data,
                         quads = c("ele2", "slp2", "casp2", "twi2", "sri2", "minpr2", "maxtp2", "aws2", "clay2", "sand2", "silt2", "cec2", "d2r2", "ph2", "om2", "caco32", "tsf2", "ff2", "tc2", "sc2") ){

  
list2env(selection.data,environment())

  
# PLOTS - dataframe for analysis ####
PLOTS <- data.frame(Obs_Plot=Obs_Plot,PlotPolyIndex=PlotPolyIndex) %>%
  dplyr::left_join(data.frame(PlotPolyIndex=1:nrow(cov.cover),cov.cover)) %>%
  dplyr::rename(Prop = Obs_Plot) %>%
  dplyr::select(-PlotPolyIndex) %>%
  dplyr::mutate(Present = ifelse(Prop > 0, 1, 0))


# PTS - dataframe for analysis ####
PTS <- data.frame(Obs_Pt=Obs_Pt,PtsPolyIndex=PtsPolyIndex) %>%
  dplyr::group_by(PtsPolyIndex) %>%
  dplyr::summarise(Total = n(), Present = sum(Obs_Pt), Prop = Present/Total) %>%
  dplyr::left_join(data.frame(PtsPolyIndex=1:nrow(cov.cover),cov.cover)) %>%
  dplyr::select(-PtsPolyIndex, -Total, -Present) %>%
  dplyr::mutate(Present = ifelse(Prop > 0, 1, 0))


# COMBO - combined dataframes for analysis ####
COMBO <- dplyr::bind_rows(PLOTS,PTS)


# Presence - Backwards Selection ####
X <- dplyr::select(COMBO, -Prop)
full <- glm(Present ~ ., family="binomial", data=X)
step.present <- MASS::stepAIC(full,direction="backward",trace=FALSE)


# # Proportional Cover - Backwards Selection ####
# X <- dplyr::select(COMBO, -Present)
# full <- lm(Prop ~ ., data=X)
# step.prop <- MASS::stepAIC(full,direction="backward",trace=FALSE)
step.prop <- NULL

# Presence - also select linear predictors if the quadratic variables are selected ####
names.inc.pres <- names(coefficients(step.present))[-1]
inc.pres <- if(length(coefficients(step.present))==1) {0} else {
  which(names(cov.pres) %in% names.inc.pres)
}

if( any(quads %in% names.inc.pres) ){
  star <- list()
  for (i in 1:length(quads)){
    if( quads[i] %in% names.inc.pres ) {
      star[[i]] <- which(names(cov.pres) %in% unique(c(names.inc.pres,gsub("2","",quads[i]))))
    }
  }
  inc.pres <- unique(unlist(star))
}


# # Cover - also select linear predictors if the quadratic variables are selected ####
# names.inc.cover <- names(coefficients(step.prop))[-1]
# inc.cover <- if(length(coefficients(step.prop))==1) {0} else {
#   which(names(cov.cover) %in% names.inc.cover)
# }
# 
# if( any(quads %in% names.inc.cover) ){
#   star <- list()
#   for (i in 1:length(quads) ){
#     if( quads[i] %in% names.inc.cover ) {
#       star[[i]] <- which(names(cov.cover) %in% unique(c(names.inc.cover,gsub("2","",quads[i]))))
#     }
#   }
#   inc.cover <- unique(unlist(star))
# }
inc.cover <- inc.pres

# no covariates selected
if( all(inc.cover == 0) & all(inc.pres == 0) ){
  error.message <- "No Covariates Selected"
} else {
  # presence covariates selected, no cover covariates
  if( all(inc.cover == 0) & all(inc.pres != 0) ){
    jags.data.sel <- selection.data
    jags.data.sel$cov.pres <- jags.data.sel$cov.pres[,inc.pres]
    jags.data.sel$cov.cover <- NULL
    jags.data.sel$n.covariates.pres <- length(inc.pres)
    jags.data.sel$n.covariates.cover <- NULL
  } else {
    # cover covariates selected, no presence covariates
    if( all(inc.cover != 0) & all(inc.pres == 0) ){
      jags.data.sel <- selection.data
      jags.data.sel$cov.pres <- NULL
      jags.data.sel$cov.cover <- jags.data.sel$cov.cover[,inc.cover]
      jags.data.sel$n.covariates.pres <- NULL
      jags.data.sel$n.covariates.cover <- length(inc.cover)
    } else {
      # cover and presence covariates selected
      jags.data.sel <- selection.data
      jags.data.sel$cov.pres <- jags.data.sel$cov.pres[,inc.pres]
      jags.data.sel$cov.cover <- jags.data.sel$cov.cover[,inc.cover]
      jags.data.sel$n.covariates.pres <- length(inc.pres)
      jags.data.sel$n.covariates.cover <- length(inc.cover)
    }
  }
}

if(exists("error.message")){
  return(error.message)
} else {
  return(list(jags.data.sel=jags.data.sel,step.prop=step.prop,step.present=step.present))
}

}