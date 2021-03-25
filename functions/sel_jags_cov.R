sel_jags_cov <- function(ind.value = 0.1,
                         jags.model.output = pseudo.priors.model.jags,
                         jags.model.data = jags.data,
                         quad = c("ele2","slp2","sri2")) {
  
  # Cover - also select linear predictors if the quadratic variables are selected

  inc.cover <- if( any(jags.model.output$mean$ind_cover >= ind.value) ) {
    which(jags.model.output$mean$ind_cover >= ind.value) } else { 0 }
  
  names.inc.cover <- names(jags.model.data$cov.cover)[inc.cover]
  if( any(quads %in% names.inc.cover) ){
    star <- list()
    for (i in 1:length(quads) ){
      if( quads[i] %in% names.inc.cover ) {
        star[[i]] <- which(names(jags.model.data$cov.cover) %in% unique(c(names.inc.cover,gsub("2","",quads[i]))))
      }
    }
    inc.cover <- unique(unlist(star))
  }
  
  # Presence - also select linear predictors if the quadratic variables are selected
  
  inc.pres <- if( any(jags.model.output$mean$ind_pres >= ind.value) ) {
    which(jags.model.output$mean$ind_pres >= ind.value) } else { 0 }
  
  names.inc.pres <- names(jags.model.data$cov.pres)[inc.pres]
  if( any(quads %in% names.inc.pres) ){
    star <- list()
    for (i in 1:length(quads)){
      if( quads[i] %in% names.inc.pres ) {
        star[[i]] <- which(names(jags.model.data$cov.pres) %in% unique(c(names.inc.pres,gsub("2","",quads[i]))))
      }
    }
    inc.pres <- unique(unlist(star))
  }

  # no covariates selected
  if( all(inc.cover == 0) & all(inc.pres == 0) ){
    error.message <- "No Covariates Selected"
  } else {
    # presence covariates selected, no cover covariates
    if( all(inc.cover == 0) & all(inc.pres != 0) ){
      jags.data.sel <- jags.model.data
      jags.data.sel$cov.pres <- jags.data.sel$cov.pres[,inc.pres]
      jags.data.sel$cov.cover <- NULL
      jags.data.sel$n.covariates.pres <- length(inc.pres)
      jags.data.sel$n.covariates.cover <- NULL
    } else {
      # cover covariates selected, no presence covariates
      if( all(inc.cover != 0) & all(inc.pres == 0) ){
        jags.data.sel <- jags.model.data
        jags.data.sel$cov.pres <- NULL
        jags.data.sel$cov.cover <- jags.data.sel$cov.cover[,inc.cover]
        jags.data.sel$n.covariates.pres <- NULL
        jags.data.sel$n.covariates.cover <- length(inc.cover)
      } else {
        # cover and presence covariates selected
        jags.data.sel <- jags.model.data
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
    return(jags.data.sel)
  }
  
}