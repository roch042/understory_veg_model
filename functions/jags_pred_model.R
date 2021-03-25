jags_pred_model <- function(jags.data.sel = jags.data.sel,
                            ni = 10,
                            nt = 1,
                            nb = 5,
                            nc = 3){
  
  if( all(jags.data.sel %in% "No Covariates Selected") ) {
    
    basic.jags <- "No Covariates Selected"
    
  } else {
    
    # presence covariates and cover covariates selected ####
    if ( all(c("n.covariates.cover","n.covariates.pres") %in% names(jags.data.sel)) ) {
      
      # load the jags model that will take a flexible selection of covariates
      filename <- file.path("models","flexible_covariate_jags_model.txt")
      
      # Initial values
      inits <- function(){list(Beta_int_cover = rnorm( 1, 0, 1 ), 
                               Beta_cov_cover = rnorm( jags.data.sel$n.covariates.cover, 0, 1 ), 
                               Beta_int_pres = rnorm( 1, 0, 1 ), 
                               Beta_cov_pres = rnorm( jags.data.sel$n.covariates.pres, 0, 1 ), 
                               Y = rep( 1, jags.data.sel$n.polygon ))}
      
      # Parameters monitored
      parameters <- c("Beta_int_cover", "Beta_cov_cover", "Beta_int_pres", "Beta_cov_pres", "Beta_plotsize")
      
    } else {
      # presence covariate selected and cover covariates NOT selected ####
      if( !("n.covariates.cover" %in% names(jags.data.sel)) & (("n.covariates.pres") %in% names(jags.data.sel)) ){
        
        # load the jags model that will take a flexible selection of presence covariates only
        filename <- file.path("models","flexible_pres_covariate_jags_model.txt")
        
        # Initial values
        inits <- function(){list(Beta_int_cover = rnorm( 1, 0, 1 ),
                                 Beta_int_pres = rnorm( 1, 0, 1 ), 
                                 Beta_cov_pres = rnorm( jags.data.sel$n.covariates.pres, 0, 1 ), 
                                 Y = rep( 1, jags.data.sel$n.polygon ))}
        
        # Parameters monitored
        parameters <- c("Beta_int_cover", "Beta_int_pres", "Beta_cov_pres", "Beta_plotsize")
        
      } else {
        # presence covariates NOT selected and cover covariates selected ####
        if( ("n.covariates.cover" %in% names(jags.data.sel)) & !(("n.covariates.pres") %in% names(jags.data.sel)) ){
          
          # load the jags model that will take a flexible selection of covariates
          filename <- file.path("models","flexible_cover_covariate_jags_model.txt")
          
          # Initial values
          inits <- function(){list(Beta_int_cover = rnorm( 1, 0, 1 ), 
                                   Beta_cov_cover = rnorm( jags.data.sel$n.covariates.cover, 0, 1 ), 
                                   Beta_int_pres = rnorm( 1, 0, 1 ),
                                   Y = rep( 1, jags.data.sel$n.polygon ))}
          
          # Parameters monitored
          parameters <- c("Beta_int_cover", "Beta_cov_cover", "Beta_int_pres", "Beta_plotsize")
          
        }
      }
      
    }
    
    
    # Call JAGS from R
    runif(1)
    basic.jags <- jagsUI::jags(jags.data.sel, 
                               inits, 
                               parameters, 
                               filename, 
                               n.iter = ni, 
                               n.chains = nc, 
                               n.burnin = nb,
                               codaOnly = c("eps"), 
                               n.thin = nt,
                               modules = c("basemod","bugs","glm","dic"),
                               parallel = TRUE)
    
  }
  
  return(basic.jags)
  
}
