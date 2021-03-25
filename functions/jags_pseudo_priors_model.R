jags_pseudo_priors_model <- function(jags.data = jags.data,
                               ni = 10,
                               nt = 1,
                               nb = 5,
                               nc = 3){
  
  # load the jags model that will take a flexible selection of covariates with pseudo priors
  filename <- file.path("models","pseudopriors_flexible_covariate_jags_model.txt")
  
  # Initial values
  inits <- function(){list(Beta_int_cover = rnorm( 1, 0, 1 ), 
                           betaT_cover = rnorm( jags.data$n.covariates.cover, 0, 1 ), 
                           Beta_int_pres = rnorm( 1, 0, 1 ), 
                           betaT_pres = rnorm( jags.data$n.covariates.pres, 0, 1 ), 
                           Y = rep( 1, jags.data$n.polygon ))}
  
  # Parameters monitored
  parameters <- c("Beta_int_cover","ind_cover","Beta_cov_cover","Beta_int_pres","ind_pres","Beta_cov_pres","Beta_plotsize")
  
  # Call JAGS from R
  runif(1)
  basic.jags <- jagsUI::jags(jags.data, 
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
  
  return(basic.jags)
  
}