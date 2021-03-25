bkwdsel_predict <- function(selection.data = jags.data_test,
                            model = bkwdsel_results_all){
 
   
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
  
  
  # # Proportional Cover - Prediction ####
  # X <- dplyr::select(COMBO, -Present) %>%
  #   dplyr::filter(Prop > 0)  %>% # limit to only present
  #   dplyr::mutate(Prop = ifelse(Prop == 1, Prop - 0.0001, Prop),
  #                 lProp = qlogis(Prop)) %>%
  #   dplyr::select(-Prop)
  # pred.prop <- plogis(predict(model$step.prop, newdata=X, type="response"))
  # test.prop <- plogis(X$lProp)
  pred.prop <- NULL
  test.prop <- NULL
  
  # Presence - Prediction ####
  X <- dplyr::select(COMBO, -Prop)
  pred.pres <- ifelse(predict(model$step.pres, newdata=X, type="response") < 0.5, 0, 1)
  prob <- predict(model$step.pres, newdata=X, type="response")
  pred <- ROCR::prediction(prob, X$Present)
  auc <- ROCR::performance(pred, measure = "auc")@y.values[[1]]
  
  return(list(pred.prop=pred.prop,
              test.prop=test.prop,
              pred.pres=pred.pres,
              test.pres=COMBO$Present,
              auc=auc))
  
}