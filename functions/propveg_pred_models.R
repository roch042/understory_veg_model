propveg_pred_models <- function(pred_data = pred_data,
                            pred_data_ids = pred_data_ids,
                            mod_file = mod_file,
                            miss_rows = miss_rows){
  
  
  # Load Model ####
  load(here(file.path("models",folder,"percent_cover",mod_file)))
  
  
  # Refit best model with all the data ####
  
  attach(RES)
  
  best_model <- BEST$min_RMSE_test
  
  if(best_model == "LM"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$GLM$outcomeName,DATA$predictorsNames)], 
                               method = "lm", 
                               trControl = CONTROL$myControl,
                               metric = "RMSE",
                               preProc = c("center","scale"),
                               na.action = na.pass)
    
  }
  
  if(best_model == "GLM"){

    var <- sym(MODELS$GLM$outcomeName)
    
    DATA$total <- DATA$total %>%
      dplyr::mutate(!! var := ifelse(!! var > 1, 1, !! var))
    
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$GLM$outcomeName,DATA$predictorsNames)], 
                               method = "glm",
                               family = binomial(link = "logit"),
                               trControl = CONTROL$myControl,
                               metric = "RMSE",
                               preProc = c("center","scale"),
                               na.action = na.pass)
    
  }
  
  if(best_model == "GBM"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$GBM$outcomeName,DATA$predictorsNames)], 
                               method = "gbm", 
                               trControl = CONTROL$myControl,
                               metric = "RMSE",
                               preProc = c("center","scale"))
  }
  
  if(best_model == "GLMNET"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$GLMNET$outcomeName,DATA$predictorsNames)], 
                               method = "glmnet", 
                               trControl = CONTROL$myControl,
                               metric = "RMSE",
                               preProc = c("center","scale"))
  }
  
  if(best_model == "RF"){
    pred_model <- caret::train(CONTROL$formula,
                               data = DATA$total[,c(MODELS$RF$outcomeName,DATA$predictorsNames)],  
                               method = "ranger",
                               preProc = c("center","scale"),
                               trControl = CONTROL$myControl
    )
  }
  
  if(best_model == "XGBOOST"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$XGBOOST$outcomeName,DATA$predictorsNames)],  
                               trControl = CONTROL$myControl,
                               method = "xgbTree",
                               verbose = TRUE
    )
  }
  
  if(best_model == "RPART"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$RPART$outcomeName,DATA$predictorsNames)],  
                               trControl = CONTROL$myControl,
                               method = "rpart"
    )
  }  
  
  if(best_model == "RPART2"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$RPART2$outcomeName,DATA$predictorsNames)],  
                               trControl = CONTROL$myControl,
                               method = "rpart2"
    )
  }  
  
  if(best_model == "TREEBAG"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$TREEBAG$outcomeName,DATA$predictorsNames)],  
                               trControl = CONTROL$myControl,
                               method = "treebag",
                               verbose = TRUE
    )
  }  
  
  if(best_model == "BAGEARTH"){
    pred_model <- caret::train(CONTROL$formula, 
                               data = DATA$total[,c(MODELS$BAGEARTH$outcomeName,DATA$predictorsNames)],  
                               trControl = CONTROL$myControl,
                               method = "bagEarth"
    )
  }  
  
  # Predictions ####
  predictions_prob <- predict(object = pred_model, pred_data)
  
  if(length(miss_rows)>1){
    # Predictions ####
    missed_ids <- pred_data_ids[miss_rows]
    ids <- pred_data_ids[-miss_rows]
    pred <- data.frame(QuadPoly_ID = c(ids, missed_ids), 
                       Prob = c(predictions_prob, rep(NA,length(missed_ids))))
  } else {
    # Predictions ####
    pred <- data.frame(QuadPoly_ID = pred_data_ids, 
                       Prob = predictions_prob)  
  }
  
  return(list(pred_model = pred_model, pred = pred, best_model = best_model))
  
}
