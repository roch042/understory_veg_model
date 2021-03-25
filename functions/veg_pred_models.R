veg_pred_models <- function(pred_data = pred_data,
                            pred_data_ids = pred_data_ids,
                            mod_file = mod_file,
                            miss_rows = miss_rows){
  
  
  # Load Model ####
  load(here(file.path("models",folder,"presence",mod_file)))
  
  
  # Refit best model with all the data ####
  
  attach(RES)
  
  best_model <- BEST$max_AUC_test
  
  if(length(best_model)!= 0) {
  
  if(best_model == "GLM"){
    pred_model <- caret::train(Present2 ~., 
                               data = DATA$total[,c(MODELS$GLM$outcomeName,DATA$predictorsNames)], 
                               method = "glm", 
                               trControl = CONTROL$myControl,
                               metric = "ROC",
                               preProc = c("center","scale"))
  }
  
  if(best_model == "GBM"){
    pred_model <- caret::train(Present2 ~., 
                               data = DATA$total[,c(MODELS$GBM$outcomeName,DATA$predictorsNames)], 
                               method = "gbm", 
                               trControl = CONTROL$myControl,
                               metric = "ROC",
                               preProc = c("center","scale"))
  }
  
  if(best_model == "GLMNET"){
    pred_model <- caret::train(Present ~., 
                               data = DATA$total[,c(MODELS$GLMNET$outcomeName,DATA$predictorsNames)], 
                               method = "glmnet", 
                               trControl = trainControl(
                                 method = "cv",
                                 verboseIter = TRUE,
                                 savePredictions = TRUE,
                                 index = CONTROL$myFolds
                               ),
                               metric = "RMSE",
                               preProc = c("center","scale"))
  }
  
  if(best_model == "RF"){
    pred_model <- caret::train(
      Present2 ~ .,
      tuneLength = 3,
      data = DATA$total[,c(MODELS$RF$outcomeName,DATA$predictorsNames)],  
      method = "ranger",
      preProc = c("center","scale"),
      trControl = CONTROL$myControl
    )
  }
  
  if(best_model == "GLMNET_class"){
    pred_model <- caret::train(
      Present2 ~ ., 
      data = DATA$total[,c(MODELS$GLMNET_class$outcomeName,DATA$predictorsNames)],  
      tuneGrid = expand.grid(
        alpha = 0:1,
        lambda = seq(0.0001, 1, length = 20)
      ),
      method = "glmnet",
      trControl = CONTROL$myControl
    )
  }
  
  if(best_model == "XGBOOST"){
    pred_model <- caret::train(
      Present2 ~ ., 
      data = DATA$total[,c(MODELS$XGBOOST$outcomeName,DATA$predictorsNames)],  
      trControl = CONTROL$myControl,
      tuneGrid = expand.grid(
        nrounds = 100,
        max_depth = 6,
        eta = 0.3,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      ),
      method = "xgbTree",
      verbose = TRUE
    )
  }

  predictions_raw <- predict(object = pred_model, pred_data, type = 'raw')
  predictions_prob <- predict(object = pred_model, pred_data, type = 'prob')
    
  if(length(miss_rows)>1){
    # Predictions ####
    missed_ids <- pred_data_ids[miss_rows]
    ids <- pred_data_ids[-miss_rows]
    pred <- data.frame(QuadPoly_ID = c(ids, missed_ids), 
                       Present = c(ifelse(predictions_raw == "Yes",1,0), rep(NA,length(missed_ids))),
                       Prob = c(predictions_prob$Yes, rep(NA,length(missed_ids))))
  } else {
    # Predictions ####
    pred <- data.frame(QuadPoly_ID = pred_data_ids, 
                       Present = ifelse(predictions_raw == "Yes",1,0),
                       Prob = predictions_prob$Yes)  
  }
    
  return(list(pred_model = pred_model, pred = pred, best_model = best_model))
  } else {
    return(list(pred_model = NULL, pred = NULL, best_model = NULL))
  }
}
