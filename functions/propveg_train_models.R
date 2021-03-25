propveg_train_models <- function(DAT=DAT)
{
  
  #~~~~~~~~~~~~~~~~~~~~~#
  # MACHINE LEARNING ####
  #~~~~~~~~~~~~~~~~~~~~~#
  
  Total <- dplyr::bind_rows(DAT$PTS,DAT$PLOTS) %>%
    dplyr::ungroup() %>%
    dplyr::select(-QuadPoly_ID,-ECOCODE) %>%
    dplyr::select(-nass,-tpi,-dev,-tsf,-ff,-sc) %>% # exclude categorical covariates
    dplyr::mutate(Count = round(Prop*100, digits=0),
                  lCount = log(Count + 0.001))
  
  if(length(unique(Total$Present))==1 | table(Total$Present)[2] < 3){
    return("not enough data collected") # changed
  } else {
    
    # extra variables
    extraNames <- c("QuadPoly_ID","Total","Prop","OBJECTID","quad","Shape_Length","Shape_Area","mean_blue","mean_green","mean_red","mean_nir","sd_blue","sd_green","sd_red","sd_nir","Hit","Present","lCount","Count") #changed
    
    # response variable
    outcomeName <- "Prop" # changed
    
    # covariates
    predictorsNames <- names(Total)[which(!names(Total) %in% c(extraNames,outcomeName))]
    
    # Get the number of observations
    n_obs <- nrow(Total)
    
    repeat{
      # Shuffle row indices: permuted_rows
      permuted_rows <- sample(n_obs)
      
      # Randomly order data
      Total <- Total[permuted_rows, ] 
      
      # Identify row to split on: split
      split <- round(n_obs * 0.6)
      # splitIndex <- createDataPartition(titanicDF[,outcomeName], p = .75, list = FALSE, times = 1)
      
      # Create train
      train <- Total[1:split,]
      
      # Create test
      test <- Total[(split+1):nrow(Total),]
      
      if(length(unique(train$Present))==2 & length(unique(test$Present))==2) {
        break
      }
    }  
    
    # Collect Training Data ####
    DATA <- list(train = train, test = test, total = Total, predictorsNames = predictorsNames)
    
    # Define trainControl() ####
    
    # Create custom indices: myFolds
    myFolds <- createFolds(unlist(train[,outcomeName]), k = 10)
    myControl <- trainControl(
      method = "cv",
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = myFolds
    )
    
    outcomeName <- "Prop"
    
    formula <- formula(paste(outcomeName," ~ .",sep=""))
    
    CONTROL <- list(myFolds = myFolds, myControl = myControl, outcomeName = outcomeName, formula = formula)

    
    # GLM with caret package ####
    GLM <- NULL
    
    tryCatch({
      # Fit glm model: model
      GLM$outcomeName <- outcomeName
      
      model <- caret::train(formula, # changed
                            data = train[,c(outcomeName,predictorsNames)], 
                            method = "glm",
                            family = binomial(link = "logit"),
                            trControl = myControl,
                            metric = "RMSE",
                            preProc = c("center","scale"),
                            na.action = na.pass)
      GLM$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      GLM$train_results_summary <- model$results
      GLM$test_results_summary <- postResample(pred=predictions, obs=observations)
      GLM$pred_prob <- predictions
      
      
    }, error=function(e){})
    print("finished GLM")
    
    # LM with caret package ####
    LM <- NULL
    
    tryCatch({
      # Fit glm model: model
      LM$outcomeName <- outcomeName
      
      model <- caret::train(formula, # changed
                            data = train[,c(outcomeName,predictorsNames)], 
                            method = "lm",
                            trControl = myControl,
                            metric = "RMSE",
                            preProc = c("center","scale"),
                            na.action = na.pass)
      LM$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      LM$train_results_summary <- model$results
      LM$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      LM$pred_prob <- predictions
      
      
    }, error=function(e){})
    print("finished LM")
    
    # GBM with caret package ####
    
    GBM <- NULL
    
    tryCatch({
      # Create tuneGrid object: Grid
      Grid <- expand.grid(interaction.depth = c(1, 2, 3, floor(sqrt(NCOL(train)))),
                          n.trees = c(0,10,50,100,500,1000,2000), 
                          # shrinkage = seq(.0005, .05,.005),
                          shrinkage = c(.0005, .005, .05),
                          n.minobsinnode = 10) 
      
      # Train gbm with custom trainControl: model
      GBM$outcomeName <- outcomeName
      
      model <- train(formula, 
                     data = train[,c(outcomeName,predictorsNames)], 
                     method = "gbm", 
                     trControl = myControl,
                     metric = "RMSE",
                     tuneGrid = Grid,
                     preProc = c("center","scale"),
                     na.action = na.pass)
      GBM$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      GBM$train_results_summary <- model$results
      GBM$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      GBM$pred_prob <- predictions
      
    }, error=function(e){})
    print("finished GBM")    
    
    # GLMNET with caret package ####
    
    GLMNET <- NULL
    
    tryCatch({
      
      # Train glmnet with custom trainControl: model
      GLMNET$outcomeName <- outcomeName
      
      # Create tuneGrid object: Grid
      Grid <- expand.grid(alpha = seq(0, 1, 0.1),
                          lambda = c(0.7804577, 0.07804577, 0.007804577, 0.0007804577, 0.00007804577)) 
      
      model <- train(formula, 
                     data = train[,c(outcomeName,predictorsNames)], 
                     method = "glmnet", 
                     trControl = myControl,
                     metric = "RMSE",
                     # tuneGrid = Grid,
                     preProc = c("center","scale"),
                     na.action = na.pass)
      GLMNET$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      GLMNET$train_results_summary <- model$results
      GLMNET$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      GLMNET$pred_prob <- predictions
      
    }, error=function(e){}) 
    
    print("finished GLMNET")    
    
    # Random forest model with caret ####
    
    RF <- NULL
    
    tryCatch({
      # Fit random forest: model
      RF$outcomeName <- outcomeName
      
      model <- train(formula,
                     data = train[,c(outcomeName,predictorsNames)],  
                     method = "ranger",
                     metric = "RMSE",
                     # tuneGrid = Grid,
                     preProc = c("center","scale"),
                     trControl = myControl,
                     na.action = na.pass
      )
      RF$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      RF$train_results_summary <- model$results
      RF$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      RF$pred_prob <- predictions
      
    }, error=function(e){}) 
    
    print("finished RF")   
    
    # XGBOOST with caret package ####
    
    XGBOOST <- NULL
    
    tryCatch({
      
      # Fit XGBOOST
      XGBOOST$outcomeName <- outcomeName
      
      Grid <- expand.grid(
        nrounds = 100,
        max_depth = 6,
        eta = 0.3,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )
      
      model <- caret::train(formula, 
                            data = train[,c(outcomeName,predictorsNames)],  
                            trControl = myControl,
                            # tuneGrid = Grid,
                            method = "xgbTree",
                            verbose = TRUE,
                            na.action = na.pass
      )
      XGBOOST$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      XGBOOST$train_results_summary <- model$results
      XGBOOST$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      XGBOOST$pred_prob <- predictions
      
    }, error=function(e){})
    
    print("finished XGBOOST")   
    
    
    # RPART with caret package ####
    
    RPART <- NULL
    
    tryCatch({
      
      # Fit RPART
      RPART$outcomeName <- outcomeName
      
      model <- caret::train(formula, 
                            data = train[,c(outcomeName,predictorsNames)],  
                            trControl = myControl,
                            # tuneGrid = Grid,
                            method = "rpart",
                            maximize=FALSE,
                            na.action = na.pass)
      RPART$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      RPART$train_results_summary <- model$results
      RPART$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      RPART$pred_prob <- predictions
      
    }, error=function(e){})
    
    print("finished RPART") 
    
    
    # RPART2 with caret package ####
    
    RPART2 <- NULL
    
    tryCatch({
      
      # Fit RPART
      RPART2$outcomeName <- outcomeName
      
      model <- caret::train(formula, 
                            data = train[,c(outcomeName,predictorsNames)],  
                            trControl = myControl,
                            # tuneGrid = Grid,
                            method = "rpart2",
                            maximize=FALSE,
                            na.action = na.pass)
      RPART2$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      RPART2$train_results_summary <- model$results
      RPART2$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      RPART2$pred_prob <- predictions
      
    }, error=function(e){})
    
    print("finished RPART2") 
    
    
    # TREEBAG with caret package ####
    
    TREEBAG <- NULL
    
    tryCatch({
      
      # Fit RPART
      TREEBAG$outcomeName <- outcomeName
      
      model <- caret::train(formula, 
                            data = train[,c(outcomeName,predictorsNames)],  
                            trControl = myControl,
                            # tuneGrid = Grid,
                            method = "treebag",
                            maximize=FALSE,
                            na.action = na.pass)
      TREEBAG$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      TREEBAG$train_results_summary <- model$results
      TREEBAG$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      TREEBAG$pred_prob <- predictions
      
    }, error=function(e){})
    
    print("finished TREEBAG")
    
    
    # BAGEARTH with caret package ####
    
    BAGEARTH <- NULL
    
    tryCatch({
      
      # Fit RPART
      BAGEARTH$outcomeName <- outcomeName
      
      model <- caret::train(formula, 
                            data = train[,c(outcomeName,predictorsNames)],  
                            trControl = myControl,
                            # tuneGrid = Grid,
                            method = "bagEarth",
                            maximize=FALSE,
                            na.action = na.pass)
      BAGEARTH$model <- model
      
      # probabilities
      predictions <- predict(object=model, test)
      observations <- unlist(test[,outcomeName])
      BAGEARTH$train_results_summary <- model$results
      BAGEARTH$test_results_summary <- postResample(pred=predictions, obs=test$Prop)
      BAGEARTH$pred_prob <- predictions
      
    }, error=function(e){})
    
    print("finished BAGEARTH")
    
    
    # Collect models ####
    
    MODELS <- list(LM = LM, GLM = GLM, GBM = GBM, GLMNET = GLMNET, RF = RF, XGBOOST = XGBOOST, RPART = RPART, RPART2 = RPART2, TREEBAG = TREEBAG, BAGEARTH = BAGEARTH)
    
    print("collect models")
    
    # Compare models ####
    
    COMPARE <- NULL
    
    tryCatch({
      # Create model_list
      model_list <- list(LM = MODELS$LM$model, GBM = MODELS$GBM$model, GLM = MODELS$GLM$model, RF = MODELS$RF$model, XGBOOST = MODELS$XGBOOST$model, RPART = MODELS$RPART$model, RPART2 = MODELS$RPART2$model, TREEBAG = MODELS$TREEBAG$model, BAGEARTH = MODELS$BAGEARTH$model)
      
      # Pass model_list to resamples(): resamples
      resamples <- resamples(model_list)
      COMPARE$resamples <- resamples
      
      # Summarize the results
      COMPARE$resamples_summary <- summary(resamples)
      
      # Create bwplot
      # In general, you want the model with the higher median AUC, as well as a smaller range between min and max AUC.
      COMPARE$bwplot <- bwplot(resamples, metric = "RMSE") 
      
      # Create xyplot
      # It's particularly useful for identifying if one model is consistently better than the other across all folds, or if there are situations when the inferior model produces better predictions on a particular subset of the data.
      COMPARE$bwplot <- xyplot(resamples, metric = "RMSE")
    }, error=function(e){})
    
    # Identify BEST model ####
    
    BEST <- NULL
    
    tryCatch({
      # min mean train RMSE
      value <- COMPARE$resamples_summary$statistics$RMSE[,"Mean"]
      min_RMSE_train <- names(which(value == min(value,na.rm=T)))
      
      # min RMSE spread
      spread <- COMPARE$resamples_summary$statistics$RMSE[,"3rd Qu."] - COMPARE$resamples_summary$statistics$RMSE[,"1st Qu."]
      min_RMSEspread_train <- names(which(spread == min(spread[spread != 0],na.rm=T)))
      
      # max mean train Rsquared
      value <- COMPARE$resamples_summary$statistics$Rsquared[,"Mean"]
      max_Rsquared_train <- names(which(value == max(value,na.rm=T)))
      
      # min Rsquared spread
      spread <- COMPARE$resamples_summary$statistics$Rsquared[,"3rd Qu."] - COMPARE$resamples_summary$statistics$Rsquared[,"1st Qu."]
      min_Rsquaredspread_train <- names(which(spread == min(spread[spread != 0],na.rm=T)))
      
      # min test RMSE
      value <- c("LM"=as.vector(MODELS$LM$test_results_summary["RMSE"]), "GLM"=as.vector(MODELS$GLM$test_results_summary["RMSE"]), "GBM"=as.vector(MODELS$GBM$test_results_summary["RMSE"]), "RF"=as.vector(MODELS$RF$test_results_summary["RMSE"]), "XGBOOST"=as.vector(MODELS$XGBOOST$test_results_summary["RMSE"]), "RPART"=as.vector(MODELS$RPART$test_results_summary["RMSE"]), "RPART2"=as.vector(MODELS$RPART2$test_results_summary["RMSE"]), "TREEBAG"=as.vector(MODELS$TREEBAG$test_results_summary["RMSE"]), "BAGEARTH"=as.vector(MODELS$BAGEARTH$test_results_summary["RMSE"]))
      min_RMSE_test <- names(which(value == min(value,na.rm=T)))
      
      # min test Rsquared
      value <- c("LM"=as.vector(MODELS$LM$test_results_summary["Rsquared"]), "GLM"=as.vector(MODELS$GLM$test_results_summary["Rsquared"]), "GBM"=as.vector(MODELS$GBM$test_results_summary["Rsquared"]), "RF"=as.vector(MODELS$RF$test_results_summary["Rsquared"]), "XGBOOST"=as.vector(MODELS$XGBOOST$test_results_summary["Rsquared"]), "RPART"=as.vector(MODELS$RPART$test_results_summary["Rsquared"]), "RPART2"=as.vector(MODELS$RPART2$test_results_summary["Rsquared"]), "TREEBAG"=as.vector(MODELS$TREEBAG$test_results_summary["Rsquared"]), "BAGEARTH"=as.vector(MODELS$BAGEARTH$test_results_summary["Rsquared"]))
      max_Rsquared_test <- names(which(value == max(value,na.rm=T)))
      
      BEST <- list(min_RMSE_train = min_RMSE_train, min_RMSEspread_train = min_RMSEspread_train, max_Rsquared_train = max_Rsquared_train, min_Rsquaredspread_train = min_Rsquaredspread_train, min_RMSE_test = min_RMSE_test, max_Rsquared_test = max_Rsquared_test)
    }, error=function(e){})
    
    # Return Results ####
    return(list(DATA = DATA, CONTROL = CONTROL, MODELS = MODELS, COMPARE = COMPARE, BEST = BEST))
    
    
  }
}