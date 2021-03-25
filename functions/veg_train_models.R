veg_train_models <- function(DAT=DAT)
{
  
  #~~~~~~~~~~~~~~~~~~~~~#
  # MACHINE LEARNING ####
  #~~~~~~~~~~~~~~~~~~~~~#
  
  
  # Create training and test data ####
  Total <- dplyr::bind_rows(DAT$PTS,DAT$PLOTS) %>%
    dplyr::ungroup() %>%
    dplyr::select(-QuadPoly_ID,-ECOCODE) %>%
    dplyr::select(-nass,-tpi,-dev,-tsf,-ff,-sc) # exclude categorical covariates
  
  
  # divide for presence  vs. proportion here ####
  
  if(length(unique(Total$Present))==1 | table(Total$Present)[2] < 3){
    return("not enough presence data collected")
  } else {
    # Create dummy variables for factors
    # TotalDummyModel <- caret::dummyVars("~.", data = Total, fullRank = F)
    # Total <- as.data.frame(predict(TotalDummyModel,Total))
    # Use prepare() to one-hot-encode testframe
    # (testframe.treat <- vtreat::prepare(treatplan, testframe, varRestriction = newvars))
    
    # extra variables
    extraNames <- c("QuadPoly_ID","Total","Prop","OBJECTID","quad","Shape_Length","Shape_Area","mean_blue","mean_green","mean_red","mean_nir","sd_blue","sd_green","sd_red","sd_nir","Hit")
    
    # response variable
    outcomeName <- "Present"
    
    # covariates
    predictorsNames <- names(Total)[which(!names(Total) %in% c(extraNames,outcomeName))]
    
    # non-factor response variable for GLMNET
    Total$Present2 <- as.factor(ifelse(Total$Present == "1","Yes","No"))
    
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
    myFolds <- createFolds(1:nrow(train), k = 10)
    myControl <- trainControl(
      method = "cv",
      summaryFunction = twoClassSummary,
      classProbs = TRUE, # IMPORTANT!
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = myFolds
    )
    CONTROL <- list(myFolds = myFolds, myControl = myControl)
    
    # GLM with caret package ####
    GLM <- NULL
    
    tryCatch({
      # Fit glm model: model
      outcomeName <- "Present2"
      GLM$outcomeName <- outcomeName
      
      model <- caret::train(Present2 ~., 
                            data = train[,c(outcomeName,predictorsNames)], 
                            method = "glm", 
                            trControl = myControl,
                            metric = "ROC",
                            preProc = c("center","scale"),
                            na.action = na.pass)
      GLM$model <- model
      
      # Print model to console
      # ( model )
      # summary(model) # shows variables of most importance
      
      # accuracy score
      predictions <- predict(object=model, test, type='raw')
      GLM$accuracy <- postResample(pred=predictions, obs=as.factor(test$Present2))
      GLM$confusion <- confusionMatrix(predictions, test[["Present2"]])
      GLM$pred_raw <- predictions
      
      # probabilities
      predictions <- predict(object=model, test, type='prob')
      auc <- roc(ifelse(test$Present2=="Yes",1,0), predictions[[2]])
      GLM$auc <- auc
      GLM$ROCplot <- plot(auc) # look at ROC curve
      GLM$pred_prob <- predictions
    }, error=function(e){})
    print("finished GLM")
    
    # GBM with caret package ####
    
    GBM <- NULL
    
    tryCatch({
    # Create trainControl object: myControl
    # myControl <- trainControl(
    #   method = "cv",
    #   number = 3,
    #   summaryFunction = twoClassSummary,
    #   classProbs = TRUE, # IMPORTANT!
    #   verboseIter = TRUE,
    #   returnResamp = "none"
    # )
    
    # Train gbm with custom trainControl: model
    outcomeName <- "Present2"
    GBM$outcomeName <- outcomeName
    
    model <- train(Present2 ~., 
                   data = train[,c(outcomeName,predictorsNames)], 
                   method = "gbm", 
                   trControl = myControl,
                   metric = "ROC",
                   preProc = c("center","scale"),
                   na.action = na.pass)
    GBM$model <- model
    
    # Print model to console
    # ( model )
    # summary(model) # shows variables of most importance
    
    # accuracy score
    predictions <- predict(object=model, test, type='raw')
    GBM$accuracy <- postResample(pred=predictions, obs=as.factor(test$Present2))
    GBM$confusion <- confusionMatrix(predictions, test[["Present2"]])
    GBM$pred_raw <- predictions
    
    # probabilities
    predictions <- predict(object=model, test, type='prob')
    auc <- roc(ifelse(test$Present2=="Yes",1,0), predictions[[2]])
    GBM$auc <- auc
    GBM$ROCplot <- plot(auc) # look at ROC curve
    GBM$pred_prob <- predictions
    }, error=function(e){})
    print("finished GBM")    
    
    # GLMNET with caret package ####
    
    GLMNET <- NULL
    
    tryCatch({
    # Create trainControl object: myControl
    # myControl <- trainControl(
    #   method = "cv",
    #   number = 3,
    #   returnResamp = "none"
    # )
    
    # Train glmnet with custom trainControl: model
    outcomeName <- "Present"
    GLMNET$outcomeName <- outcomeName
    
    myControlReg <- trainControl(
      method = "cv",
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = myFolds
    )

      model <- train(Present ~., 
                     data = train[,c(outcomeName,predictorsNames)], 
                     method = "glmnet", 
                     trControl = myControlReg,
                     metric = "RMSE",
                     preProc = c("center","scale"),
                     na.action = na.pass)
      GLMNET$model <- model
    
    # Print model to console
    # ( model )
    # summary(model) 
    
    # probabilities
    predictions <- predict(object=model, test)
    auc <- roc(test$Present, predictions)
    GLMNET$auc <- auc
    GLMNET$ROCplot <- plot(auc) # look at ROC curve
    GLMNET$pred_prob <- predictions
    # plot(varImp(model,scale=F))
    }, error=function(e){}) 
    print("finished GLMNET")    
    
    # Random forest model with caret ####
    
    RF <- NULL
    
    tryCatch({
    # Fit random forest: model
    outcomeName <- "Present2"
    RF$outcomeName <- outcomeName
    
    # Create trainControl object: myControl
    # myControl <- trainControl(
    #   method = "cv", 
    #   verboseIter = TRUE,
    #   summaryFunction=twoClassSummary,
    #   classProbs = TRUE,
    #   savePredictions = TRUE
    # )
    model <- train(
      Present2 ~ .,
      tuneLength = 3,
      data = train[,c(outcomeName,predictorsNames)],  
      method = "ranger",
      preProc = c("center","scale"),
      trControl = myControl,
      na.action = na.pass
    )
    RF$model <- model
    
    # Print model to console
    # ( model )
    # plot(model)
    
    # accuracy score
    predictions <- predict(object=model, test, type='raw')
    RF$accuracy <- postResample(pred=predictions, obs=as.factor(test$Present2))
    RF$confusion <- confusionMatrix(predictions, test[["Present2"]])
    RF$pred_raw <- predictions
    
    # probabilities
    predictions <- predict(object=model, test, type='prob')
    auc <- roc(ifelse(test$Present2=="Yes",1,0), predictions[[2]])
    RF$auc <- auc
    RF$ROCplot <- plot(auc) # look at ROC curve
    RF$pred_prob <- predictions
    }, error=function(e){}) 
    print("finished RF")   
    
    # GLMNET classification model with caret ####
    
    GLMNET_class <- NULL
    
    tryCatch({
    # Fit GLMNET classification model with caret
    outcomeName <- "Present2"
    GLMNET_class$outcomeName <- outcomeName
    
    # Create trainControl object: myControl
    # myControl <- trainControl(
    #   method = "cv", 
    #   number = 10,
    #   summaryFunction = twoClassSummary,
    #   classProbs = TRUE, # IMPORTANT!
    #   verboseIter = TRUE
    # )
    
    model <- train(
      Present2 ~ ., 
      data = train[,c(outcomeName,predictorsNames)],  
      tuneGrid = expand.grid(
        alpha = 0:1,
        lambda = seq(0.0001, 1, length = 20)
      ),
      method = "glmnet",
      trControl = myControl,
      na.action = na.pass
    )
    GLMNET_class$model <- model
    
    # Print model to console
    # ( model )
    # plot(model)
    
    # accuracy score
    predictions <- predict(object=model, test, type='raw')
    GLMNET_class$accuracy <- postResample(pred=predictions, obs=as.factor(test$Present2))
    GLMNET_class$confusion <- confusionMatrix(predictions, test[["Present2"]])
    GLMNET_class$pred_raw <- predictions
    
    # probabilities
    predictions <- predict(object=model, test, type='prob')
    auc <- roc(ifelse(test$Present2=="Yes",1,0), predictions[[2]])
    GLMNET_class$auc <- auc
    GLMNET_class$ROCplot <- plot(auc) # look at ROC curve
    GLMNET_class$pred_prob <- predictions
    
    # Print maximum ROC statistic
    # ( max(model[["results"]]$ROC) )
    }, error=function(e){})
    print("finished GLMNET_class")   
    
    # XGBOOST with caret package ####
    
    XGBOOST <- NULL
    
    tryCatch({
    # Fit XGBOOST
    outcomeName <- "Present2"
    XGBOOST$outcomeName <- outcomeName
    
    grid_default <- expand.grid(
      nrounds = 100,
      max_depth = 6,
      eta = 0.3,
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    # train_control <- caret::trainControl(
    #   method = "none",
    #   verboseIter = FALSE, # no training log
    #   allowParallel = TRUE # FALSE for reproducible results 
    # )
    model <- caret::train(
      Present2 ~ ., 
      data = train[,c(outcomeName,predictorsNames)],  
      trControl = myControl,
      tuneGrid = grid_default,
      method = "xgbTree",
      verbose = TRUE,
      na.action = na.pass
    )
    XGBOOST$model <- model
    
    # Print model to console
    # ( model )
    # plot(model)
    
    # accuracy score
    predictions <- predict(object=model, test, type='raw')
    XGBOOST$accuracy <- postResample(pred=predictions, obs=as.factor(test$Present2))
    XGBOOST$confusion <- confusionMatrix(predictions, test[["Present2"]])
    XGBOOST$pred_raw <- predictions
    
    # probabilities
    predictions <- predict(object=model, test, type='prob')
    auc <- roc(ifelse(test$Present2=="Yes",1,0), predictions[[2]])
    XGBOOST$auc <- auc
    XGBOOST$ROCplot <- plot(auc) # look at ROC curve
    XGBOOST$pred_prob <- predictions
    }, error=function(e){})
    print("finished XGBOOST")   
    
    
    # Collect models ####
    
    MODELS <- list(GLM = GLM, GBM = GBM, GLMNET = GLMNET, RF = RF, GLMNET_class = GLMNET_class, XGBOOST = XGBOOST)
    print("collect models")
    
    # Compare models ####
    
    COMPARE <- NULL
    
    tryCatch({
    # Create model_list
    model_list <- list(GBM = MODELS$GBM$model, GLM = MODELS$GLM$model, GLMNET_class = MODELS$GLMNET_class$model, RF = MODELS$RF$model, XGBOOST = MODELS$XGBOOST$model)
    
    # Pass model_list to resamples(): resamples
    resamples <- resamples(model_list)
    COMPARE$resamples <- resamples
    
    # Summarize the results
    COMPARE$resamples_summary <- summary(resamples)
    
    # Create bwplot
    # In general, you want the model with the higher median AUC, as well as a smaller range between min and max AUC.
    COMPARE$bwplot <- bwplot(resamples, metric = "ROC") 
    
    # Create xyplot
    # It's particularly useful for identifying if one model is consistently better than the other across all folds, or if there are situations when the inferior model produces better predictions on a particular subset of the data.
    COMPARE$bwplot <- xyplot(resamples, metric = "ROC")
    }, error=function(e){})
    
    # Identify BEST model ####
    
    BEST <- NULL
    
    tryCatch({
    # maximum ROC
    max <- COMPARE$resamples_summary$statistics$ROC[,"Max."]
    max_ROC_caret <- names(which(max == max(max,na.rm=T)))
    
    # minimum ROC spread
    spread <- COMPARE$resamples_summary$statistics$ROC[,"3rd Qu."] - COMPARE$resamples_summary$statistics$ROC[,"1st Qu."]
    min_ROCspread_caret <- names(which(spread == min(spread[spread != 0],na.rm=T)))
    
    # max auc
    auc_vector <- c("GLM"=MODELS$GLM$auc$auc,"GBM"=MODELS$GBM$auc$auc,"GLMNET"=MODELS$GLMNET$auc$auc,"RF"=MODELS$RF$auc$auc,"GLMNET_class"=MODELS$GLMNET_class$auc$auc,"XGBOOST"=MODELS$XGBOOST$auc$auc)
    max_AUC_test <- names(which(auc_vector == max(auc_vector,na.rm=T)))
    
    BEST <- list(max_ROC_caret = max_ROC_caret, min_ROCspread_caret = min_ROCspread_caret, max_AUC_test = max_AUC_test)
    }, error=function(e){})
    
    # Return Results ####
    return(list(DATA = DATA, CONTROL = CONTROL, MODELS = MODELS, COMPARE = COMPARE, BEST = BEST))
    
  }
}