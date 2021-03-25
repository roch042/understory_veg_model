# load libraries ####
require(tidyverse)
require(stringr)
require(here)
require(caret)
require(pROC)
require(rpart)
require(ranger)
require(caTools)
require(skimr)

# load model ####
load(here(file.path("data","FORAGE.RData"))) # species listed as forage species (deer, elk, and sage grouse)
folder.list <- c("species","genus","subspecies") #  c("species","genus","subspecies")
type.list <- c("presence","percent_cover") # percent_cover, presence
# title <- paste(c("Vaccinium_membranaceum_NA","Vaccinium_uliginosum_NA","Vaccinium_scoparium_NA","Vaccinium_cespitosum_NA","Prunus_virginiana_NA","Prunus_emarginata_NA","Crataegus_douglasii_NA","Amelanchier_alnifolia_NA"),collapse="|")


for(k in 1:length(type.list)){
  
  type <- type.list[k]
  
  for (j in 1:length(folder.list)){
    
    folder <- folder.list[j] # folder
    mod_files <- list.files(here(file.path("models",folder,type)))
    # mod_files <- mod_files[which(grepl(title, mod_files))]
    # i <- which(grepl("Pseudoroegneria_spicata",mod_files))
    # i <- 5
    
    # PRESENCE ####
    if(type == "presence"){
      
      # Genus ####
      
      if(folder == "genus"){
        
        MODEL_summary <- list()
        Model <- FORAGE$ModelCode1
        file_title <- paste(Model$Genus,sep="")
        
        for (i in 1:length(mod_files)){
          # Load Model ####
          load(here(file.path("models",folder,type,mod_files[i])))
          if( length(RES) == 1 ) {
            MODEL_summary[[i]] <- data.frame(Name = gsub("_NA_NA.RData","",mod_files[i]), EnoughData = "No", BestModel = NA, TestAUC = NA, SampleSize = NA)
          } else {
            MODEL_summary[[i]] <- data.frame(Name = gsub("_NA_NA.RData","",mod_files[i]),
                                             EnoughData = "Yes", 
                                             BestModel = ifelse(length(RES$BEST$max_AUC_test) < 0,NA, paste(RES$BEST$max_AUC_test,collapse=",")), 
                                             TestAUC = ifelse(length(RES$BEST$max_AUC_test)>1 | length(RES$BEST$max_AUC_test) <= 0,NA,RES$MODELS[[RES$BEST$max_AUC_test]]$auc$auc),
                                             SampleSize = sum(RES$DATA$total$Present))
          }
          gc()
          print(paste("iteration",i,"of",length(mod_files),"complete",sep=" "))
        }
        
        MODEL_summary_genus <- dplyr::bind_rows(MODEL_summary) %>%
          dplyr::right_join(data.frame(Name = file_title)) %>%
          dplyr::mutate(EnoughData = ifelse(is.na(EnoughData),"No",as.character(EnoughData)))
        
      }
      
      
      # Species ####
      
      if(folder == "species"){
        
        MODEL_summary <- list()
        Model <- FORAGE$ModelCode2
        file_title <- paste(Model$Genus,"_",Model$Species,sep="")
        
        for (i in 1:length(mod_files)){
          # Load Model ####
          load(here(file.path("models",folder,type,mod_files[i])))
          if( length(RES) == 1 ) {
            MODEL_summary[[i]] <- data.frame(Name = gsub("_NA.RData","",mod_files[i]), EnoughData = "No", BestModel = NA, TestAUC = NA, SampleSize = NA)
          } else {
            MODEL_summary[[i]] <- data.frame(Name = gsub("_NA.RData","",mod_files[i]),
                                             EnoughData = "Yes", 
                                             BestModel = ifelse(length(RES$BEST$max_AUC_test) < 0,NA, paste(RES$BEST$max_AUC_test,collapse=",")), 
                                             TestAUC = ifelse(length(RES$BEST$max_AUC_test)>1 | length(RES$BEST$max_AUC_test) <= 0,NA,RES$MODELS[[RES$BEST$max_AUC_test]]$auc$auc),
                                             SampleSize = sum(RES$DATA$total$Present))
          }
          gc()
          print(paste("iteration",i,"of",length(mod_files),"complete",sep=" "))
        }
        
        MODEL_summary_species <- dplyr::bind_rows(MODEL_summary) %>%
          dplyr::right_join(data.frame(Name = file_title)) %>%
          dplyr::mutate(EnoughData = ifelse(is.na(EnoughData),"No",as.character(EnoughData)))
        
      }
      
      
      # SubSpecies ####
      
      if(folder == "subspecies"){
        
        MODEL_summary <- list()
        Model <- FORAGE$ModelCode3
        file_title <- paste(Model$Genus,"_",Model$Species,"_",Model$SubSpecies,sep="")
        
        for (i in 1:length(mod_files)){
          # Load Model ####
          load(here(file.path("models",folder,type,mod_files[i])))
          if( length(RES) == 1 ) {
            MODEL_summary[[i]] <- data.frame(Name = gsub(".RData","",mod_files[i]), EnoughData = "No", BestModel = NA, TestAUC = NA, SampleSize = NA)
          } else {
            MODEL_summary[[i]] <- data.frame(Name = gsub(".RData","",mod_files[i]),
                                             EnoughData = "Yes", 
                                             BestModel = ifelse(length(RES$BEST$max_AUC_test) < 0,NA, paste(RES$BEST$max_AUC_test,collapse=",")), 
                                             TestAUC = ifelse(length(RES$BEST$max_AUC_test)>1 | length(RES$BEST$max_AUC_test) <= 0,NA,RES$MODELS[[RES$BEST$max_AUC_test]]$auc$auc),
                                             SampleSize = sum(RES$DATA$total$Present))
          }
          gc()
          print(paste("iteration",i,"of",length(mod_files),"complete",sep=" "))
        }
        
        MODEL_summary_subspecies <- dplyr::bind_rows(MODEL_summary) %>%
          dplyr::right_join(data.frame(Name = file_title)) %>%
          dplyr::mutate(EnoughData = ifelse(is.na(EnoughData),"No",as.character(EnoughData)))
        
      }
      
    } else {
      
      # PERCENT COVER ####
      if(type == "percent_cover"){
        
        # Genus ####
        
        if(folder == "genus"){
          
          MODEL_summary <- list()
          Model <- FORAGE$ModelCode1
          file_title <- paste(Model$Genus,sep="")
          
          for (i in 1:length(mod_files)){
            # Load Model ####
            load(here(file.path("models",folder,type,mod_files[i])))
            if( length(RES) == 1 ) {
              MODEL_summary[[i]] <- data.frame(Name = gsub("_NA_NA.RData","",mod_files[i]), EnoughData = "No", BestModel_RMSE = NA, BestModel_Rsquared = NA, Best_RMSE_TestRMSE = NA, Best_RMSE_TestRsquared = NA, Best_Rsquared_TestRMSE = NA, Best_Rsquared_TestRsquared = NA, SampleSize = NA)
            } else {
              MODEL_summary[[i]] <- data.frame(Name = gsub("_NA_NA.RData","",mod_files[i]),
                                               EnoughData = "Yes", 
                                               BestModel_RMSE = paste(RES$BEST$min_RMSE_test,collapse=","), 
                                               BestModel_Rsquared = paste(RES$BEST$max_Rsquared_test,collapse=","), 
                                               Best_RMSE_TestRMSE = ifelse(length(RES$BEST$min_RMSE_test)>1 | length(RES$BEST$min_RMSE_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$min_RMSE_test]]$test_results_summary["RMSE"])),
                                               Best_RMSE_TestRsquared = ifelse(length(RES$BEST$min_RMSE_test)>1 | length(RES$BEST$min_RMSE_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$min_RMSE_test]]$test_results_summary["Rsquared"])),
                                               Best_Rsquared_TestRMSE = ifelse(length(RES$BEST$max_Rsquared_test)>1 | length(RES$BEST$max_Rsquared_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$max_Rsquared_test]]$test_results_summary["RMSE"])),
                                               Best_Rsquared_TestRsquared = ifelse(length(RES$BEST$max_Rsquared_test)>1 | length(RES$BEST$max_Rsquared_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$max_Rsquared_test]]$test_results_summary["Rsquared"])),
                                               SampleSize = sum(RES$DATA$total$Present))
            }
            gc()
            print(paste("iteration",i,"of",length(mod_files),"complete",sep=" "))
          }
          
          MODEL_summary_genus <- dplyr::bind_rows(MODEL_summary) %>%
            dplyr::right_join(data.frame(Name = file_title)) %>%
            dplyr::mutate(EnoughData = ifelse(is.na(EnoughData),"No",as.character(EnoughData)))
          
        }
        
        
        # Species ####
        
        if(folder == "species"){
          
          MODEL_summary <- list()
          Model <- FORAGE$ModelCode2
          file_title <- paste(Model$Genus,"_",Model$Species,sep="")
          
          for (i in 1:length(mod_files)){
            # Load Model ####
            load(here(file.path("models",folder,type,mod_files[i])))
            if( length(RES) == 1 ) {
              MODEL_summary[[i]] <- data.frame(Name = gsub("_NA.RData","",mod_files[i]), EnoughData = "No", BestModel_RMSE = NA, BestModel_Rsquared = NA, Best_RMSE_TestRMSE = NA, Best_RMSE_TestRsquared = NA, Best_Rsquared_TestRMSE = NA, Best_Rsquared_TestRsquared = NA, SampleSize = NA)
            } else {
              MODEL_summary[[i]] <- data.frame(Name = gsub("_NA.RData","",mod_files[i]),
                                               EnoughData = "Yes", 
                                               BestModel_RMSE = paste(RES$BEST$min_RMSE_test,collapse=","), 
                                               BestModel_Rsquared = paste(RES$BEST$max_Rsquared_test,collapse=","), 
                                               Best_RMSE_TestRMSE = ifelse(length(RES$BEST$min_RMSE_test)>1 | length(RES$BEST$min_RMSE_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$min_RMSE_test]]$test_results_summary["RMSE"])),
                                               Best_RMSE_TestRsquared = ifelse(length(RES$BEST$min_RMSE_test)>1 | length(RES$BEST$min_RMSE_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$min_RMSE_test]]$test_results_summary["Rsquared"])),
                                               Best_Rsquared_TestRMSE = ifelse(length(RES$BEST$max_Rsquared_test)>1 | length(RES$BEST$max_Rsquared_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$max_Rsquared_test]]$test_results_summary["RMSE"])),
                                               Best_Rsquared_TestRsquared = ifelse(length(RES$BEST$max_Rsquared_test)>1 | length(RES$BEST$max_Rsquared_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$max_Rsquared_test]]$test_results_summary["Rsquared"])),
                                               SampleSize = sum(RES$DATA$total$Present))
            }
            gc()
            print(paste("iteration",i,"of",length(mod_files),"complete",sep=" "))
          }
          
          MODEL_summary_species <- dplyr::bind_rows(MODEL_summary) %>%
            dplyr::right_join(data.frame(Name = file_title)) %>%
            dplyr::mutate(EnoughData = ifelse(is.na(EnoughData),"No",as.character(EnoughData)))
          
        }
        
        
        # SubSpecies ####
        
        if(folder == "subspecies"){
          
          MODEL_summary <- list()
          Model <- FORAGE$ModelCode3
          file_title <- paste(Model$Genus,"_",Model$Species,"_",Model$SubSpecies,sep="")
          
          for (i in 1:length(mod_files)){
            # Load Model ####
            load(here(file.path("models",folder,type,mod_files[i])))
            if( length(RES) == 1 ) {
              MODEL_summary[[i]] <- data.frame(Name = gsub(".RData","",mod_files[i]), EnoughData = "No", BestModel_RMSE = NA, BestModel_Rsquared = NA, Best_RMSE_TestRMSE = NA, Best_RMSE_TestRsquared = NA, Best_Rsquared_TestRMSE = NA, Best_Rsquared_TestRsquared = NA, SampleSize = NA)
            } else {
              MODEL_summary[[i]] <- data.frame(Name = gsub(".RData","",mod_files[i]),
                                               EnoughData = "Yes", 
                                               BestModel_RMSE = paste(RES$BEST$min_RMSE_test,collapse=","), 
                                               BestModel_Rsquared = paste(RES$BEST$max_Rsquared_test,collapse=","), 
                                               Best_RMSE_TestRMSE = ifelse(length(RES$BEST$min_RMSE_test)>1 | length(RES$BEST$min_RMSE_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$min_RMSE_test]]$test_results_summary["RMSE"])),
                                               Best_RMSE_TestRsquared = ifelse(length(RES$BEST$min_RMSE_test)>1 | length(RES$BEST$min_RMSE_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$min_RMSE_test]]$test_results_summary["Rsquared"])),
                                               Best_Rsquared_TestRMSE = ifelse(length(RES$BEST$max_Rsquared_test)>1 | length(RES$BEST$max_Rsquared_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$max_Rsquared_test]]$test_results_summary["RMSE"])),
                                               Best_Rsquared_TestRsquared = ifelse(length(RES$BEST$max_Rsquared_test)>1 | length(RES$BEST$max_Rsquared_test)==0,NA,as.vector(RES$MODELS[[RES$BEST$max_Rsquared_test]]$test_results_summary["Rsquared"])),
                                               SampleSize = sum(RES$DATA$total$Present))
            }
            gc()
            print(paste("iteration",i,"of",length(mod_files),"complete",sep=" "))
          }
          
          MODEL_summary_subspecies <- dplyr::bind_rows(MODEL_summary) %>%
            dplyr::right_join(data.frame(Name = file_title)) %>%
            dplyr::mutate(EnoughData = ifelse(is.na(EnoughData),"No",as.character(EnoughData)))
          
        }  
        
      }
      
    }
    
  }
  
  save(list=c("MODEL_summary_genus","MODEL_summary_species","MODEL_summary_subspecies"),file=here("results",file.path(paste(type,"_summary_trained_models.RData",sep=""))))
  
}
# save(list=c("MODEL_summary_species"),file=here("results",file.path(paste(type,"_summary_trained_models.RData",sep=""))))

