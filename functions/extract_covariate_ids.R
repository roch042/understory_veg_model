extract_covariate_ids <- function(filepath = filepath,
                        cov_files_list = cov_files_list)
  
{
  
  # load libraries
  require(tidyverse)
  require(here)
  
  # load functions
  source(here(file.path("functions","pred_prep_covariates.R")))
  
  cov_files_list = list.files(filepath) %>%
    stringr::str_subset("^(?=.*dbf)(?!.*xml)")
  
  # loop through a list of files
  RESULTS <- COV <- IDS <- list()
  for (k in 1:length(cov_files_list)){
    RESULTS[[k]] <- pred_prep_covariates(filepath = filepath,
                                         cov_files = cov_files_list[k])
  } 
  
  # extract
  for (i in 1:length(RESULTS)){
    COV[[k]] <- RESULTS[[k]]$pred_data
    IDS[[k]] <- RESULTS[[k]]$pred_data_ids
  }
  
  # collapse
  pred_data <- dplyr::bind_rows(COV) # remove this to go one 100k grouping at a time
  pred_data_ids <- c(unlist(IDS))
  
  return(list(pred_data=pred_data,
              pred_data_ids=pred_data_ids))
  
}