# Steps to make covariates to predict from:
#
# 1. Create shapefile of whatever the spatial extent is you want (all boundaries have to be dissolved so the quadpolygon_IDs don't get repeated when they fall across boundaries) (D:\Fine scale vegetation analysis\understory_veg_model\functions\select_feature_from_geodatabase.py code)
# 2. Use python code to extract covariates for all ecog polygons that the shapefile overlays 
#     # script to use is currently (D:\Fine scale vegetation analysis\understory_veg_model\functions\extract_covariate_values_to_dbf.py)
# 3. Combine the dbf files created by python code into one datafile
      # a. script to use is currently (D:\Fine scale vegetation analysis\Erin Rcode\veg_model\functions\combine_covariate_dbfs.R)
      # b. use script combine_covariate_csv.R when using covariates from an entire 100k quad (D:\Fine scale vegetation analysis\understory_veg_model\functions\combine_covariate_csv.R)  
# 4. Use this file to prepare the covariates so they can be fed into the model predicting functions (e.g. Predict_models_template.R)

# load libraries
require(tidyverse)
require(here)

# load functions
source(here(file.path("functions","pred_prep_covariates.R")))

# input
filepath <- "//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/data" # location of covariates for area you want to predict from
# cov_files_list <- list.files(filepath) %>%
#   stringr::str_subset("^(?=.*dbf)(?!.*xml)")
cov_files_list <- "Units_1_6_10A_pred_cov_dbf_all.RData" # .RData file of covariates extracted for the area you want to predict from

# loop through a list of files
RESULTS <- COV <- IDS <- list()
for (k in 1:length(cov_files_list)){
  RESULTS[[k]] <- pred_prep_covariates(filepath = filepath,
                                       cov_files = cov_files_list[k],
                                       filetype = "dbf") # "dbf" if a merged dbf creation, "csv" if just a full 100k polygon set
} 

# extract
for (i in 1:length(RESULTS)){
  COV[[k]] <- RESULTS[[k]]$pred_data
  IDS[[k]] <- RESULTS[[k]]$pred_data_ids
}

# collapse
pred_data <- dplyr::bind_rows(COV) # remove this to go one 100k grouping at a time
pred_data_ids <- c(unlist(IDS))

# missing data
miss_rows <- pred_data %>%
  dplyr::rowwise() %>%
  do(data.frame("is_NA"=anyNA(.))) %>%
  pull(is_NA) %>%
  which()

save(list=c("pred_data","pred_data_ids","miss_rows"),file=here(file.path("data","Prepare_Covariates_Prediction.RData")))
