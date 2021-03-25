#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in the output tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("E:/idahoveg_database/output_tables")

files <- list.files(pattern="*.xls")



#~~~~~~~~~~~~~~~~~~~#
# Apply read.xls ####
#~~~~~~~~~~~~~~~~~~~#

myfiles <- purrr::map(files, ~ gdata::read.xls(., stringsAsFactors=FALSE))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Collapse list of output_tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

myfiles <- myfiles %>%
  purrr::map(function(x) {
    dplyr::mutate(x, TranKey = as.character(TranKey), PlotKey = as.character(PlotKey) )
  } 
  ) %>%
  dplyr::bind_rows()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create an 'index' file that gives the QuadPloy_ID for each point/plot in the field data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("C:/Users/eroche/Documents/Fine scale vegetation analysis")

FieldDataPoints_QuadPolyID <- myfiles %>% 
  dplyr::select(QuadPoly_ID,quad,PlotKey,TranKey) %>%
  dplyr::distinct()

save(FieldDataPoints_QuadPolyID, file="FieldDataPoints_QuadPolyID.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create an 'index' file that gives the QuadPloy_ID and the associated covariate values ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
Covariates_QuadPolyID <- myfiles %>%
  dplyr::select(-OBJECTID,-Join_Count,-TARGET_FID,-JOIN_FID,-Code,-SpName,-Mdl_N_R,-Lif_Frm,-MuleDer,-SageGrs,-Elk,-Moose,-PlotKey,-TranKey,-id,-Source,-Smpl_Yr,-Shape_Length,-Shape_Area,-Easting,-Northng, -ScntfcN, -Species, -Prcnt_C, -Plot_Ar) %>%
  dplyr::distinct()

save(Covariates_QuadPolyID, file="Covariates_QuadPolyID.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Add Scott's reclassified data to Covariates_QuadPolyID ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

SD <- readr::read_csv("K:/Wildlife/Wildlife Research/Fine_scale_vegetation_2017/Bergen/polygon_composition_by_class_for_field_data/fieldpoly_cls_3_26_2018.csv") %>%
  dplyr::tbl_df()

Covariates_QuadPolyID <- dplyr::left_join(Covariates_QuadPolyID,SD,by=c("QuadPoly_ID"="QUADPOLY_ID"))

save(Covariates_QuadPolyID, file="Covariates_QuadPolyID.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Save compiled output_table files ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

save(myfiles, file="Compiled_Output_Tables.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Add Scott's reclassified data to Covariates_QuadPolyID - March 25, 2019 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# remove old scott data
load("K:/Wildlife/Wildlife Research/Fine_scale_vegetation_2017/veg_jags_model/data/Covariates_QuadPolyID_old.RData")

Covariates_QuadPolyID <- Covariates_QuadPolyID %>%
  dplyr::select(-water,-shadow,-bare,-mgrass,-xgrass,-mshrub,-xshrub,-conifer,-broadleaf,-agriculture,-developed)

SD <- readr::read_csv("K:/Wildlife/Wildlife Research/Fine_scale_vegetation_2017/Bergen/allfield_wclasses.csv") %>%
  dplyr::tbl_df() %>%
  dplyr::select(OBJECTID, QuadPoly_ID, Shape_Length, Shape_Area, water_m2, shadow_m2, bareground_m2, mgrass_m2, xgrass_m2, mshrub_m2, xshrub_m2, conifer_m2, broadleaf_m2, agriculture_m2, developed_m2)

# missing_QuadPolyID <- Covariates_QuadPolyID$QuadPoly_ID[which(!Covariates_QuadPolyID$QuadPoly_ID %in% SD$QuadPoly_ID)] %>%
#   dplyr::tbl_df()
# 
# readr::write_csv(missing_QuadPolyID,"missing_QuadPolyID_25March2019.csv")

Covariates_QuadPolyID <- dplyr::left_join(Covariates_QuadPolyID,SD,by=c("QuadPoly_ID"="QuadPoly_ID"))

save(Covariates_QuadPolyID, file="Covariates_QuadPolyID.RData")