veg_data <- function(genus = "Populus",
                     species = "angustifolia",
                     subspecies = NA,
                     fielddata = "FieldData_14Sept2020.csv") {
 
  # - the following defines function code - #

  # NEED TO GO THROUGH AND FIX THE 'SHAPE AREA' COLUMN USED TO CALCULATE PERCENT COVER!!!!
  
  # Load Data ####
  # load(here(file.path("data","FieldData.RData")))
  # X <- read.csv(here(file.path("data","FieldData","FieldData_19Nov2019.csv")))
  # x <- read.csv(here(file.path("data","FieldData",fielddata)))
  X <- fielddata
  
  # Add Eco Regions and QuadPolyID ####
  # X <- assign_ecoregions(X)
  X <- X %>%
    dplyr::left_join(EcoCodes[,c("QuadPoly_ID","quad","PlotKey","TranKey","Shape_Length","Shape_Area","ECOCODE")])
  
  # Create Modeling Groups #### 
  LpiCov <- X %>%
    tidyr::separate(ModelCode, c("G1","G2","G3","G4"), sep=" ", remove = FALSE) %>%
    dplyr::select(-G3) %>%
    dplyr::mutate(
      ModelCode1 = G1,
      ModelCode2 = stringr::str_c( G1, G2, sep = " " ),
      ModelCode3 = stringr::str_c( G1, G2, G4, sep = " " )) %>%
    dplyr::select(-G1, -G2, -G4) %>%
    dplyr::mutate( ModelCode1 = ifelse( ModelCode1 == "NO", ModelCode2, ModelCode1 )) %>%
    dplyr::mutate( ModelCode2 = ifelse(stringr::str_detect( ModelCode2, "sp\\."), NA, ModelCode2 ))
  
  # Create Response Variable ####
  LpiCov <- LpiCov %>%
    dplyr::mutate(
      ResponseVariable = if( is.na(subspecies) & is.na(species) & !is.na(genus) ){
        (ifelse( ModelCode1 == genus,
                       ifelse(DataType == "COVER", Prcnt_C, 1 ), 0 ))
      } else {
        if( is.na(subspecies) & !is.na(species) & !is.na(genus)){
          (ifelse( !is.na(ModelCode2) & ModelCode2 == stringr::str_c( genus, species, sep = " "), 
                         ifelse(DataType == "COVER", Prcnt_C, 1 ), 0 )) 
        } else {
          if( !is.na(subspecies) & !is.na(species) & !is.na(genus)){
            (ifelse( !is.na(ModelCode3) & ModelCode3 == stringr::str_c( genus, species, subspecies, sep = " "), 
                           ifelse(DataType == "COVER", Prcnt_C, 1 ), 0 )) 
          } else {
            (NA)
          }
        }
      }
    )

  
  # load covariate for models ####
  
  # brief editing of covariates
  Covariates_QuadPolyID <- Covariates_QuadPolyID %>%
    # dplyr::select( -lcv, -tpi, -nass, -dev, -mintp, -maxpr, -sasp, -tapr) %>%
    dplyr::mutate( ytsf = as.integer(format(Sys.Date(), "%Y")) - tsf, # years since most recent fire
                   ele2 = ele^2,
                   slp2 = slp^2,
                   casp2 = casp^2,
                   twi2 = twi^2,
                   sri2 = sri^2,
                   minpr2 = minpr^2,
                   maxtp2 = maxtp^2,
                   aws2 = aws^2,
                   clay2 = clay^2,
                   sand2 = sand^2,
                   silt2 = silt^2,
                   cec2 = cec^2,
                   d2r2 = d2r^2,
                   ph2 = ph^2,
                   om2 = om^2,
                   caco32 = caco3^2,
                   ytsf2 = ytsf^2) %>%
    dplyr::mutate(nass = as.factor(nass),
                  tpi = as.factor(tpi),
                  dev = as.factor(dev),
                  tsf = as.factor(tsf),
                  ff = as.factor(ff),
                  sc = as.factor(sc)) %>%
    dplyr::mutate(Real_Shape_Area = water_m2 + shadow_m2 + bareground_m2 + mgrass_m2 + xgrass_m2 + mshrub_m2 + xshrub_m2 + conifer_m2 + decid_m2 + agriculture_m2 + developed_m2) %>%
    dplyr::left_join(FieldDataPoints_QuadPolyID[,c("QuadPoly_ID","Shape_Area")]) %>%
    dplyr::mutate(water_m2 = water_m2/Real_Shape_Area,
                  shadow_m2 = shadow_m2/Real_Shape_Area,
                  bareground_m2 = bareground_m2/Real_Shape_Area,
                  mgrass_m2 = mgrass_m2/Real_Shape_Area,
                  xgrass_m2 = xgrass_m2/Real_Shape_Area,
                  mshrub_m2 = mshrub_m2/Real_Shape_Area,
                  xshrub_m2 = xshrub_m2/Real_Shape_Area,
                  conifer_m2 = conifer_m2/Real_Shape_Area,
                  decid_m2 = decid_m2/Real_Shape_Area,
                  agriculture_m2 = agriculture_m2/Real_Shape_Area,
                  developed_m2 = developed_m2/Real_Shape_Area)
  
  # if( !is.na(genus) & is.na(species) & is.na(subspecies) ){
  #   x.cov <- MC1 %>%
  #     dplyr::filter( ModelCode1 == genus )
  #   x.cov <-  c(names( x.cov )[which( x.cov == 1 )], "QuadPoly_ID" )
  # } else {
  #   if( !is.na(genus) & !is.na(species) & is.na(subspecies) ){
  #     x.cov <- MC2 %>%
  #       dplyr::filter( ModelCode2 == stringr::str_c( genus, species, sep = " " ))
  #     x.cov <-  c(names( x.cov )[which( x.cov == 1 )], "QuadPoly_ID" )
  #   } else {
  #     if( !is.na(subspecies) & !is.na(species) & !is.na(genus) ){
  #       x.cov <- MC3 %>%
  #         dplyr::filter( ModelCode3 == stringr::str_c( genus, species, subspecies, sep = " " ))
  #       x.cov <-  c(names( x.cov )[which( x.cov == 1 )], "QuadPoly_ID" )
  #     }
  #   }
  # }
  
  # select covariates and remove NAs ####
  # Covariates_QuadPolyID <-  Covariates_QuadPolyID %>%
  #   dplyr::select( dplyr::one_of( x.cov ))
  
  # remove NAs ####
  Covariates_QuadPolyID <-  Covariates_QuadPolyID %>%
    dplyr::select(-mean_blue,-mean_green,-mean_red,-mean_nir,-sd_blue,-sd_green,-sd_red,-sd_nir,quad,-Date,-INSIDE_X,-INSIDE_Y,-Shape_Area) %>%
    na.omit()
    
  # split up field data by DataType (LPI vs. COVER) ####
  ObsData_plots <- LpiCov %>%
    dplyr::filter( DataType == "COVER" ) %>%
    dplyr::mutate( plots = round(as.numeric( ResponseVariable )/100, digits =4) ) %>%
    dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
    dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northing, DataType, Plot_Ar, QuadPoly_ID, quad, Shape_Length, Shape_Area, ECOCODE) %>%
    dplyr::distinct() %>% # delete replicate zeros
    dplyr::summarise( plots = mean(plots, na.rm=T) ) %>% # multipe plot sizes for some PlotKeys (ignoring)
    dplyr::inner_join(Covariates_QuadPolyID)
  
  ObsData_pts <- LpiCov %>%
    dplyr::filter(DataType == "LPI") %>%
    dplyr::mutate( Hit = as.numeric(ResponseVariable) ) %>%
    dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
    dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northing, DataType, Plot_Ar, QuadPoly_ID, quad, Shape_Length, Shape_Area, ECOCODE) %>%
    dplyr::distinct() %>% # delete replicate zeros
    dplyr::summarise( Hit = sum(Hit) ) %>%  # multiple polygons for some PlotKeys (ignoring) 
    dplyr::inner_join(Covariates_QuadPolyID)
  
  # PLOTS - dataframe for analysis - one row ####
  PLOTS <- LpiCov %>%
    dplyr::filter( DataType == "COVER" ) %>%
    dplyr::mutate( plots = ifelse(ResponseVariable == 0, NA, round(as.numeric( ResponseVariable )/100, digits =4)) ) %>%
    dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
    dplyr::rename(Prop = plots) %>%
    dplyr::group_by(QuadPoly_ID, Shape_Length, Shape_Area, ECOCODE) %>%
    dplyr::summarise(Total = length(unique(TranKey)),
                     Prop = ifelse(all(is.na(Prop)), 0, mean(Prop, na.rm=T)),
                     Present = ifelse(Prop > 0, 1, 0)) %>%
    dplyr::inner_join(Covariates_QuadPolyID) %>%
    dplyr::distinct()
  
  # PTS - dataframe for analysis  - one row per polygon ####
  PTS <- LpiCov %>%
    dplyr::filter(DataType == "LPI") %>%
    dplyr::mutate( Hit = as.numeric(ResponseVariable) ) %>%
    dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
    group_by(QuadPoly_ID, Shape_Length, Shape_Area, ECOCODE) %>%
    dplyr::summarise(Total = length(unique(PlotKey)), 
                     Hit = sum(Hit),
                     Present = sum(Hit), 
                     Prop = Present/Total) %>%
    dplyr::select(-Present) %>%
    dplyr::mutate(Present = ifelse(Prop > 0, 1, 0)) %>%
    dplyr::inner_join(Covariates_QuadPolyID) %>%
    dplyr::distinct()
  
  return(list(FieldDataPoints_QuadPolyID = FieldDataPoints_QuadPolyID, 
              Covariates_QuadPolyID = Covariates_QuadPolyID, 
              LpiCov = LpiCov,
              ObsData_plots = ObsData_plots,
              ObsData_pts = ObsData_pts,
              PLOTS = PLOTS,
              PTS = PTS))
  
}
