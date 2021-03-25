veg_jags_data <- function(filepath = "data/FieldData.RData",
                          genus = "Populus",
                          species = "angustifolia",
                          subspecies = NA,
                          train.prop = 0.75) {
  # train.prop = the proportion of polygons to treat as the training data
  
  # - the following defines function code - #
  
  require(tidyverse)
  require(stringr)
  
  # load function
  # source(file.path("functions","assign_ecoregions.R"))
  
  # load workspaces ####
  load(file.path("data","FieldDataPoints_QuadPolyID.RData")) # index of PlotKeys and QuadPolyIDs/quads
  load(file.path("data","Covariates_QuadPolyID.RData")) # covariate values by QuadPolyID
  load(file.path("data","FORAGE.RData"))
  load(file.path("data","ModelCode_List.RData"))
  load(file.path("data","EcoCodes.RData"))
  
  # load covariates to include in models ####
  MC3 <- readr::read_csv(file.path("data","ModelCode3_Covariates.csv")) # genus species subspecies models
  MC2 <- readr::read_csv(file.path("data","ModelCode2_Covariates.csv")) # genus species models
  MC1 <- readr::read_csv(file.path("data","ModelCode1_Covariates.csv")) # genus only models
  
  # Load Data ####
  load(filepath)
  
  # Add Eco Regions and QuadPolyID ####
  # X <- assign_ecoregions(X)
  X <- X %>%
    dplyr::left_join(EcoCodes[,c("QuadPoly_ID","quad","PlotKey","TranKey","ECOCODE")])
  
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
        return(ifelse( ModelCode1 == genus,
                       ifelse(DataType == "COVER", Prcnt_C, 1 ), 0 ))
      } else {
        if( is.na(subspecies) & !is.na(species) & !is.na(genus)){
          return(ifelse( !is.na(ModelCode2) & ModelCode2 == stringr::str_c( genus, species, sep = " "), 
                         ifelse(DataType == "COVER", Prcnt_C, 1 ), 0 )) 
        } else {
          if( !is.na(subspecies) & !is.na(species) & !is.na(genus)){
            return(ifelse( !is.na(ModelCode3) & ModelCode3 == stringr::str_c( genus, species, subspecies, sep = " "), 
                           ifelse(DataType == "COVER", Prcnt_C, 1 ), 0 )) 
          } else {
          return(NA)
          }
        }
      }
    )
  
  # # Add QuadPolyID ####
  # LpiCov <- LpiCov %>%
  #   dplyr::left_join(FieldDataPoints_QuadPolyID) %>%
  #   dplyr::filter( !is.na( QuadPoly_ID ) )
  
  if( !all(LpiCov$ResponseVariable==0) ) {
  # Divide into Training and Testing dataset (repeat until at least one occurrence in training data set)
  repeat{
    train.prop <- train.prop
    QuadPoly_IDs <- unique(FieldDataPoints_QuadPolyID$QuadPoly_ID)
    train.n <- round(train.prop*length(QuadPoly_IDs),digits=0)
    train.QuadPoly_ID <- sample(QuadPoly_IDs,train.n,replace=FALSE)
    test.QuadPoly_ID <- QuadPoly_IDs[which(!QuadPoly_IDs %in% train.QuadPoly_ID)]
    
    LpiCov_Test <- LpiCov %>%
      dplyr::filter(QuadPoly_ID %in% test.QuadPoly_ID)
    
    LpiCov_Train <- LpiCov %>%
      dplyr::filter(QuadPoly_ID %in% train.QuadPoly_ID)
    if( !all(LpiCov_Train$ResponseVariable==0) ){
      break
    }
  }

  # load covariate for models ####
  
  # brief editing of covariates
  Covariates_QuadPolyID <- Covariates_QuadPolyID %>%
    dplyr::select( -lcv, -tpi, -nass, -dev, -mintp, -maxpr, -sasp, -tapr) %>%
    dplyr::mutate( tsf = as.integer(format(Sys.Date(), "%Y")) - tsf, # years since most recent fire
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
                   tsf2 = tsf^2,
                   ff2 = ff^2,
                   tc2 = tc^2,
                   sc2 = sc^2 ) 
  
  cov.pres <- cov.cover <- Covariates_QuadPolyID
  
    if( !is.na(genus) & is.na(species) & is.na(subspecies) ){
      x.cov <- MC1 %>%
        dplyr::filter( ModelCode1 == genus )
      x.cov <-  c(names( x.cov )[which( x.cov == 1 )], "QuadPoly_ID" )
    } else {
      if( !is.na(genus) & !is.na(species) & is.na(subspecies) ){
        x.cov <- MC2 %>%
          dplyr::filter( ModelCode2 == stringr::str_c( genus, species, sep = " " ))
        x.cov <-  c(names( x.cov )[which( x.cov == 1 )], "QuadPoly_ID" )
      } else {
        if( !is.na(subspecies) & !is.na(species) & !is.na(genus) ){
          x.cov <- MC3 %>%
            dplyr::filter( ModelCode3 == stringr::str_c( genus, species, subspecies, sep = " " ))
          x.cov <-  c(names( x.cov )[which( x.cov == 1 )], "QuadPoly_ID" )
        }
      }
    }
    
    
    # select covariates and remove NAs ####
    cov.pres.orig <-  Covariates_QuadPolyID %>%
      dplyr::select( dplyr::one_of( x.cov )) %>%
      na.omit()
    cov.cover.orig <- Covariates_QuadPolyID %>%
      dplyr::select( dplyr::one_of( x.cov )) %>%
      na.omit()
    
    # remove the polygons with NA covariates from the field data
    LpiCov <- LpiCov %>%
      dplyr::filter( QuadPoly_ID %in% unique(c(cov.pres.orig$QuadPoly_ID,cov.cover.orig$QuadPoly_ID)) )
    LpiCov_Train <- LpiCov_Train %>%
      dplyr::filter( QuadPoly_ID %in% unique(c(cov.pres.orig$QuadPoly_ID,cov.cover.orig$QuadPoly_ID)) )
    LpiCov_Test <- LpiCov_Test %>%
      dplyr::filter( QuadPoly_ID %in% unique(c(cov.pres.orig$QuadPoly_ID,cov.cover.orig$QuadPoly_ID)) )
    
    # Add PolygonID ####
    LpiCov <- LpiCov %>%
      dplyr::mutate( polygonID = as.numeric(as.factor( QuadPoly_ID )) )
    LpiCov_Train <- LpiCov_Train %>%
      dplyr::mutate( polygonID = as.numeric(as.factor( QuadPoly_ID )) )
    LpiCov_Test <- LpiCov_Test %>%
      dplyr::mutate( polygonID = as.numeric(as.factor( QuadPoly_ID )) )
    
    # split up field data by DataType (LPI vs. COVER) ####
    ObsData_plots <- LpiCov %>%
      dplyr::filter( DataType == "COVER" ) %>%
      dplyr::mutate( plots = round(as.numeric( ResponseVariable )/100, digits =4) ) %>%
      dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
      dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID,ECOCODE) %>%
      dplyr::distinct() %>%
      dplyr::summarise( plots = sum(plots) ) # multipe plot sizes for some PlotKeys (ignoring)
    ObsData_pts <- LpiCov %>%
      dplyr::filter(DataType == "LPI") %>%
      dplyr::mutate( Hit = as.numeric(ResponseVariable) ) %>%
      dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
      dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID,ECOCODE) %>%
      dplyr::distinct() %>%
      dplyr::summarise( Hit = sum(Hit) ) # multiple polygons for some PlotKeys (ignoring)
    ObsData_plots_train <- LpiCov_Train %>%
      dplyr::filter( DataType == "COVER" ) %>%
      dplyr::mutate( plots = round(as.numeric( ResponseVariable )/100, digits =4) ) %>%
      dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
      dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID,ECOCODE) %>%
      dplyr::distinct() %>%
      dplyr::summarise( plots = sum(plots) ) # multipe plot sizes for some PlotKeys (ignoring)
    ObsData_pts_train <- LpiCov_Train %>%
      dplyr::filter(DataType == "LPI") %>%
      dplyr::mutate( Hit = as.numeric(ResponseVariable) ) %>%
      dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
      dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID,ECOCODE) %>%
      dplyr::distinct() %>%
      dplyr::summarise( Hit = sum(Hit) ) # multiple polygons for some PlotKeys (ignoring)
    ObsData_plots_test <- LpiCov_Test %>%
      dplyr::filter( DataType == "COVER" ) %>%
      dplyr::mutate( plots = round(as.numeric( ResponseVariable )/100, digits =4) ) %>%
      dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
      dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID,ECOCODE) %>%
      dplyr::distinct() %>%
      dplyr::summarise( plots = sum(plots) ) # multipe plot sizes for some PlotKeys (ignoring)
    ObsData_pts_test <- LpiCov_Test %>%
      dplyr::filter(DataType == "LPI") %>%
      dplyr::mutate( Hit = as.numeric(ResponseVariable) ) %>%
      dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
      dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID,ECOCODE) %>%
      dplyr::distinct() %>%
      dplyr::summarise( Hit = sum(Hit) ) # multiple polygons for some PlotKeys (ignoring)
  
  # arrange covariates by polygonID ####
  cov.pres <- dplyr::left_join(dplyr::distinct(LpiCov[,c("QuadPoly_ID","polygonID")]), cov.pres.orig) %>%
      dplyr::arrange( polygonID ) %>%
      dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates
  cov.cover <- dplyr::left_join(dplyr::distinct(LpiCov[,c("QuadPoly_ID","polygonID")]), cov.cover.orig) %>%
      dplyr::arrange( polygonID ) %>%
      dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates  
  cov.pres_train <- dplyr::left_join(dplyr::distinct(LpiCov_Train[,c("QuadPoly_ID","polygonID")]), cov.pres.orig) %>%
    dplyr::arrange( polygonID ) %>%
    dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates
  cov.cover_train <- dplyr::left_join(dplyr::distinct(LpiCov_Train[,c("QuadPoly_ID","polygonID")]), cov.cover.orig) %>%
    dplyr::arrange( polygonID ) %>%
    dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates  
  cov.pres_test <- dplyr::left_join(dplyr::distinct(LpiCov_Test[,c("QuadPoly_ID","polygonID")]), cov.pres.orig) %>%
    dplyr::arrange( polygonID ) %>%
    dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates
  cov.cover_test <- dplyr::left_join(dplyr::distinct(LpiCov_Test[,c("QuadPoly_ID","polygonID")]), cov.cover.orig) %>%
    dplyr::arrange( polygonID ) %>%
    dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates  
  
  # covariate summary ####
  df.cov.pres <- cov.pres %>%
    tidyr::gather(covariate,value) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T))
  df.cov.cover <- cov.cover %>%
    tidyr::gather(covariate,value) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T))
  df.cov.pres_train <- cov.pres_train %>%
    tidyr::gather(covariate,value) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T))
  df.cov.cover_train <- cov.cover_train %>%
    tidyr::gather(covariate,value) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T))
  df.cov.pres_test <- cov.pres_test %>%
    tidyr::gather(covariate,value) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T))
  df.cov.cover_test <- cov.cover_test %>%
    tidyr::gather(covariate,value) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarise(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T))
  
  # standardize covariates ####
  fnc1 <- function(x){round((x - mean(x))/sd(x),digits=4)}
  cov.pres <-  cov.pres %>%
    dplyr::mutate_all(fnc1) 
  cov.cover <- cov.cover %>%
    dplyr::mutate_all(fnc1)
  cov.pres_train <-  cov.pres_train %>%
    dplyr::mutate_all(fnc1) 
  cov.cover_train <- cov.cover_train %>%
    dplyr::mutate_all(fnc1)
  cov.pres_test <-  cov.pres_test %>%
    dplyr::mutate_all(fnc1) 
  cov.cover_test <- cov.cover_test %>%
    dplyr::mutate_all(fnc1)
  
  # count the number of covariates for the model ####
  n.covariates.pres <- ncol( cov.pres )
  n.covariates.cover <- ncol( cov.cover )
  n.covariates.pres_train <- ncol( cov.pres_train )
  n.covariates.cover_train <- ncol( cov.cover_train )
  n.covariates.pres_test <- ncol( cov.pres_test )
  n.covariates.cover_test <- ncol( cov.cover_test )
  
  # Bundle jags data ####
  jags.data <- list(Obs_Plot = ObsData_plots$plots,
                    Obs_Pt = ObsData_pts$Hit,
                    PlotPolyIndex = ObsData_plots$polygonID,
                    PtsPolyIndex = ObsData_pts$polygonID,
                    PlotEcoCode = ObsData_plots$ECOCODE,
                    PtsEcoCode = ObsData_pts$ECOCODE,
                    n.polygon = length(unique(c( ObsData_plots$polygonID, ObsData_pts$polygonID ))),
                    n.plots = nrow( ObsData_plots ),
                    n.pts = nrow( ObsData_pts ),
                    cov.pres = cov.pres,
                    cov.cover = cov.cover,
                    df.cov.pres = df.cov.pres,
                    df.cov.cover = df.cov.cover,
                    PlotSize = as.numeric( ObsData_plots$Plot_Ar ),
                    n.covariates.pres = n.covariates.pres,
                    n.covariates.cover = n.covariates.cover)
  jags.data_train <- list(Obs_Plot = ObsData_plots_train$plots,
                    Obs_Pt = ObsData_pts_train$Hit,
                    PlotPolyIndex = ObsData_plots_train$polygonID,
                    PtsPolyIndex = ObsData_pts_train$polygonID,
                    PlotEcoCode = ObsData_plots_train$ECOCODE,
                    PtsEcoCode = ObsData_pts_train$ECOCODE,
                    n.polygon = length(unique(c( ObsData_plots_train$polygonID, ObsData_pts_train$polygonID ))),
                    n.plots = nrow( ObsData_plots_train ),
                    n.pts = nrow( ObsData_pts_train ),
                    cov.pres = cov.pres_train,
                    cov.cover = cov.cover_train,
                    df.cov.pres = cov.pres_train,
                    df.cov.cover = cov.cover_train,
                    PlotSize = as.numeric( ObsData_plots_train$Plot_Ar ),
                    n.covariates.pres = n.covariates.pres_train,
                    n.covariates.cover = n.covariates.cover_train)
  jags.data_test <- list(Obs_Plot = ObsData_plots_test$plots,
                    Obs_Pt = ObsData_pts_test$Hit,
                    PlotPolyIndex = ObsData_plots_test$polygonID,
                    PtsPolyIndex = ObsData_pts_test$polygonID,
                    PlotEcoCode = ObsData_plots_test$ECOCODE,
                    PtsEcoCode = ObsData_pts_test$ECOCODE,
                    n.polygon = length(unique(c( ObsData_plots_test$polygonID, ObsData_pts_test$polygonID ))),
                    n.plots = nrow( ObsData_plots_test ),
                    n.pts = nrow( ObsData_pts_test ),
                    cov.pres = cov.pres_test,
                    cov.cover = cov.cover_test,
                    df.cov.pres = cov.pres_test,
                    df.cov.cover = cov.cover_test,
                    PlotSize = as.numeric( ObsData_plots_test$Plot_Ar ),
                    n.covariates.pres = n.covariates.pres_test,
                    n.covariates.cover = n.covariates.cover_test)
  
  save(list=c("FieldDataPoints_QuadPolyID","Covariates_QuadPolyID","LpiCov_Train","LpiCov_Test","test.QuadPoly_ID","train.QuadPoly_ID","ObsData_plots","ObsData_pts","jags.data","jags.data_test","jags.data_train"),
       file=file.path("results",paste(genus,species,subspecies,"Testing_Data",stringr::str_c(Sys.Date(),".RData",sep=""),sep="_")))
  
  return(list( FieldDataPoints_QuadPolyID = FieldDataPoints_QuadPolyID, 
               Covariates_QuadPolyID = Covariates_QuadPolyID, 
               LpiCov = LpiCov,
               ObsData_plots = ObsData_plots,
               ObsData_pts = ObsData_pts,
               jags.data = jags.data,
               jags.data_test = jags.data_test,
               jags.data_train = jags.data_train))
  
  } else {
    error <- list(error="ModelCode not recorded in FieldData")
    return(error)
  }
  
}
