veg_jags_data <- function(filepath = "data/FieldData_17April2018.csv",
                          genus = "Populus",
                          species = "angustifolia",
                          subspecies = NA,
                          cont.cov = c("ele", "slp", "casp", "sasp", "twi", "lcv", "sri", "tpi", "minpr", "maxpr", "tapr", "mintp", "maxtp", "aws", "clay", "sand", "silt", "cec", "d2r", "ph", "om", "caco3", "tc", "water", "shadow", "bare", "mgrass", "xgrass", "mshrub", "xshrub", "conifer", "broadleaf", "agriculture", "developed"),
                          cat.cov = c("nass", "sc", "tsf", "ff", "dev")) {
  
  # - the following defines function code - #
  
  require(tidyverse)
  require(stringr)
  
  # Create Modeling Groups ####
  LpiCov <- readr::read_csv(filepath) %>%
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
  
  # Add QuadPolyID ####
  LpiCov <- LpiCov %>%
    dplyr::left_join(FieldDataPoints_QuadPolyID) %>%
    dplyr::filter( !is.na( QuadPoly_ID ) )
  
  cov.pres <- cov.cover <- Covariates_QuadPolyID
  
  # load covariate for models ####
  
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
    cov.pres <-  Covariates_QuadPolyID %>%
      dplyr::select( dplyr::one_of( x.cov )) %>%
      na.omit()
    cov.cover <- Covariates_QuadPolyID %>%
      dplyr::select( dplyr::one_of( x.cov )) %>%
      na.omit()
    
    # remove the polygons with NA covariates from the field data
    LpiCov <- LpiCov %>%
      dplyr::filter( QuadPoly_ID %in% unique(c(cov.pres$QuadPoly_ID,cov.cover$QuadPoly_ID)) )

  # Add PolygonID ####
  LpiCov <- LpiCov %>%
    dplyr::mutate( polygonID = as.numeric(as.factor( QuadPoly_ID )) )

  # split up field data by DataType (LPI vs. COVER) ####
  ObsData_plots <- LpiCov %>%
    dplyr::filter( DataType == "COVER" ) %>%
    dplyr::mutate( plots = round(as.numeric( ResponseVariable )/100, digits =4) ) %>%
    dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
    dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID) %>%
    dplyr::distinct() %>%
    dplyr::summarise( plots = sum(plots) ) # multipe plot sizes for some PlotKeys (ignoring)
  ObsData_pts <- LpiCov %>%
    dplyr::filter(DataType == "LPI") %>%
    dplyr::mutate( Hit = as.numeric(ResponseVariable) ) %>%
  dplyr::select(-Include, -ModelCode, -ModelCode1, -ModelCode2, -ModelCode3, -ResponseVariable) %>%
    dplyr::group_by( PlotKey, TranKey, Source, Smpl_Yr, Easting, Northng, DataType, Plot_Ar, QuadPoly_ID, quad, polygonID) %>%
    dplyr::distinct() %>%
    dplyr::summarise( Hit = sum(Hit) ) # multiple polygons for some PlotKeys (ignoring)

  # arrange covariates by polygonID ####
  cov.pres <- dplyr::left_join(dplyr::distinct(LpiCov[,c("QuadPoly_ID","polygonID")]), cov.pres) %>%
    dplyr::arrange( polygonID ) %>%
    dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates
  cov.cover <- dplyr::left_join(dplyr::distinct(LpiCov[,c("QuadPoly_ID","polygonID")]), cov.cover) %>%
    dplyr::arrange( polygonID ) %>%
    dplyr::select( -QuadPoly_ID, -polygonID ) # remove the polygonID from the covariates  
  
  # split covariates into continuous and categorical ####
  cont.cov.pres <- dplyr::select(cov.pres, dplyr::one_of(cont.cov))
  cat.cov.pres <- dplyr::select(cov.pres, dplyr::one_of(cat.cov))
  cont.cov.cover <- dplyr::select(cov.cover, dplyr::one_of(cont.cov))
  cat.cov.cover <- dplyr::select(cov.cover, dplyr::one_of(cat.cov))
  rm(cov.pres,cov.cover)
  
  # standardize continuous covariates ####
  fnc1 <- function(x){round((x - mean(x))/sd(x),digits=4)}
  cont.cov.pres <-  cont.cov.pres %>%
    dplyr::mutate_all(fnc1) 
  cont.cov.cover <- cont.cov.cover %>%
    dplyr::mutate_all(fnc1)
  
  # renumber the groups for the categorical covariates ####
  fnc1 <- function(x){as.numeric(as.factor(x))}
  cat.cov.pres <-  cat.cov.pres %>%
    dplyr::mutate_all(fnc1) 
  cat.cov.cover <- cat.cov.cover %>%
    dplyr::mutate_all(fnc1)
  
  # dummary array for matrix math
  dummy.cat.pres <- array(1,c(nrow(cat.cov.pres),ncol(cat.cov.pres)))
  dummy.cat.cover <- array(1,c(nrow(cat.cov.cover),ncol(cat.cov.cover)))
  
  # count the number of covariates for the model ####
  n.cont.covariates.pres <- ncol( cont.cov.pres )
  n.cat.covariates.pres <- ncol( cat.cov.pres )
  n.cont.covariates.cover <- ncol( cont.cov.cover )
  n.cat.covariates.cover <- ncol( cat.cov.cover )
  
  # groups for each categorical variable
  if (n.cat.covariates.pres == 0) { 
    cat.covariates.pres.group <- 0 } else {
      cat.covariates.pres.group <- dplyr::summarise_all(cat.cov.pres, funs(max))     
    }
  if (n.cat.covariates.cover == 0) { 
    cat.covariates.cover.group <- 0 } else {
      cat.covariates.cover.group <- dplyr::summarise_all(cat.cov.cover, funs(max))      
    }
  
  # maximum number of groups in the categorical variables
  max.cat.covariates.cover.group <- ifelse(n.cat.covariates.cover == 0, 0, max(unlist(cat.cov.cover)))
  max.cat.covariates.pres.group <- ifelse(n.cat.covariates.pres == 0, 0, max(unlist(cat.cov.pres)))
  
  # Bundle jags data ####
  jags.data <- list(Obs_Plot = ObsData_plots$plots,
                    Obs_Pt = ObsData_pts$Hit,
                    PlotPolyIndex = ObsData_plots$polygonID,
                    PtsPolyIndex = ObsData_pts$polygonID,
                    n.polygon = length(unique(c( ObsData_plots$polygonID, ObsData_pts$polygonID ))),
                    n.plots = nrow( ObsData_plots ),
                    n.pts = nrow( ObsData_pts ),
                    cont.cov.pres = cont.cov.pres,
                    cont.cov.cover = cont.cov.cover,
                    cat.cov.pres = cat.cov.pres,
                    cat.cov.cover = cat.cov.cover,
                    PlotSize = as.numeric( ObsData_plots$Plot_Ar ),
                    n.cont.covariates.pres = n.cont.covariates.pres,
                    n.cont.covariates.cover = n.cont.covariates.cover,
                    n.cat.covariates.pres = n.cat.covariates.pres,
                    n.cat.covariates.cover = n.cat.covariates.cover,
                    cat.covariates.pres.group = cat.covariates.pres.group,
                    cat.covariates.cover.group = cat.covariates.cover.group,
                    dummy.cat.pres = dummy.cat.pres,
                    dummy.cat.cover = dummy.cat.cover,
                    max.cat.covariates.cover.group = max.cat.covariates.cover.group,
                    max.cat.covariates.pres.group = max.cat.covariates.pres.group)
  
  return(list( FieldDataPoints_QuadPolyID = FieldDataPoints_QuadPolyID, 
               Covariates_QuadPolyID = Covariates_QuadPolyID, 
               LpiCov = LpiCov, 
               ObsData_plots = ObsData_plots,
               ObsData_pts = ObsData_pts,
               jags.data = jags.data ))
  
}
