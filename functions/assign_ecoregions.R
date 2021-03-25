assign_ecoregions <- function(DAT=X){
  
  
  #~~~~~~~~~~~~~~~#
  # Field Data ####
  #~~~~~~~~~~~~~~~#
  
  # convert to spatial dataframe
  coords <- as.matrix(DAT[,c("Easting","Northng")]); colnames(coords) <- c("x","y")
  proj <- sp::CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  FieldData <- sp::SpatialPointsDataFrame(data=DAT,coords=coords,proj4string=proj)
  
  
  #~~~~~~~~~~~~~~~#
  # EcoRegions ####
  #~~~~~~~~~~~~~~~#
  
  # Read in eco-regions
  ECO <- rgdal::readOGR(file.path("data","Baileys_ecoregions_sgca"),layer="Baileys_ecoregions_sgca")
  
  # reproject 
  proj <- sp::CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  ECO <- sp::spTransform(ECO,proj)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Assign Field Data to Eco Regions ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  sf.FieldData <- sf::st_as_sf(FieldData)
  sf.ECO <- sf::st_as_sf(ECO)
  X <- sf::st_intersection(sf.FieldData,sf.ECO) %>%
    dplyr::select(-AREA, -PERIMETER, -DOMAIN, -DIVISION, -PROVINCE, -SECTION)
  sf::st_geometry(X) <- NULL
  
  return(X)
  
}