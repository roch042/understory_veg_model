require(rgdal)
require(sp)
require(raster)
require(here)
require(tidyverse)
require(gridExtra)
require(grid)
require(viridis)

# Spatial Points 
unit <- readr::read_csv("C:\\Users\\eroche\\Documents\\Fine scale vegetation analysis\\idahoveg_database\\GMU_output_tables\\Units_56_73A.txt")

make_centroid_map <- function(unit = unit,
                              presence_file = "56_73A_Pseudoroegneria_spicata_NA.csv")
{
  
  presence <- readr::read_csv(here(file.path("results","species","text",presence_file)))
  title <- gsub(".csv","",presence_file)
  
  # Join the probabilities/presence to the centroids
  unit <- unit %>%
    dplyr::select(QuadPoly_ID, Centroid_X, Centroid_Y) %>%
    dplyr::left_join(presence) %>%
    dplyr::mutate(Present = dplyr::recode(Present, "1"="Yes", "0"="No"))
  
  # by probability
  p1 <- ggplot(unit, aes(x=Centroid_X,y=Centroid_Y,color=Prob)) +
    geom_point(size=1) +
    # scale_color_gradient(low = "blue", high = "red") 
    # scale_color_gradientn(colours = terrain.colors(7))
    scale_color_viridis(discrete = FALSE, option = "D")
  
  # by presence
  p2 <- ggplot(unit, aes(x=Centroid_X,y=Centroid_Y,color=Present)) +
    geom_point(size=1) +
    scale_color_viridis(discrete = TRUE, option = "D")
  
  pfinal <- grid.arrange(p1, p2, ncol=2, top = textGrob(title,gp=gpar(fontsize=20,font=3)))
  
  return(pfinal)
  
}

make_centroid_map()