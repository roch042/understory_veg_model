make_centroid_map <- function(unit = unit,
                              folder = "species",
                              type = "presence",
                              file_name = "56_73A_Pseudoroegneria_spicata_NA.csv")
{
  
  if(type == "presence"){
    
    presence <- readr::read_csv(here(file.path("results",folder,type,"text",file_name)))
    title <- gsub(".csv","",file_name)
    
    # Join the probabilities/presence to the centroids
    unit <- unit %>%
      dplyr::select(QuadPoly_ID, Centroid_X, Centroid_Y) %>%
      dplyr::left_join(presence) %>%
      dplyr::mutate(Present = dplyr::recode(Present, "1"="Yes", "0"="No"))
    
    # by probability
    p1 <- ggplot(unit, aes(x=Centroid_X,y=Centroid_Y,color=Prob)) +
      geom_point(size=2, alpha=0.8) +
      # scale_color_gradient(low = "blue", high = "red") 
      # scale_color_gradientn(colours = terrain.colors(7))
      scale_color_viridis(limits = c(0,1), discrete = FALSE, option = "D")
    
    # by presence
    p2 <- ggplot(unit, aes(x=Centroid_X,y=Centroid_Y,color=Present)) +
      geom_point(size=2, alpha=0.8) +
      scale_color_viridis(discrete = TRUE, option = "D")
    
    pfinal <- grid.arrange(p1, p2, ncol=2, top = textGrob(title,gp=gpar(fontsize=20,font=3)))
    
  } else {
    if(type == "percent_cover"){
      
      percent <- readr::read_csv(here(file.path("results",folder,type,"text",file_name)))
      title <- gsub(".csv","",file_name)
      
      # Join the probabilities/presence to the centroids
      unit <- unit %>%
        dplyr::select(QuadPoly_ID, Centroid_X, Centroid_Y) %>%
        dplyr::left_join(percent)
      
      # by probability
      p1 <- ggplot(unit, aes(x=Centroid_X,y=Centroid_Y,color=Prob)) +
        geom_point(size=2, alpha=0.8) +
        # scale_color_gradient(low = "blue", high = "red") 
        # scale_color_gradientn(colours = terrain.colors(7))
        scale_color_viridis(limits = c(0,1), discrete = FALSE, option = "D")
      
      # by probability - no fixed limits
      p2 <- ggplot(unit, aes(x=Centroid_X,y=Centroid_Y,color=Prob)) +
        geom_point(size=2, alpha=0.8) +
        # scale_color_gradient(low = "blue", high = "red") 
        # scale_color_gradientn(colours = terrain.colors(7))
        scale_color_viridis(discrete = FALSE, option = "D")
      
      pfinal <- grid.arrange(p1, p2, ncol=2, top = textGrob(title,gp=gpar(fontsize=20,font=3)))
      
    }
    
  }
  
  return(pfinal)
  
}
