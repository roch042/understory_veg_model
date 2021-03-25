library(sf)
library(here)
library(tidyverse)

shp_location <- "//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output" 
pred_location <- "//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/results/species/presence/text/"
pred_file <- "56_73A_Pseudoroegneria_spicata_NA.csv"
pred <- readr::read_csv(paste(pred_location,pred_file,sep=""))
model_name <- gsub(".csv|56_73A_","",pred_file)

# text files available
txt_list <- list.files(pred_location)
X <- list()
# for (i in 1:length(txt_list)){
for (i in 1:10){
  pred_file <- txt_list[i]
  model_name <- gsub(".csv|56_73A_","",pred_file)
  X[[i]] <- readr::read_csv(paste(pred_location,pred_file,sep="")) %>%
    dplyr::mutate(ModelName = model_name) %>%
    dplyr::group_by(QuadPoly_ID, ModelName) %>% # remove these parts when fix duplicate problem with predictions
    dplyr::summarise(Present = mean(Present, na.rm=T),
                     Prob = mean(Prob, na.rm=T))
}
X1 <- dplyr::bind_rows(X) %>%
  dplyr::distinct()
X2 <- X1 %>%
  dplyr::select(-Present) %>%
  tidyr::spread(key=ModelName,value=Prob)

# X2 <- purrr::map(as.list(txt_list),function(x){
#   readr::read_csv(paste(pred_location,x,sep="")) %>%
#     dplyr::mutate(ModelName = gsub(".csv|56_73A_","",x))
# })

# single text file
pred_format <- readr::read_csv(paste(pred_location,pred_file,sep="")) %>%
  dplyr::mutate(ModelName = model_name)
X1 <- dplyr::bind_rows(pred_format) %>%
  dplyr::distinct()
X2 <- X1 %>%
  dplyr::select(-Present) %>%
  tidyr::spread(key=ModelName,value=Prob)

# lists shapefiles available
shp_list <- unique(gsub(".dbf|.prj|.shp|.shx","",list.files(shp_location)))

# for( i in 1:length(shp_list)){
i <- 8
# reads in the feature layer you want to filter to
shp <- sf::st_read(dsn = paste(shp_location,"/Result.gdb",sep=""), layer = "Unit_56_73A_Merged")
shp_join <- dplyr::inner_join(shp,pred,by=c("QdPl_ID"="QuadPoly_ID")) #%>%
  # dplyr::rename(!!quo_name(paste("ProbPresent_",model_name,sep="")) := Prob)

sf::st_write(shp_join, paste(shp_location,"/",model_name,"_ProbPres.shp",sep=""))
# }

shp_crop <- sf::st_crop(shp_join, c(xmin=2608862, xmax=2609000, ymin=1214645, ymax=1214700))
plot(shp_crop["Prob"])
plot(shp_join["Prob"])

