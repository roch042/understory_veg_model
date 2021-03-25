file.folder <- file.path("~","Fine scale vegetation analysis","veg_jags_model","data","FieldData")

data.files <- list.files(file.folder)

X <- list()
for(i in 1:length(data.files)){
X[[i]] <- readr::read_csv(file.path(file.folder,data.files[i]))
}
X <- dplyr::bind_rows(X)

save(X,file=file.path("~","Fine scale vegetation analysis","veg_jags_model","data","FieldData.RData"))
