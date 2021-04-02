### Rectify Model Codes with ITIS Taxonomic Serial Number ####
# Last edited by: Robert Ritson, WMI Research Associate, 4/2/2021
# Purpose: this batch of code resolves misspelled and synonymous scientific names with most up-to-date accepted name and a unique taxonomic serial number
#          then matches model codes to the taxon ids and saves as a translation object (for subsequent analysis)
# Note: Infraspecies (Model Code 3) code removes records that are no longer considered supspecies or varieties, but are still accounted for in Model Code 2
#Load Packages
#install.packages("taxize") #Install 'taxize' package, toolbelt of functions for dealing with taxonomy
lapply(c("taxize","here","tidyverse"),require,character.only=T) 

#Load files
load(here(file.path("data","ModelCode_List.RData"))
     
##Step 1: Resolve Scientific Names (account for spelling errors and synonyms)
itis.id<-gnr_datasources()[gnr_datasources()$title == "Integrated Taxonomic Information SystemITIS", "id"] #Get ITIS id for global names resolver

#Model Code 1 (Genera)
mc1<-gnr_resolve(ModelCode1$ModelCode1,data_source_ids = itis.id$id, http = "post", canonical=T, fields = "all") #Genus only
mc1<-mc1[grepl("Plantae",mc1$classification_path),] #select only plant names
mc1<-mc1[!duplicated(mc1$user_supplied_name),] #eliminate duplicate records
mc1$TSN<-ifelse(is.na(mc1$current_taxon_id),mc1$taxon_id,mc1$current_taxon_id) #Select the most up-to-date ID (collapses synonyms)
mc1$Accepted.Name<-NA #for loop to populate updated accepted name
for(i in 1:nrow(mc1)){
  mc1$Accepted.Name[i]<-id2name(mc1$TSN[i],db="itis")[[1]][[2]]
}

#Model Code 2 (Species)
mc2<-gnr_resolve(ModelCode2$ModelCode2,data_source_ids = itis.id$id, http = "post", canonical=T, fields = "all") #Scientific name (Genus species)
mc2<-mc2[grepl("Plantae",mc2$classification_path),] #select only plant names
mc2<-mc2[!duplicated(mc2$user_supplied_name),] #eliminate duplicate records
mc2$TSN<-ifelse(is.na(mc2$current_taxon_id),mc2$taxon_id,mc2$current_taxon_id) #Select the most up-to-date ID (collapses synonyms)
mc2$Accepted.Name<-NA #for loop to populate updated accepted name
for(i in 1:nrow(mc2)){
  mc2$Accepted.Name[i]<-id2name(mc2$TSN[i],db="itis")[[1]][[2]]
}

#Model Code 3 (Infraspecies: ssp. & var.)
mc3<-gnr_resolve(ModelCode3$ModelCode3,data_source_ids = itis.id$id,resolve_once = T, http = "get", canonical=T, fields = "all") #Infraspecies (Genus species ssp/var infraspecific)
mc3<-mc3[grepl("Plantae",mc3$classification_path),] #select only plant names
mc3<-mc3[!duplicated(mc3$user_supplied_name),] #eliminate duplicate records
mc3$TSN<-ifelse(is.na(mc3$current_taxon_id),mc3$taxon_id,mc3$current_taxon_id) #Select the most up-to-date ID (collapses synonyms)
mc3$Accepted.Name<-NA #for loop to populate updated accepted name
for(i in 1:nrow(mc3)){
 mc3$Accepted.Name[i]<-id2name(mc3$TSN[i],db="itis")[[1]][[2]]
}
mc3<-mc3[grep(c("ssp.|var."),mc3$Accepted.Name),] #select only accepted infraspecies (removes varieties and subspecies no longer recognized)

##Step 2: Match TSN ids to Model Codes
matched_ids<- ModelCode1 %>% 
  dplyr::left_join(mc1[,c("user_supplied_name","TSN")],by=c("ModelCode1"="user_supplied_name")) 
ModelCode1$TaxonID<-matched_ids$TSN

matched_ids<- ModelCode2 %>% 
  dplyr::left_join(mc2[,c("user_supplied_name","TSN")],by=c("ModelCode2"="user_supplied_name")) 
ModelCode2$TaxonID<-matched_ids$TSN

matched_ids<- ModelCode3 %>% 
  dplyr::left_join(mc3[,c("user_supplied_name","TSN")],by=c("ModelCode3"="user_supplied_name"))
ModelCode3$TaxonID<-matched_ids$TSN
ModelCode3<-ModelCode3[!is.na(ModelCode3$TaxonID),] #Remove records that are no longer subspecies

#Clean up work space
rm(itis.id,mc1,mc2,mc3,matched_ids,i)

#Step 3: Save Output####
TAXA_translations<-list(ModelCode1,ModelCode2,ModelCode3)
save(TAXA_translations,file=here(file.path("data","TAXA_translations.RData")))
