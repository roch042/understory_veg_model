# load library ####
library(here)
library(tidyverse)


# database connection ####
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  database = "IFWIS_Master",
  uid = "ShinyUserInternal", # "ShinyUserInternal",
  pwd = "hurt seven sat pupil", # "hurt seven sat pupil",
  server = "164.165.105.241",
  port = "1433")


# Connect to Taxa database ####
TAXA_Taxon <- DBI::dbSendQuery(con, paste("SELECT * FROM ","TAXA_Taxon",sep="")) %>%
  DBI::dbFetch()


# Load data on species codes ####
load(here(file.path("data","ModelCode_List.RData")))


# Create TAXA_translations object ####

PLANT <- TAXA_Taxon %>%
  dplyr::filter(Taxa_Kingdom == "Plantae") # could adjust to include fungi

matched_ids <- ModelCode2 %>%
  dplyr::left_join(PLANT[,c("Scientific_Name","TaxonID")], by = c("ModelCode2"="Scientific_Name")) %>%
  filter(!is.na(TaxonID))

# run this code if we have to figure out what is missing
missing_ids <- ModelCode2 %>%
  dplyr::left_join(PLANT[,c("Scientific_Name","TaxonID")], by = c("ModelCode2"="Scientific_Name")) %>%
  filter(is.na(TaxonID))


TAXA_translations <- dplyr::bind_rows(matched_ids,missing_ids)


# Save the TAXA_translations.RData ####
save(TAXA_translations,file=here(file.path("data","TAXA_translations.RData")))























missing_ids$TaxonID[which(missing_ids == "Achnatherum nelsonii")] <- 54866 # 
missing_ids$TaxonID[which(missing_ids == "Achnatherum thurberianum")] <- 47555 #   
missing_ids$TaxonID[which(missing_ids == "Aconogonon phytolaccifolium")] <- 56389 # 
missing_ids$TaxonID[which(missing_ids == "Agropyron trichophorum")] <- 61389 #
missing_ids$TaxonID[which(missing_ids == "Agrostis longiloba")] <- NA # double check code?
missing_ids$TaxonID[which(missing_ids == "Alopecurus magellanicus")] <- 58121 # 
missing_ids$TaxonID[which(missing_ids == "Arctostaphylos acutifolia")] <- 46049 # 
missing_ids$TaxonID[which(missing_ids == "Arenaria kingii")] <- 51462 # 
missing_ids$TaxonID[which(missing_ids == "Arenaria macrophylla")] <- 45603 # 
missing_ids$TaxonID[which(missing_ids == "Aster conspicuus")] <- 48091 # 
missing_ids$TaxonID[which(missing_ids == "Aster foliaceus")] <- 55000 # 
missing_ids$TaxonID[which(missing_ids == "Aster scopulorum")] <- 54173 #
missing_ids$TaxonID[which(missing_ids == "Atriplex heterosperma")] <- 55792 #
missing_ids$TaxonID[which(missing_ids == "Berberis repens")] <- 45301 #
missing_ids$TaxonID[which(missing_ids == "Bistorta viviparum")] <- NA # double check code?
missing_ids$TaxonID[which(missing_ids == "Boechera cobrensis")] <- 39342 #
missing_ids$TaxonID[which(missing_ids == "Celtis douglasii")] <- 90315 #
missing_ids$TaxonID[which(missing_ids == "Celtis reticulata")] <- 90315 #
missing_ids$TaxonID[which(missing_ids == "Centaurea biebersteinii")] <- 55677 #
missing_ids$TaxonID[which(missing_ids == "Centaurea maculosa")] <- 55677 #
missing_ids$TaxonID[which(missing_ids == "Chamerion angustifolium")] <- 51759 #
missing_ids$TaxonID[which(missing_ids == "Cirsium alpina")] <- NA #
missing_ids$TaxonID[which(missing_ids == "Cirsium inamoenum")] <- NA #
missing_ids$TaxonID[which(missing_ids == "Clinopodium douglasii")] <- 51737 #
missing_ids$TaxonID[which(missing_ids == "Cornus stolonifera")] <- 47490 #
missing_ids$TaxonID[which(missing_ids == "Dasiphora floribunda")] <- 90543 #
missing_ids$TaxonID[which(missing_ids == "Delphinium Ã—burkei")] <- 39454 #
missing_ids$TaxonID[which(missing_ids == "Delphinium Ã—occidentale")] <- 78690 #
missing_ids$TaxonID[which(missing_ids == "Dipsacus sylvestris")] <- 60720 #
missing_ids$TaxonID[which(missing_ids == "Disporum trachycarpum")] <- 54799 #
missing_ids$TaxonID[which(missing_ids == "Elymus cinereus")] <- 54702 #
missing_ids$TaxonID[which(missing_ids == "Epilobium densiflorum")] <- 56497 #
missing_ids$TaxonID[which(missing_ids == "Epilobium pygmaeum")] <- 44436 #
missing_ids$TaxonID[which(missing_ids == "Epilobium torreyi")] <- 45913 #
missing_ids$TaxonID[which(missing_ids == "Eremogone congesta")] <- 44401 #
missing_ids$TaxonID[which(missing_ids == "Euphorbia serpyllifolia")] <- 55673 #
missing_ids$TaxonID[which(missing_ids == "Glossopetalon nevadense")] <- 54725 #
missing_ids$TaxonID[which(missing_ids == "Heleochloa alopecuroides")] <- 41812 #
missing_ids$TaxonID[which(missing_ids == "Helianthella nuttallii")] <- NA #
missing_ids$TaxonID[which(missing_ids == "Hieracium albertinum")] <- 78141 #
missing_ids$TaxonID[which(missing_ids == "Hordeum leporinum")] <- 91436 #
missing_ids$TaxonID[which(missing_ids == "Hymenoxys hoopesii")] <- 45908 #
missing_ids$TaxonID[which(missing_ids == "Kochia scoparia")] <- 52015 #
missing_ids$TaxonID[which(missing_ids == "Koeleria cristata")] <- 49042 #
missing_ids$TaxonID[which(missing_ids == "Leptosiphon septentrionalis")] <- 50946 #
missing_ids$TaxonID[which(missing_ids == "Linanthus glabrum")] <- 44300 #
missing_ids$TaxonID[which(missing_ids == "Lithospermum arvense")] <- 59729 #
missing_ids$TaxonID[which(missing_ids == "Lupinus all")] <- NA #
missing_ids$TaxonID[which(missing_ids == "Luzula hitchcockii")] <- 85415 #
missing_ids$TaxonID[which(missing_ids == "Micranthes oregana")] <- 49731 #
missing_ids$TaxonID[which(missing_ids == "Microseris troximoides")] <- 44091 #
missing_ids$TaxonID[which(missing_ids == "Microsteris gracilis")] <- 54815 #
missing_ids$TaxonID[which(missing_ids == "Montia cordifolia")] <- 56334 #
missing_ids$TaxonID[which(missing_ids == "Nassella viridula")] <- 56639 #
missing_ids$TaxonID[which(missing_ids == "Nasturtium officinale")] <- 56700 #
missing_ids$TaxonID[which(missing_ids == "Nuphar polysepala")] <- 53605 #
missing_ids$TaxonID[which(missing_ids == "Oryzopsis hymenoides")] <- 52179 #
missing_ids$TaxonID[which(missing_ids == "Packera streptanthifolia")] <- 56337 #
missing_ids$TaxonID[which(missing_ids == "Panicum scribnerianum")] <- 91422 #
missing_ids$TaxonID[which(missing_ids == "Persicaria lapathifolia")] <- 58607 #
missing_ids$TaxonID[which(missing_ids == "Populus trichocarpa")] <- 89872 #
missing_ids$TaxonID[which(missing_ids == "Potentilla fruticosa")] <- 67953 #
missing_ids$TaxonID[which(missing_ids == "Pseudognaphalium macounii")] <- NA #
missing_ids$TaxonID[which(missing_ids == "Pseudognaphalium stramineum")] <- 52760 #
missing_ids$TaxonID[which(missing_ids == "Pyrola uniflora")] <- 49505 #
missing_ids$TaxonID[which(missing_ids == "Ranunculus testiculatus")] <- 54178 #
missing_ids$TaxonID[which(missing_ids == "Rhamnus purshiana")] <- 59217 #
missing_ids$TaxonID[which(missing_ids == "Rhododendron neoglandulosum")] <- 45314 #
missing_ids$TaxonID[which(missing_ids == "Rubacer parviflorus")] <- 86119 #
missing_ids$TaxonID[which(missing_ids == "Sambucus caerulea")] <- 84407 #
missing_ids$TaxonID[which(missing_ids == "Sambucus cerulea")] <- 84407 #
missing_ids$TaxonID[which(missing_ids == "Sanguisorba stipulata")] <- 58523 #
missing_ids$TaxonID[which(missing_ids == "Schedonorus phoenix")] <- 54849 #
missing_ids$TaxonID[which(missing_ids == "Schedonorus pratensis")] <- 60937 #
missing_ids$TaxonID[which(missing_ids == "Schoenoplectus fluviatilis")] <- 61262 #
missing_ids$TaxonID[which(missing_ids == "Scirpus acutus")] <- 92983 #
missing_ids$TaxonID[which(missing_ids == "Scirpus americanus")] <- 60802 #
missing_ids$TaxonID[which(missing_ids == "Scirpus pungens")] <- 56807 #
missing_ids$TaxonID[which(missing_ids == "Senecio canus")] <- 56336 #
missing_ids$TaxonID[which(missing_ids == "Sitanion hystrix")] <- 52792 #
missing_ids$TaxonID[which(missing_ids == "Spiraea Ã—pyramidata")] <- 79312 #
missing_ids$TaxonID[which(missing_ids == "Spiraea densiflora")] <- 47737 #
missing_ids$TaxonID[which(missing_ids == "Stuckenia pectinata")] <- 42137 #
missing_ids$TaxonID[which(missing_ids == "Stuckenia vaginata")] <- 48529 #
missing_ids$TaxonID[which(missing_ids == "Tofieldia glutinosa")] <- 45207 #
missing_ids$TaxonID[which(missing_ids == "Townsendia florifer")] <- 43518 #
missing_ids$TaxonID[which(missing_ids == "Trichophorum cespitosum")] <- 79566 #
missing_ids$TaxonID[which(missing_ids == "Triglochin maritimum")] <- 44638 #
missing_ids$TaxonID[which(missing_ids == "UNKNOWN VEG")] <- NA #
missing_ids$TaxonID[which(missing_ids == "Vaccinium cespitosum")] <- 58682 #
missing_ids$TaxonID[which(missing_ids == "Ventenata koeler")] <- NA # this is actually a genus
missing_ids$TaxonID[which(missing_ids == "Veronica anagallis")] <- 44354 #