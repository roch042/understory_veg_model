#~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in Forage Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~#

filepath <- here(file.path("data","ForageSpecies.csv"))

X <- readr::read_csv(filepath) %>%
  dplyr::rename(Genus = `Genus-corrected`,
                Species = `species-corrected`,
                SubSpecies = `subspecies/variant-corrected`) %>%
  dplyr::mutate(
    ModelCode1 = Genus,
    ModelCode2 = stringr::str_c( Genus, Species, sep = " " ),
    ModelCode3 = stringr::str_c( Genus, Species, SubSpecies, sep = " " )) %>%
  dplyr::select(-SpName) %>%
  dplyr::distinct() %>%
  dplyr::filter(MuleDeer == "Y" | SageGrouse == "Y" | Elk == "Y" | Moose == "Y")

# Create FORAGE list
FORAGE <- list("ModelCode1"=NULL,"ModelCode2"=NULL,"ModelCode3"=NULL)

FORAGE[["ModelCode1"]] <- X %>%
  dplyr::select(ModelCode1, Genus, MuleDeer, SageGrouse, Elk, Moose) %>%
  dplyr::group_by(ModelCode1, Genus) %>%
  dplyr::summarize(MuleDeer_c = sum(!is.na(MuleDeer)),
                   SageGrouse_c = sum(!is.na(SageGrouse)),
                   Elk_c = sum(!is.na(Elk)),
                   Moose_c = sum(!is.na(Moose))) %>%
  dplyr::mutate( MuleDeer = ifelse( MuleDeer_c > 0, "Y", "N"),
                 SageGrouse = ifelse( SageGrouse_c > 0, "Y", "N"),
                 Elk = ifelse( Elk_c > 0, "Y", "N"),
                 Moose = ifelse( Moose_c > 0, "Y", "N")) %>%
  dplyr::select( -MuleDeer_c, -SageGrouse_c, -Moose_c, -Elk_c) %>%
  dplyr::filter(!is.na(ModelCode1))

FORAGE[["ModelCode2"]] <- X %>%
  dplyr::select(ModelCode2, Genus, Species, MuleDeer, SageGrouse, Elk, Moose) %>%
  dplyr::group_by(ModelCode2, Genus, Species) %>%
  dplyr::summarize(MuleDeer_c = sum(!is.na(MuleDeer)),
                   SageGrouse_c = sum(!is.na(SageGrouse)),
                   Elk_c = sum(!is.na(Elk)),
                   Moose_c = sum(!is.na(Moose))) %>%
  dplyr::mutate( MuleDeer = ifelse( MuleDeer_c > 0, "Y", "N"),
                 SageGrouse = ifelse( SageGrouse_c > 0, "Y", "N"),
                 Elk = ifelse( Elk_c > 0, "Y", "N"),
                 Moose = ifelse( Moose_c > 0, "Y", "N")) %>%
  dplyr::select( -MuleDeer_c, -SageGrouse_c, -Moose_c, -Elk_c) %>%
  dplyr::filter(!is.na(ModelCode2))

FORAGE[["ModelCode3"]] <- X %>%
  dplyr::select(ModelCode3, Genus, Species, SubSpecies, MuleDeer, SageGrouse, Elk, Moose) %>%
  dplyr::group_by(ModelCode3, Genus, Species, SubSpecies) %>%
  dplyr::summarize(MuleDeer_c = sum(!is.na(MuleDeer)),
                   SageGrouse_c = sum(!is.na(SageGrouse)),
                   Elk_c = sum(!is.na(Elk)),
                   Moose_c = sum(!is.na(Moose))) %>%
  dplyr::mutate( MuleDeer = ifelse( MuleDeer_c > 0, "Y", "N"),
                 SageGrouse = ifelse( SageGrouse_c > 0, "Y", "N"),
                 Elk = ifelse( Elk_c > 0, "Y", "N"),
                 Moose = ifelse( Moose_c > 0, "Y", "N")) %>%
  dplyr::select( -MuleDeer_c, -SageGrouse_c, -Moose_c, -Elk_c) %>%
  dplyr::filter(!is.na(ModelCode3))

# create categories
Models <- dplyr::bind_rows(FORAGE[[1]],FORAGE[[2]],FORAGE[[3]]) %>%
  dplyr::distinct() %>%
  tidyr::gather(ModelCode,ModelCodeTitle,ModelCode1,ModelCode2,ModelCode3) %>%
  dplyr::select(ModelCode,ModelCodeTitle,Genus,Species,SubSpecies) %>%
  dplyr::filter(!is.na(ModelCodeTitle))

Forage_Models_Genus <- dplyr::filter(Models, ModelCode == "ModelCode1")
Forage_Models_Species <- dplyr::filter(Models, ModelCode == "ModelCode2")
Forage_Models_SubSpecies <- dplyr::filter(Models, ModelCode == "ModelCode3")

save(list=c("FORAGE","Models","Forage_Models_Genus","Forage_Models_Species","Forage_Models_SubSpecies"), file = here(file.path("data","FORAGE.RData")) )
