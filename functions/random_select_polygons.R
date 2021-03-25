library(tidyverse)
library(here)

Unit_56_73A <- foreign::read.dbf(here("spatial/output/Unit_56_73A_Merged_select_poly.dbf"))
Unit_43_48 <- foreign::read.dbf(here("spatial/output/Unit_43_48_Merged_select_poly.dbf"))
size <- 100

Unit_56_73A_sample <- sample(Unit_56_73A$QdPl_ID, size, replace=FALSE)
Unit_43_48_sample <- sample(Unit_43_48$QuadPoly_I, size, replace=FALSE)

Unit_56_73A_centroids <- Unit_56_73A[which(Unit_56_73A$QdPl_ID %in% Unit_56_73A_sample),c("QdPl_ID","Xcoord","Ycoord")] %>%
  dplyr::mutate(Number = sample(1:size, size, replace=FALSE)) %>%
  dplyr::rename(QuadPolygonID = QdPl_ID, X = Xcoord, Y = Ycoord) %>%
  dplyr::arrange(Number)
Unit_43_48_centroids <- Unit_43_48[which(Unit_43_48$QuadPoly_I %in% Unit_43_48_sample),c("QuadPoly_I","Xcoord","Ycoord")] %>%
  dplyr::mutate(Number = sample(1:size, size, replace=FALSE)) %>%
  dplyr::rename(QuadPolygonID = QuadPoly_I, X = Xcoord, Y = Ycoord) %>%
  dplyr::arrange(Number)

readr::write_csv(Unit_56_73A_centroids,"Unit_56_73A_centroids.csv")
readr::write_csv(Unit_43_48_centroids,"Unit_43_48_centroids.csv")
