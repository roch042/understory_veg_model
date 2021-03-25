require(tidyverse)

LPI_IDFG <- readr::read_csv(file.path("data","FieldData","DataFormatting","formatted_data","LPI_IDFG","LPI_IDFG_FINAL_26Oct2020.csv"))
ORIG <- readr::read_csv(file.path("data","FieldData","DataFormatting","formatted_data","ORIGINAL_FORMATTED_DATA","FieldData_19Nov2019.csv")) %>%
  dplyr::rename(Northing = Northng)

FINAL <- dplyr::bind_rows(LPI_IDFG, ORIG)

readr::write_csv(FINAL,file.path("data","FieldData","FieldData_26Oct2020.csv"))
