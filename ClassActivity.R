install.packages("terra")
install.packages("tidyterra")
install.packages("FedData")
library(terra)
library(tidyterra)
library(FedData)

nlcd_meve16 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2016,
                       extraction.dir = "Z:\\lmellis\\Data\\")

nlcd_meve16
terra::plot(nlcd_meve16)

cavm <- vect("Z:\\lmellis\\Data\\cp_veg_la_shp\\")

cavm

