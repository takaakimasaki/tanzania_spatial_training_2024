# install packages --------------------------------------------------------
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp")
loss_year_00N_030E <- terra::rast("data-raw/deforestation/Hansen_GFC-2023-v1.11_lossyear_00N_030E.tif")
loss_year_00N_030E <- terra::rast("data-raw/deforestation/Hansen_GFC-2023-v1.11_lossyear_00N_030E.tif")
loss_year_00N_030E <- terra::rast("data-raw/deforestation/Hansen_GFC-2023-v1.11_lossyear_00N_030E.tif")

plot(st_geometry(admin1_sf))
plot(loss_year, add=TRUE)