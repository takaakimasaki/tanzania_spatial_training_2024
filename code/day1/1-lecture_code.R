# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# use pacman::p_load to load packages in one line
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

# How to read geospatial data  --------------------------------------------
# Quiz 1 - Polygon or raster?
## examples of vector data
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>% 
  st_make_valid()

plot(st_geometry(admin1_sf))

admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>% 
  st_make_valid()

road <- st_read("data-raw/tanzania_gis_osm_paved.shp") %>% 
  st_make_valid()

plot(st_geometry(road))

schools <- st_read("data-raw/primary_schools_2019.shp")
plot(st_geometry(schools))

## examples of raster data
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)
plot(pop_10)
terra::global(pop, fun="sum", na.rm=TRUE)

## you need to download data from https://drive.google.com/drive/folders/1MLANkoF-Es4kc3I5dEnr76jKv3kYmV8D!
precip <- terra::rast("data-raw/precip/cru_ts4.07.1901.2022.pre.dat.nc/cru_ts4.07.1901.2022.pre.dat.nc") 
precip_tza <- precip$pre_1 %>%
  crop(., admin1_sf) %>%
  mask(., admin1_sf)

plot(precip_tza)
  
  