# install packages --------------------------------------------------------
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer, raster)
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp")

# how to download deforestation data from https://hub.arcgis.com/documents/gfw::tree-cover-loss/explore
# but first check the metadata: https://hub.arcgis.com/documents/gfw::tree-cover-loss/explore
#loss_year_00N_020E <- terra::rast("data-raw/deforestation/Hansen_GFC-2022-v1.10_lossyear_00N_020E.tif")
loss_year_00N_030E <- terra::rast("data-raw/deforestation/Hansen_GFC-2022-v1.10_lossyear_00N_030E.tif")
#loss_year_00N_040E <- terra::rast("data-raw/deforestation/Hansen_GFC-2022-v1.10_lossyear_00N_040E.tif")
#loss_year_10S_030E <- terra::rast("data-raw/deforestation/Hansen_GFC-2022-v1.10_lossyear_10S_030E.tif")
#loss_year_10S_040E <- terra::rast("data-raw/deforestation/Hansen_GFC-2022-v1.10_lossyear_10S_040E.tif")

#merge raster layers
#loss_year_all <- terra::merge(loss_year_00N_020E,
#                          loss_year_00N_030E,
#                          loss_year_00N_040E, 
#                          loss_year_10S_030E,
#                          loss_year_10S_040E)

plot(loss_year_all)
plot(st_geometry(admin1_sf), add=TRUE)


#now you can compute the total areas of tree loss by region
##let's do this first in Morogoro
morogoro <- admin1_sf %>%
  filter(ADM1_EN == "Morogoro")

##get the area where tree has been lost for Morogoro, let's first crop and mask the raster data
loss_year_00N_030E_morogoro <- loss_year_00N_030E %>%
  crop(., morogoro) %>%
  mask(., morogoro)

plot(st_geometry(morogoro), add=TRUE)
plot(loss_year_00N_030E_morogoro)
##to do this let's change 1 if tree is lost, 0 otherwise in the treeloss layer
loss_year_00N_030E_morogoro_1 <- loss_year_00N_030E_morogoro %>%
  terra::app(function(x) ifelse(x > 0, 1, NA))
plot(loss_year_00N_030E_morogoro_1)

##compute the area of tree loss
###see more this topic in https://cran.r-project.org/web/packages/exactextractr/readme/README.html
###get the area of each cell in km2
a <- terra::cellSize(loss_year_00N_030E_morogoro_1, unit="km")
###convert values of raster to area
loss_area_morogoro <- loss_year_00N_030E_morogoro_1*a 

###compute the sum of area 
loss_area_morogoro_output <- morogoro %>%
  mutate(tree_loss_km2 = exact_extract(loss_area_morogoro,., "sum"))

###now let's compute weighted average taking into account differences in cell size
loss_year_00N_030E_morogoro_10 <- loss_year_00N_030E_morogoro_1 %>%
  terra::app(function(x) ifelse(is.na(x), 0, x))

###compute area weighted mean and simple mean
loss_area_morogoro_output <- loss_area_morogoro_output %>%
  mutate(tree_loss_mean_area_wt = exact_extract(loss_year_00N_030E_morogoro_10,., "weighted_mean", weights = cellSize(loss_area_morogoro)),
         tree_loss_mean = exact_extract(loss_year_00N_030E_morogoro_10,., "mean"))

###ok finally, export the results
loss_area_morogoro_output %>%
  st_drop_geometry() %>%
  write_csv("output/morogoro_tree_loss.csv")

loss_area_morogoro_output %>%
  st_drop_geometry() %>%
  haven::write_dta("output/morogoro_tree_loss.dta")





##ok if we have time, can we use the landcover dataset to compute the total area of builtup?
# Read in landcover data (https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=form)
# you can find codebook for LCCS Landcover data (https://datastore.copernicus-climate.eu/documents/satellite-land-cover/D5.3.1_PUGS_ICDR_LC_v2.1.x_PRODUCTS_v1.1.pdf)
lc_2022 <- terra::rast("data-raw/landcover/dataset-satellite-land-cover-dac2f583-6e2f-4b8d-81e8-35bb0d545ad5/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc") 
lcc_2022 <- lc_2022["lccs_class"]

# get lc data for tanzania
lcc_tza_2022 <- lcc_2022 %>%
  crop(., admin1_sf) %>%
  mask(., admin1_sf)

unique_values <- unique(values(lcc_tza_2022))
plot(lcc_tza_2022)
##value==10: Rainfed cropland
##value==11: Rainfed cropland
##value==12: Rainfed cropland
##value==20: Irrigated cropland
##value==30: Mosaic cropland 
##value==40: Mosaic cropland 
keep_values <- c(10, 11, 12, 20, 30, 40)
lcc_tza_cropland_2022 <- lcc_tza_2022 %>%
  terra::app(function(x) ifelse(x %in% keep_values, 1, NA))

#HOW CAN WE COMPUTE THE TOTAL SUM OF CROPLAND AREA BY REGION??