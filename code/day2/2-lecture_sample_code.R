# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# use pacman::p_load to load packages in one line
## what you are doing here is to install additional functions that do not come with R software itself
## If you are STATA users, it's similar to "ssc install."
## to run code, you can use a shortcut Ctrl + Enter
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

# read polygon shapefile
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid() #we will cover this function more closely on Day 2

# let's reproject from EPSG4326 to UTM (EPSG 21037 for Tanzania according to https://epsg.io/)
admin1_sf_rpj <- admin1_sf %>%
  st_transform(21037) #this function transforms the given object

tmap_mode("plot")
tm_shape(admin1_sf_rpj) +
  tm_borders() 

# now reproject based on different EPSG code
admin1_sf_rpj_v2 <- admin1_sf %>%
  st_transform(2736) #this code is typically used for Mozambique

# compare two objects reprojected to different CRS
e <- c(-548109.3,1317237,9000000,10000000)
plot(st_geometry(admin1_sf_rpj), border = "red", xlim = c(e[1], e[3]), ylim = c(e[2], e[4]))
plot(st_geometry(admin1_sf_rpj_v2), border = "blue", xlim = c(e[1], e[3]), ylim = c(e[2], e[4]), add=TRUE)

# read raster data
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")

# for better visualization, you may aggregate raster object
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)
plot(pop_10)

# you can also reproject raster data
pop_10_rpj <- project(pop_10, crs(admin1_sf_rpj_v2)) #projected to EPSG code more appropriate for Mozambique, not Tanzania!

# let's see how these different datasets overlap with each other.
plot(pop_10_rpj)
plot(st_geometry(admin1_sf_rpj), add=TRUE)
plot(st_geometry(admin1_sf_rpj_v2), border="blue", add=TRUE)
