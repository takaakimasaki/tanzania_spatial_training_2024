# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# use pacman::p_load to load packages in one line
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

# How to read geospatial data  --------------------------------------------
# read polygon shapefile
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp")

# check if the shapefile is valid
sf::st_is_valid(admin1_sf)

# this shapefile is not valid. To make it valid:
admin1_sf <- st_make_valid(admin1_sf)
sf::st_is_valid(admin1_sf)

# visually check the shape
tmap_mode("view")
tm_shape(admin1_sf) + tm_borders()

# Basics on tidyverse  ----------------------------------------
# To see what’s in the attribute table 
head(admin1_sf) 

# To report the structure of the data set
str(admin1_sf)

# To report summary statistics 
summary(admin1_sf) 

# select variables
# this symbol (%>% ) is called pipe, which can pass the value to line
admin1_sf %>% dplyr::select("ADM1_EN")

# filter variables
admin1_arusha_sf <- admin1_sf %>% 
  filter(ADM1_EN=="Arusha") 

plot(st_geometry(admin1_arusha_sf))

# Add new variables
admin1_sf %>% 
  mutate(ADM1_EN_UPPER=toupper(ADM1_EN)) # change to uppercase

# Orders the rows of a data frame by the values of selected columns 
admin1_sf %>% arrange(ADM1_EN) # first column gets the priority in sorting

# Basic on sf -------------------------------------------------------------
# Create, get, set or replace the coordinate reference system (CRS)
st_crs(admin1_sf) 

# read line data
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% 
  filter(fclass=="primary" | fclass=="secondary") 

# read raster data
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")

# Check the mean of raster data
pop_mean <- terra::global(pop, fun="mean", na.rm=TRUE)
pop_mean

# for better visualization, you may aggregate raster object
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)
plot(pop_10)

# let's map all geospatial data loaded
tm_shape(admin1_sf) +
  
