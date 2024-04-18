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

tmap_mode("plot")
tm_shape(admin1_sf, projection = 8857) +
  tm_borders()

tm_shape(admin1_sf) +
  tm_borders(col="blue")

# read raster data
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")
# for better visualization, you may aggregate raster object
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)
plot(pop_10)

# tm_shape(admin1_sf, projection = 8857) +
tmap_mode("view")
tm_shape(pop_10, raster.warp = FALSE) + 
  tm_raster(palette = "viridis", style="quantile", n=10) + 
  tm_shape(admin1_sf, projection = 8857) +
  tm_borders() 
  
