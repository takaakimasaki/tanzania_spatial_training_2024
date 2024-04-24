# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

################################################################################
# use pacman::p_load to load packages in one line
## what you are doing here is to install additional functions that do not come with R software itself
## If you are STATA users, it's similar to "ssc install."
## to run code, you can use a shortcut Ctrl + Enter
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

################################################################################
# reprojection
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

################################################################################
#Zonal statistics
#First confirm that polygon shapefile and raster data have the same CRS
identical( st_crs(admin1_sf)$epsg, st_crs(pop)$epsg)
##You don't need to get upset when these two have different CRS
##you can always reproject
identical( st_crs(admin1_sf_rpj)$epsg, st_crs(pop)$epsg)
admin1_sf_rpj <- st_transform(admin1_sf_rpj, st_crs(pop)$epsg)
identical( st_crs(admin1_sf_rpj)$epsg, st_crs(pop)$epsg)
##Good it worked out!

#compute zonal statistics
admin1_sf_pop <-admin1_sf %>% 
  mutate(pop=exact_extract(pop, .,"sum"))

#let's see what we have gotten
map <- tm_shape(admin1_sf_pop) + 
  tm_borders(lwd = 0.5) + 
  tm_shape(pop) %>%
  tm_raster(pop) +
  tm_layout(
    legend.outside = TRUE,
    legend.position = c("right", "top"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )


################################################################################
#How to correct geometry issues
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp")

#let's check if the shapefile is valid
admin1_sf <- admin1_sf %>%
  mutate(valid = st_is_valid(.))

table(admin1_sf$valid)

#ok there are four polygons that have gotten some issues
admin1_sf_invalid <- admin1_sf %>%
  filter(valid=="FALSE")
st_is_valid(admin1_sf_invalid, reason=TRUE)
plot(st_geometry(admin1_sf_invalid))

#let's make it valid
admin1_sf_valid <- admin1_sf_invalid %>%
  st_make_valid()
st_is_valid(admin1_sf_valid)
plot(st_geometry(admin1_sf_valid))

################################################################################
#How to correct geometry issues
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid() #we will cover this function more closely on Day 2

################################################################################
#Geometric operations with vector data
##st_join()
cities <- read_csv("data-raw/tz.csv")
View(cities)

##turn this to sf
cities <- cities %>%
  st_as_sf(., coords = c("lng", "lat"), crs = 4326)

tm_shape(admin1_sf) +
  tm_borders() +
  tm_shape(cities) +
  tm_dots(size=0.9)

##now spatially join cities with admin1 shapefile to get region names for each of the cities
cities <- cities %>%
  st_join(., admin1_sf) %>%
  dplyr::select(city, ADM1_EN)

##st_area()
##get the area of polygons in km^2
admin1_sf <- admin1_sf %>%
  st_transform(21037) %>%
  mutate(area=as.numeric(st_area(.))/1000000)
sum(admin1_sf$area)

tm_shape(admin1_sf) +
  tm_fill("area")

##st_distance()
dar <- cities %>%
  filter(city=="Dar es Salaam") %>%
  st_transform(21037)

cities <- cities %>%
  st_transform(21037) %>%
  mutate(dist_to_dar = as.numeric(st_distance(., dar))/1000) %>%
  st_transform(4326)

View(cities)

##st_length()
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% 
  filter(fclass=="primary")

plot(st_geometry(admin1_sf))
plot(st_geometry(road), add=TRUE, col="blue")

road <- road %>% 
  st_transform(21037) %>% 
  mutate(length=as.numeric(st_length(.))/1000) %>%
  st_transform(4326)

admin1_sf_road <- admin1_sf %>%
  st_transform(21037) %>%
  mutate(area=as.numeric(st_area(.))/1000000) %>%
  st_transform(4326) %>%
  st_join(., road) %>% 
  group_by(ADM1_EN) %>% 
  summarise(road_len=sum(length))

tm_shape(admin1_sf_road) +
  tm_borders() +
  tm_fill("road_len", style="quantile", n=5, title="Road length in km")

##st_intersections()
road_dar <- st_intersection(dar,road)
plot(st_geometry(dar))
plot(st_geometry(road_dar), col="blue", add=TRUE)

################################################################################
#Geometric operations with raster data
##aggregate()
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)

plot(pop)
plot(st_geometry(admin1_sf), add=TRUE)

plot(pop_10)
plot(st_geometry(admin1_sf), add=TRUE)

##crop()
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)
dar <- admin1_sf %>%
  filter(ADM1_EN == "Dar-es-salaam")
pop_dar <- crop(pop_10, dar)
plot(pop_dar)
plot(st_geometry(dar), add=TRUE)

##mask()
pop_dar <- mask(pop_dar, dar)
plot(pop_dar)
plot(st_geometry(dar), add=TRUE)

##app
rural <- pop_10 %>%
  terra::app(., fun=function(x) ifelse(x<=5000, 1, 0))
plot(pop_10)
plot(st_geometry(admin1_sf), add=TRUE)

plot(rural)
plot(st_geometry(admin1_sf), add=TRUE)

##resample()
rural_dar <- rural %>%
  terra::app(., fun=function(x) ifelse(x==1, 1, NA)) %>%
  terra::crop(., dar) %>%
  terra::mask(., dar)

plot(rural_dar)

pop_dar <- pop %>%
  terra::crop(., dar) %>%
  terra::mask(., dar)

plot(rural_dar)
plot(st_geometry(dar), add=TRUE)

plot(pop_dar)
plot(st_geometry(dar), add=TRUE)

#get population in rural dar
rural_pop_dar <- rural_dar * pop_dar

rural_dar_rsmp = terra::resample(rural_dar, pop_dar)
rural_dar_rsmp

pop_dar_100 <- terra::aggregate(pop_dar, fact = 100, fun="sum", na.rm=TRUE)
rural_dar_rsmp_100 = terra::resample(rural_dar, pop_dar_100)

plot(rural_dar_rsmp)
plot(st_geometry(dar), add=TRUE)

##confirm that now rural_dar_rsmp has the same resolution and extent as pop
rural_pop_dar <- pop_dar*rural_dar_rsmp
plot(rural_pop_dar)
plot(st_geometry(dar), add=TRUE)