# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# use pacman::p_load to load packages in one line
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

# How to read geospatial data  --------------------------------------------
# shapefile
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

# Basic on sf -------------------------------------------------------------
# Create, get, set or replace the coordinate reference system (CRS)
st_crs(admin1_sf) 

# Transform coordinates of object to new projection. 
# change it to pseudo-mercator 
# use local projection (e.g., 21037) for more accurate calculation
admin1_sf_21037<-st_transform(admin1_sf, 21037) %>% 
  mutate(area_km=st_area(geometry)/1000000)
tm_shape(admin1_sf_21037) + 
  tm_fill("area_km",style="quantile")


# Group data 
admin1_sf_21037 %>% 
  group_by(ADM0_EN) %>% 
  st_drop_geometry() %>%
  summarise(sum=sum(area_km))

# Merge spatial datasets 
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% 
  filter(fclass=="primary" | fclass=="secondary") %>% 
  st_transform(21037) 

# plot road network for the entire country of tanzania
tm_shape(admin1_sf_21037) + 
  tm_borders(lwd=0.5) + 
  tm_shape(road) + 
  tm_lines(palette = "viridis",
           lwd = 2)

#plot road network for arusha
admin1_arusha_sf_21037 <- admin1_sf %>% 
  filter(ADM1_EN=="Arusha") %>% 
  st_transform(21037) 

road_arusha_admin1 <- st_join(road, admin1_arusha_sf_21037) %>% 
  filter(!is.na(ADM1_PCODE))

tm_shape(admin1_sf) + 
  tm_borders(lwd=0.5) + 
  tm_shape(road_arusha_admin1) + 
  tm_lines(palette = "viridis",
           lwd = 2)

# Compute a buffer around this geometry/each geometry 
road_admin1_2km_buffer<-st_buffer(road,2000) %>% 
  st_union() %>%  # Combine geometries without resolving borders 
  st_transform(4326) %>% 
  st_make_valid()

tm_shape(road_admin1_2km_buffer) + 
  tm_polygons(col="red") + 
  tm_shape(admin1_sf) + 
  tm_borders()

# intersection 
sf_road_admin1_2km_buffer<-st_intersection(admin1_sf,road_admin1_2km_buffer)
tm_shape(sf_road_admin1_2km_buffer) + tm_borders()

# Save a geometry as a new file 
st_write(sf_road_admin1_2km_buffer,
         dsn="data-raw",
         layer="sf_road_admin1_2km_buffer.shp",
         driver = "ESRI Shapefile",
         append=FALSE)

# Basic on raster ---------------------------------------------------------
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")

# Check the mean of raster data
pop_mean <- terra::global(pop, fun="mean", na.rm=TRUE)
pop_mean

# for better visualization, you may aggregate raster object
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)

tm_shape(pop_10) + 
  tm_raster(palette = "viridis") + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5)

# Create a new raster object that has the same values 
#except for the cells that are NA in a mask object.  
pop_arusha_masked <- mask(pop_10,st_transform(admin1_arusha_sf, 4326))

tm_shape(pop_arusha_masked) + 
  tm_raster(palette = "viridis") + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5)

# Calculate values for a new Raster* object 
# from another Raster* object, using a formula.
pop_mean <- terra::global(pop_10, fun="mean", na.rm=TRUE)
pop_sd <- terra::global(pop_10, fun="sd", na.rm=TRUE)
pop_z_score<-terra::app(pop_10, fun = function(x) { (x - pop_mean$mean) / pop_sd$sd }) # calculate z score

tm_shape(pop_z_score) + 
  tm_raster(palette = "viridis") + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5)

# show in quantiles
tm_shape(pop_10) + 
  tm_raster(style="quantile",
            palette = "viridis") + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5)

# How to compute zonal statistics  ----------------------------------------
# Compute zonal statistics from raster
admin1_sf_pop<-admin1_sf %>% 
  mutate(pop=exact_extract(pop, .,"sum"))

tm_shape(admin1_sf_pop) + 
  tm_borders(lwd = 0.5) + 
  tm_fill("pop",style="quantile")

# Count number of points  
primary_school<-st_read("data-raw/primary_schools_2019.shp") %>% 
  st_transform(4326) 

points_by_polygon <- primary_school %>%
  st_join(., admin1_sf_pop) %>%
  group_by(ADM1_PCODE) %>%
  summarise(school_num = n()) %>%
  st_drop_geometry()

admin1_sf_primary_school<-admin1_sf_pop %>% 
  left_join(., points_by_polygon) %>%
  mutate(school_per_1000=school_num/pop*1000,
         school_per_1000=ifelse(is.na(school_per_1000),0,school_per_1000))

tm_shape(admin1_sf) + tm_borders() + tm_shape(primary_school) + tm_dots(col='blue')

tm_shape(admin1_sf_primary_school) + 
  tm_borders() + 
  tm_fill("school_per_1000",id="school_per_1000")

# How to visualize  -------------------------------------------------------
# plot()
png('figures/pop_plot.png')
plot(admin1_sf_pop['pop'])
dev.off()

# ggplot() + geom_sf() 
admin1_sf_pop %>% 
  ggplot()+
  geom_sf(aes(fill = pop))
ggsave("figures/pop_ggplot.png")

# qtm() 
qtm<-qtm(admin1_sf_pop,"pop")
tmap_save(qtm,"figures/pop_qtm.png")

# tm_shape() + tm_ploygons() 
tmap_polygon<-tm_shape(admin1_sf_pop) + tm_polygons("pop") 
tmap_save(tmap_polygon,"figures/pop_tmap_polygon.png")

# tm_shape() + tm_symbols() 
tmap_symbol<-tm_shape(admin1_sf_pop) + tm_symbols("pop") 
tmap_save(tmap_symbol,"figures/pop_tmap_symbol.png")
