# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

################################################################################
#reprojection
# use pacman::p_load to load packages in one line
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

# How to read geospatial data  --------------------------------------------
# shapefile
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid()

# look up CRS
st_crs(admin1_sf)

# reproject 
admin1_sf_21037 <- admin1_sf %>%
  st_transform(21037) 

# confirm that CRS changed!
admin1_sf_21037

# now compute area in km2
admin1_sf_21037 <- admin1_sf_21037 %>%
  mutate(area_km = as.numeric(st_area(geometry))/1000000)
sum(admin1_sf_21037$area_km) 

################################################################################
#zonal statistics
#read population data
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")
#reduce resolution for visualization purpose
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)

map <- tm_shape(pop_10) + 
  tm_raster(palette = "viridis") + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5) +
  tm_layout(
    legend.outside = TRUE,
    legend.position = c("right", "top"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )


tmap_save(tm = map,
          filename = here::here("figures","pop_10.png"), 
          dpi=600)


#calculate total population by region based on ADM1 shapefile
admin1_sf_pop <-admin1_sf %>% 
  mutate(pop=exact_extract(pop, .,"sum"))

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

map <- tm_shape(admin1_sf_pop) + 
  tm_borders(lwd = 0.5) + 
  tm_fill("pop",style="quantile") +
  tm_layout(
    legend.outside = TRUE,
    legend.position = c("right", "top"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )

tmap_save(tm = map,
          filename = here::here("figures","pop_by_region.png"), 
          dpi=600)