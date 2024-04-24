# install packages --------------------------------------------------------
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

#TASK 1: Overlay various layers of spatial data
# Spatial data clean-up procedure  ----------------------------------------
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp")

# Read in landcover data (https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=form)
# you can find codebook for LCCS Landcover data (https://datastore.copernicus-climate.eu/documents/satellite-land-cover/D5.3.1_PUGS_ICDR_LC_v2.1.x_PRODUCTS_v1.1.pdf)
lc <- terra::rast("data-raw/landcover/dataset-satellite-land-cover-dac2f583-6e2f-4b8d-81e8-35bb0d545ad5/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc") 

# get variable names
names(lc)

# subset dataset to variable of interest
lcc <- lc["lccs_class"]
lcc

# get lc data for tanzania
e <- st_bbox(admin1_sf)
lcc_tza <- crop(lcc, e)
lcc_tza <- mask(lcc_tza, admin1_sf)
#extract cropland
unique_values <- unique(values(lcc_tza))
plot(lcc_tza)
##value==10: Rainfed cropland
##value==11: Rainfed cropland
##value==12: Rainfed cropland
##value==20: Irrigated cropland
##value==30: Mosaic cropland 
##value==40: Mosaic cropland 
keep_values <- c(10, 11, 12, 20, 30, 40)
lcc_tza_cropland <- lcc_tza %>%
  terra::app(function(x) ifelse(x %in% keep_values, 1, 0))

plot(lcc_tza_cropland)

lcc_tza_tree <- lcc_tza %>%
  terra::app(function(x) ifelse(x >=50 & x<110, 1, 0))

plot(lcc_tza_tree)

lcc_tza_urban <- lcc_tza %>%
  terra::app(function(x) ifelse(x == 190, 1, 0))
plot(lcc_tza_urban)

terra::writeRaster(lcc_tza_urban, "figures/lcc_tza_urban.tif")

#check how this compare to other cropland map (https://www.fao.org/giews/countrybrief/country.jsp?code=TZA&lang=fr)

#let's plot this over road network, places of cities and so on
cities <- read_csv("data-raw/tz.csv")

cities_sf <- cities %>%
  mutate(!is.na(lng) & !is.na(lat)) %>%
  st_as_sf(., coords = c("lng", "lat"), crs = 4326)

lcc_tza_cropland_10 <- aggregate(lcc_tza_cropland, fact=10, fun="mean")

tmap_mode("plot")

map <- lcc_tza_cropland_10 %>%
  terra::app(function(x) ifelse(x == 0, NA, x)) %>%
  tm_shape(.) + 
  tm_raster(palette = "viridis", 
            alpha=0.5,
            title = "Share of cropland",
            legend.reverse = TRUE) + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5) +
  tm_shape(cities_sf) + 
  tm_dots(title="Main cities") + 
  tm_text("city",
          size=0.8,
          col="black",
          ymod=0.8) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("right", "top"),
    legend.title.size
    = 1.0,
    legend.text.size = 1.0
  ) +
  tmap_options(check.and.fix = TRUE) 

tmap_save(tm = map,
          filename = here::here("figures","cropland.png"), 
          dpi=600)

writeRaster(lcc_tza_cropland,
            file = here::here("data-raw","lcc_tza_cropland.tif"))
