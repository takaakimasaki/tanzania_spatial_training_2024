# install packages --------------------------------------------------------
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer)

#TASK 3: Calculate # or % of rural population exposed to drought risks (Source: https://data.apps.fao.org/catalog/iso/f8568e67-46e7-425d-b779-a8504971389b)
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid()
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")


drought_freq <- 1 # choose from 1-100% annual historic frequency 

# (based on 39 years of obs.- NOT A PROBABILISTIC MODEL)
drought_t <- 30 #or 50

drought <- terra::rast(paste0("data-raw/drought/drought/drought_hist_",drought_t,"pc_1km.tif")) %>%
  crop(., admin1_sf) %>%
  terra::app(function(x) ifelse(x > drought_freq, 1, 0)) #1 if drought prone, 0 otherwise

urban <- st_read("data-raw/grump-v1-urban-ext-polygons-rev02-shp/global_urban_extent_polygons_v1.01.shp") %>%
  dplyr::filter(ISO3 == "TZA") %>%
  st_make_valid()

#REVO: Can you write the following scripts? 
##1. Calculate the number and share of rural population living in drought prone areas 
##1.1 - obtain the number of people who are in rural areas by removing those people living in urban

# compute total population 

total_pop <- admin1_sf %>% 
  mutate(pop_all=exact_extract(pop, .,'sum'))

# compute urban population 

# Intersect urban shapefile with Tanzania regional level shapefile 

urban_sf <- urban %>%
  st_intersection(admin1_sf, .) %>%
  st_collection_extract() 

# Compute zonal statistics - (sum population cells that fall into each urban polygon) 
# then aggregate results by region

urban_pop <- urban_sf %>% 
  mutate(pop_urban=exact_extract(pop, .,'sum')) %>%
  group_by(ADM1_EN) %>%
  summarise(pop_urban = sum(pop_urban)) %>%
  st_drop_geometry()

# compute and plot rural population 

# rural pop

# Merge total population dataset with urban dataset (we perform a full join as
# total_pop dataset has all the 31 regions while urban_pop only contains regions with 
# urban population. Areas without an urban population (Kaskazini Unguja and Simiyu) receive values of 0)

rural_pop <- full_join(total_pop, urban_pop, by = "ADM1_EN")
rural_pop <- rural_pop %>%
  mutate(pop_urban = coalesce(pop_urban, 0),
         pop_rural = pop_all - pop_urban) %>%
  st_as_sf()

# plot 

map <- tm_shape(rural_pop) + 
  tm_fill(
    col = "pop_rural", 
    style = "quantile",
    title = "Rural Population",
    palette = "YlOrBr",
    legend.reverse = TRUE,) +
  tm_borders() +
  tm_text("ADM1_EN",
          size=0.8,
          col="black",
          ymod=0.8) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )

tmap_save(tm = map,
          filename = here::here("figures","rural_population.png"), 
          dpi=600)


##1.2 - calculate the number of total rural population and rural population living in drought prone areas

# intersect drought areas with total population 

# Check whether our drought raster and population raster have the same Coordination Reference System 
# then make them consistent if not 

if (crs(drought) != crs(pop)) {
  crs(drought) <- crs(pop)
}

# Resample drought data to have the same spatial resolution as population data

drought_resampled <- resample(drought, pop, method="ngb")

# Adjusting the extent of drought data to match pop data

drought_resampled <- extend(drought_resampled, pop)

# Now both rasters have the same CRS, resolution, and extent, we can proceed to intersect
# them and compute zonal statistics for the whole of Tanzania and Urban Areas

# Intersecting 

exposed_pop <- terra::intersect(drought_resampled, pop)

# computing number of people in drought prone areas across Tanzania 

drought_pop <- admin1_sf %>%
  mutate(drought_all = exact_extract(exposed_pop, .,'sum'))
 
# computing number of people in drought prone urban areas

urban_dry <- urban_sf %>% 
  mutate(drought_urban=exact_extract(exposed_pop, .,'sum')) %>%
  group_by(ADM1_EN) %>%
  summarise(drought_urban = sum(drought_urban)) %>%
  st_drop_geometry()


# estimate and plot rural population exposed to drought 

rural_dry <- full_join(drought_pop, urban_dry, by = "ADM1_EN")
rural_pop <- rural_pop %>%
  dplyr::select(ADM1_EN, pop_rural) %>%
  st_drop_geometry()
rural_dry <- full_join(rural_dry, rural_pop, by = "ADM1_EN")
rural_dry <- rural_dry %>%
  mutate(drought_urban = coalesce(drought_urban, 0),
         drought_rural = drought_all - drought_urban,
         share_drought = drought_rural/pop_rural) %>%
  st_as_sf()


# Generate Plots

map <- tm_shape(rural_dry) + 
  tm_fill(
    col = "drought_rural", 
    style = "quantile",
    title = "Exposed Population",
    palette = "YlOrBr",
    legend.reverse = TRUE,) +
  tm_borders() +
  tm_text("ADM1_EN",
          size=0.8,
          col="black",
          ymod=0.8) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )

tmap_save(tm = map,
          filename = here::here("figures","drought_rural.png"), 
          dpi=600)

map <- tm_shape(rural_dry) + 
  tm_fill(
    col = "share_drought", 
    style = "quantile",
    title = "% Exposed",
    palette = "YlOrBr",
    legend.reverse = TRUE,) +
  tm_borders() +
  tm_text("ADM1_EN",
          size=0.8,
          col="black",
          ymod=0.8) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )

tmap_save(tm = map,
          filename = here::here("figures","drought_share.png"), 
          dpi=600)