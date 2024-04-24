# install packages --------------------------------------------------------
rm(list = ls())
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer, rhdf5, imager)

admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid()

################################################################################
#Example 1: Historic Agricultural Drought Frenquecy Maps
drought_freq <- 1 # choose from 1-100% annual historic frequency 

# (based on 39 years of obs.- NOT A PROBABILISTIC MODEL)
drought_t <- 30 #or 50
drought <- terra::rast(paste0("data-raw/drought/gsg5/drought_hist_",drought_t,"pc_1km.tif")) %>%
  crop(., admin1_sf)

plot(drought)
plot(st_geometry(admin1_sf), add=TRUE)

################################################################################
#Example 2: Flood map
##Band Values:
#0 - No Data
#1 - Low Flood (0-33% of pixels are detected with water)
#2 - Moderate Flood (34% - 66% of pixels are detected with water)
#3 - High Flood (67% - 99% of pixels are detected with water)
#9 - Clouds
#10 - Land Mass

flood <- terra::rast(paste0("data-raw/flood/hdx/fl-20231117-tza-00.tiff")) %>%
  crop(., admin1_sf)  %>%
  terra::app(function(x) ifelse(x == 0 | x == 9, NA, x)) %>%
  terra::app(function(x) ifelse(x == 1 | x == 2 | x == 3, 1, 0)) 

plot(flood)
plot(st_geometry(admin1_sf), add=TRUE)

################################################################################
#Example 3: Temperature from CRU
# Read temperature data
tmp <- terra::rast("data-raw/temp/cru_ts4.07.1901.2022.tmp.dat.nc/cru_ts4.07.1901.2022.tmp.dat.nc") 
tmp
##*stn*	number of stations contributing to each interpolation	count 0-8
names(tmp)

##Get avg. monthly temperature for December 2022
tmp_2022_12 <- tmp$tmp_1464 %>%
  crop(., admin1_sf)

plot(tmp_2022_12)
plot(st_geometry(admin1_sf), add=TRUE)
#tmp_stack <- c(tmp)

tmp_adm1 <- admin1_sf %>% 
  exact_extract(tmp, ., append_cols = 'ADM1_EN', 'mean', force_df = TRUE) %>%
  as_tibble() 

##1. assign year and month to variable names  
# retain only columns with mean tmpitation
head(tmp_adm1)
only_tmp_adm1 <- tmp_adm1 %>%
  dplyr::select(ADM1_EN, starts_with("mean.tmp_"))

# Get the number of columns excluding ADM1_EN
n_cols <- length(only_tmp_adm1) - 1

# Specify the starting date
start_date <- as.Date("1901-01-16")

# Generate a sequence of dates from the starting date
dates <- seq.Date(start_date, by = "month", length.out = n_cols)
new_cols <- format(dates, "%Y-%m") 

# Combine "ADM1_EN" with the new column names
final_col_names <- c("ADM1_EN", new_cols)

# Assign the new column names to the dataframe
colnames(only_tmp_adm1) <- final_col_names
tmp_sf <- left_join(admin1_sf, only_tmp_adm1, by = "ADM1_EN")
View(tmp_sf)

tm_shape(tmp_sf) +
  tm_fill("1903-02")

################################################################################
#Optional: how to combine HDF datasets
#see https://code.earthengine.google.com/?asset=projects/UNFAO/ASIS/HDF
#{'value': 251, 'description': 'incomplete season'}, {'value': 252, 'description': 'insufficient data'}, {'value': 253, 'description': 'no seasonality (or no season2)'}, {'value': 254, 'description': 'no cropland / no grassland'}
drought_30_c_s1 <- terra::rast(paste0("data-raw/drought/asi/HDF_C_S1_LA30.tif")) %>%
  #terra::app(function(x) ifelse(x == 252, NA, x)) %>%
  terra::app(function(x) ifelse(x > 250 | is.na(x), 0, x)) 
drought_30_c_s2 <- terra::rast(paste0("data-raw/drought/asi/HDF_C_S2_LA30.tif")) %>%
  #terra::app(function(x) ifelse(x == 252, NA, x)) %>%
  terra::app(function(x) ifelse(x > 250 | is.na(x), 0, x))
drought_30_p_s1 <- terra::rast(paste0("data-raw/drought/asi/HDF_P_S1_LA30.tif")) %>%
  #terra::app(function(x) ifelse(x == 252, NA, x)) %>%
  terra::app(function(x) ifelse(x > 250 | is.na(x), 0, x))
drought_30_p_s2 <- terra::rast(paste0("data-raw/drought/asi/HDF_P_S2_LA30.tif")) %>%
  #terra::app(function(x) ifelse(x == 252, NA, x)) %>%
  terra::app(function(x) ifelse(x > 250 | is.na(x), 0, x))
drought_30 <- max(c(drought_30_c_s1, drought_30_p_s1, drought_30_c_s2, drought_30_p_s2))
drought_30_freq <- drought_30 %>%
  terra::app(function(x) ifelse(x >= 1, 1, 0))
drought_freq <- drought %>%
  terra::app(function(x) ifelse(x >= 1, 1, 0))
