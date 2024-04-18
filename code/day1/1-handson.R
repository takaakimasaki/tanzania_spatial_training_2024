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

# How to read geospatial data  --------------------------------------------
# read polygon shapefile
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid() #we will cover this function more closely on Day 2

# now let's see what this is
admin1_sf

## confirm Coordinate Reference n
# let's look closely what variables are included in this dataset
View(admin1_sf)
##geometry contains georeferencing information!

# basic functions in dplyr package (you can also check this cheatsheet: https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf)
# this symbol (%>% ) is called pipe, which can pass the value to line
# select variables
admin1_sf %>% dplyr::select("ADM1_EN")

# filter variables
admin1_arusha_sf <- admin1_sf %>% 
  filter(ADM1_EN=="Arusha") 

plot(st_geometry(admin1_arusha_sf))

#Q: Can we create a new object that contains locations from Zanzibar??

# Add new variables
admin1_sf <- admin1_sf %>% 
  mutate(Value = rnorm(n = dim(.)[1], mean = 0, sd = 1)) #generate a new variable called Value whose values are drawn from 
hist(admin1_sf$Value)

admin1_sf <- admin1_sf %>%
  mutate(Group = ifelse(Value > 0, "Group A", "Group B")) # randomly assign to Group A and Group B
table(admin1_sf$Group)

# Aggregate by group
summary_by_group <- admin1_sf %>%
  group_by(Group) %>%
  summarise(Value_mean = mean(Value))

View(summary_by_group)

# Orders the rows of a data frame by the values of selected columns 
admin1_sf %>% arrange(Value) 
admin1_sf %>% arrange(-Value) 

# How to join two datasets: summary_by_group, admin1_sf

admin1_sf <- admin1_sf %>%
  left_join(., summary_by_group, by="Group")
##Note that you cannot join two spatial objects using left_join!
##To turn spatial object to data
summary_by_group <- summary_by_group %>%
  st_drop_geometry()

admin1_sf <- admin1_sf %>%
  left_join(., summary_by_group, by="Group")
View(admin1_sf)
##Now it works!

# Q2: Can you create a new object that takes the primary and secondary road
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp"))
table(road$fclass)

# read raster data
pop<-terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")

# Check the mean of raster data
pop_mean <- terra::global(pop, fun="mean", na.rm=TRUE)
pop_mean

# for better visualization, you may aggregate raster object
pop_10 <-terra::aggregate(pop, fact=10, fun="sum", na.rm=TRUE)
plot(pop_10)

# How to map raster data with polygon shapefile
plot(pop_10)
plot(st_geometry(admin1_sf), add=TRUE)

# Let's make it an interative map 
tmap_mode("view")
map <- tm_shape(pop_10) + 
  tm_raster(palette = "viridis", style="quantile", n=10) + 
  tm_shape(admin1_sf) + 
  tm_borders(lwd = 0.5) +
  tm_layout(
    legend.outside = TRUE,
    legend.position = c("right", "top"),
    legend.title.size
    = 1.2,
    legend.text.size = 0.8
  )
map
## We will cover data visualizations more in depth!
