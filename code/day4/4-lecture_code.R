# install packages --------------------------------------------------------
pacman::p_load(here,sf,terra,tidyverse,dplyr,tmap,exactextractr,paletteer,haven,labelled, survey)

admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid()

################################################################################
#Example: DHS data
##Open your dataset
HR_dta <- read_dta("data-raw/microdata/TZ_2022_DHS_04222024_160_47858/TZHR82DT/TZHR82FL.DTA")

##Creating a new categorical variable
##check categorical labels
print_labels(HR_dta$hv201)

##generate a binary variable indicating access to piped water
HR_dta <- HR_dta %>%
  mutate(piped_water = ifelse(hv201 %in% c(10, 11, 12, 13, 14),1,0)) %>%
  set_value_labels(piped_water = c(yes = 1, no = 0)) %>%
  labelled::set_variable_labels(piped_water = "Access to piped water")

##check
table(HR_dta$hv201)
table(HR_dta$piped_water)

##get weight variable
HR_dta <- HR_dta %>%
  mutate(wt = hv005/1000000)

##add electricity variable
HR_dta <- HR_dta %>%
  mutate(electricity = hv206) %>%
  set_value_labels(electricity = c(yes = 1, no = 0)) %>%
  labelled::set_variable_labels(electricity = "Access to electricity")

##now let's use survey package called
##set survey design
mysurvey <- svydesign(id=HR_dta$hv021, ##PSU
                      data=HR_dta,
                      strata=HR_dta$hv022, ##Strata
                      weight=HR_dta$wt,
                      nest=T)
options(survey.lonely.psu="adjust")

#attach your data
attach(HR_dta) #The attach() function in R allows users to access variables in a data frame without calling the data frame

#to get frequency
svytable(~piped_water, mysurvey) ##note it's now weighted freqnecies
table(HR_dta$piped_water)

#to get proportion
prop.table(svytable(~piped_water, mysurvey))
prop.table(svytable(~electricity, mysurvey))
##check if it's right (https://www.statcompiler.com/en/)

table1 <- prop.table(svytable(~piped_water, mysurvey))
print_labels(mysurvey$variables$hv024)

#cross tabulation
electricity_by_region <- svyby(~electricity, 
                               by=~hv024,
                               design = mysurvey,
                               FUN=svymean,
                               vartype=c("se","ci"))

piped_water_by_region <- svyby(~piped_water, 
                               by=~hv024,
                               design = mysurvey,
                               FUN=svymean,
                               vartype=c("se","ci"))
################################################################################
#Now map the results
#first generate a new string variable that contains regional names
region_label <- HR_dta %>%
  mutate(ADM1_EN = str_to_title(as.character(to_character(hv024)))) %>%
  group_by(ADM1_EN) %>%
  summarise(hv024 = mean(hv024))

electricity_by_region_dta <- electricity_by_region %>%
  as.data.frame() %>%
  left_join(., region_label) %>%
  select(ADM1_EN, electricity)

electricity_by_region_dta <- electricity_by_region_dta %>%
  mutate(ADM1_EN =ifelse(ADM1_EN=="Dar Es Salaam","Dar-es-salaam",ADM1_EN))

piped_water_by_region_dta <- piped_water_by_region %>%
  as.data.frame() %>%
  left_join(., region_label) %>%
  select(ADM1_EN, piped_water)

piped_water_by_region_dta <- piped_water_by_region_dta %>%
  mutate(ADM1_EN =ifelse(ADM1_EN=="Dar Es Salaam","Dar-es-salaam",ADM1_EN))
admin1_sf <- admin1_sf %>%
  left_join(., electricity_by_region_dta) %>%
  left_join(., piped_water_by_region_dta) 


#NOTICE! Dar es Salaam has not been matched. How can we address this issue?

#Finally, map the outcome
tm_shape(admin1_sf) + 
  tm_borders() +
  tm_fill("electricity",
          style="quantile")  + 
  tm_layout(legend.outside=TRUE, 
            legend.position= c("left", "bottom"),
            legend.title.size
            =2, legend.text.size=1) 

#let's conduct a simple regression looking at the relationship between access to piped water and electricity
scatter_plot <- ggplot(data = admin1_sf, aes(x = piped_water, y = electricity)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = TRUE) +  # Add regression line with confidence interval
  labs(x = "Access to piped water", y = "Access to electricity", title = "Scatter Plot with Regression Line")
scatter_plot

##add region labels
scatter_plot <- ggplot(data = admin1_sf, aes(x = piped_water, y = electricity)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = TRUE) +  # Add regression line with confidence interval
  geom_text(aes(label = ADM1_EN), vjust = -0.5, size = 2) +  # Add labels to points
  labs(x = "Access to piped water", y = "Access to electricity", title = "Scatter Plot with Regression Line")
scatter_plot

#how do we map data at the EA level
ea_sf <- st_read("data-raw/microdata/TZ_2022_DHS_04222024_160_47858/TZGE81FL/TZGE81FL.shp")
##check PSU variables
summary(ea_sf$DHSCLUST)
summary(HR_dta$hv021)

##generate EA level dataset where electricity is 1 if at least one person in EA has access to electricity
electricity <- HR_dta %>%
  mutate(DHSCLUST = hv021) %>%
  group_by(DHSCLUST) %>%
  summarise(electricity=max(electricity))

##merge EA shapefile with electricity data
ea_sf_dta <- ea_sf %>%
  left_join(., electricity) %>%
  mutate(electricity_str = ifelse(electricity==1,"Yes","No"))

##map
map <- tm_shape(admin1_sf) +
  tm_borders() +
  tm_shape(ea_sf_dta) +
  tm_symbols(
    shape ="electricity_str",
    shapes = c(19, 4),
    size=0.5,
    col="black",
    alpha=0.5,
    title.shape = "Access to electricity")+
  tm_layout(
    title="Access to electricity",
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.title.size
    = 1.5,
    legend.text.size
    = 1.2)

tmap_save(map,"figures/access_to_electricity.png")


#How can we merge other geospatial data to survey data
#now we know the locations of households at the EA level
#we can use georeferencing information in the survey to match with other geospatial data
##read cropland data
cropland <- terra::rast("data-raw/lcc_tza_cropland.tif")

##read population data
pop <- terra::rast("data-raw/tza_ppp_2020_UNadj_constrained.tif")

##turn household survey to sf object
hh_sf <- HR_dta %>%
  dplyr::select(hv021, #PSU
                hv022, #Strata
                hv024, #Region
                wt, 
                piped_water,
                electricity,
                wealth=hv270) %>%
  rename(DHSCLUST = hv021) %>%
  left_join(ea_sf, .)

##match locations of households with cropland data
hh_sf <- hh_sf %>%
  mutate(cropland = terra::extract(cropland, .)$lyr.1) %>%
  mutate(cropland_str = ifelse(cropland==1,"Yes","No"))

map <- tm_shape(admin1_sf) +
  tm_borders() +
  tm_shape(hh_sf) +
  tm_symbols(
    shape ="cropland_str",
    shapes = c(19, 4),
    size=0.5,
    col="black",
    alpha=0.5,
    title.shape = "Cropland")+
  tm_layout(
    title="cropland",
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.title.size
    = 1.5,
    legend.text.size
    = 1.2)
map

##compute 1km buffer around each EA location
hh_sf_1km <- hh_sf %>%
  st_transform(21037) %>%
  st_buffer(., 1000) %>%
  st_transform(4326)

##let's compute the mean of raster data in the 1km buffer of each EA
hh_sf_1km <- hh_sf_1km %>%
  mutate(pop_1km =  exact_extract(pop, .,"mean"))

##run regression between relative wealth and population density
hh_sf_1km_df <- hh_sf_1km %>%
  st_drop_geometry(.)

##set survey design
mysurvey <- svydesign(id=hh_sf_1km_df$DHSCLUST, ##PSU
                      data=hh_sf_1km_df,
                      strata=hh_sf_1km_df$hv022, ##Strata
                      weight=hh_sf_1km_df$wt,
                      nest=T)
attach(hh_sf_1km_df)

##run a simple regression accounting for sampling weights
##look at relationship between population density and relative wealth by running a simple OLS
ols_model <- svyglm(wealth ~ pop_1km, design = mysurvey)