# install packages --------------------------------------------------------
pacman::p_load(here,sf,terra,tidyverse,dplyr,janitor,tmap,tidyr,forcats,exactextractr,paletteer)

#TASK 2: Work with climate data - precipitation and temperature
# Read ADM1 shapefile
admin1_sf <- st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>%
  st_make_valid()

# Read precipitation/temperature data
precip <- terra::rast("data-raw/precip/cru_ts4.07.1901.2022.pre.dat.nc/cru_ts4.07.1901.2022.pre.dat.nc") 

#temp <- terra::rast("data-raw/temp/cru_ts4.07.1901.2022.tmp.dat.nc") 
precip
names(precip)

precip_stack <- c(precip)

precip_adm1 <- admin1_sf %>% 
  exact_extract(precip_stack, ., append_cols = 'ADM1_EN', 'mean', force_df = TRUE) %>%
  as_tibble() 

#REVO: Can you write the following scripts? 

##1. assign year and month to variable names of precip_adm1? 

# retain only columns with mean precipitation

only_precip_adm1 <- precip_adm1 %>%
  dplyr::select(ADM1_EN, starts_with("mean.pre_"))

# Get the number of columns excluding ADM1_EN
n_cols <- length(only_precip_adm1) - 1

# Specify the starting date
start_date <- as.Date("1901-01-16")

# Generate a sequence of dates from the starting date
dates <- seq.Date(start_date, by = "month", length.out = n_cols)
new_cols <- format(dates, "%Y-%m") 

# Combine "ADM1_EN" with the new column names
final_col_names <- c("ADM1_EN", new_cols)

# Assign the new column names to the dataframe
colnames(only_precip_adm1) <- final_col_names

##2. Map month avg. precipitation by region for December 2022?

# merge precipitation dataframe with Tanzania regional level shapefile 

precip_sf <- left_join(admin1_sf, only_precip_adm1, by = "ADM1_EN")
precip_sf <- precip_sf %>%
  st_as_sf()

# generate map

map <- tm_shape(precip_sf) + 
  tm_fill(
    col = "2022-12", 
          style = "quantile",
          title = "Precipitation Dec 2022",
          palette = "GnBu",
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
          filename = here::here("figures","precipitation_dec_2022.png"), 
          dpi=600)
  

##3. Plot trends (in line graph) in month avg. precipitation by region for Jan 1901 - Dec 2022?

# Reshape the dataframe from wide to long format 

precip_long <- only_precip_adm1 %>%
  gather(., key = "month", value = "precip", 2:1465) %>%
  mutate(ADM1_EN = fct_reorder(ADM1_EN, desc(precip)))

# Convert the key into a date (year and month) object

precip_long$month <- lubridate::ym(precip_long$month)

# Group regions into zones

zones <- list(
  "Central Zone" = c("Dodoma", "Singida", "Tabora"),
  "Coastal Zone" = c("Dar-es-salaam", "Lindi", "Morogoro", "Mtwara", "Pwani"),
  "Lake Zone" = c("Geita", "Kagera", "Mara", "Mwanza", "Shinyanga", "Simiyu"),
  "Northern Zone" = c("Arusha", "Kilimanjaro", "Manyara", "Tanga"),
  "Southern Highlands Zone" = c("Iringa", "Mbeya", "Njombe", "Rukwa", "Ruvuma", "Songwe"),
  "Western Zone" = c("Katavi", "Kigoma"),
  "Zanzibar" = c("Mjini Magharibi", "Kaskazini Pemba", "Kusini Pemba", "Kaskazini Unguja", "Kusini Unguja")
)

# assign each region to its zone 

precip_long_zone <- precip_long %>%
  mutate(zone = case_when(
    ADM1_EN %in% zones[["Central Zone"]] ~ "Central Zone",
    ADM1_EN %in% zones[["Coastal Zone"]] ~ "Coastal Zone",
    ADM1_EN %in% zones[["Lake Zone"]] ~ "Lake Zone",
    ADM1_EN %in% zones[["Northern Zone"]] ~ "Northern Zone",
    ADM1_EN %in% zones[["Southern Highlands Zone"]] ~ "Southern Highlands Zone",
    ADM1_EN %in% zones[["Western Zone"]] ~ "Western Zone",
    ADM1_EN %in% zones[["Zanzibar"]] ~ "Zanzibar",
    TRUE ~ NA_character_ 
  )) %>%
  mutate(zone = fct_reorder(zone, desc(precip)))

# Plot trend in precipitation 

# Region 

trend_reg <- ggplot(data=precip_long[which(precip_long$month >= "2010-01-01"),], aes(x = month, y = precip, fill = ADM1_EN)) +
  geom_area() +
  labs(title = "Monthly Mean Precipitation by Region",
       y = "Precipitation") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 30),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),  
        legend.key.size = unit(1.5, 'cm'),
        axis.text.x = element_text(size = 30),
        axis.title = element_blank(),
        axis.text.y = element_blank())

# Zone

trend_zone <- ggplot(data=precip_long_zone[which(precip_long$month >= "2010-01-01"),], aes(x = month, y = precip, fill = zone)) +
  geom_area() +
  scale_fill_brewer(palette = "GnBu", direction = -1) + 
  labs(title = "Monthly Mean Precipitation by Zone",
       y = "Precipitation") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 30),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),  
        legend.key.size = unit(1.5, 'cm'),
        axis.text.x = element_text(size = 30),
        axis.title = element_blank(),
        axis.text.y = element_blank())

ggsave(trend_reg, filename = here::here("figures", "precipitation_trend.png"), height = 20, width = 49, dpi = 300)
ggsave(trend_zone, filename = here::here("figures", "precipitation_trend_zone.png"), height = 20, width = 49, dpi = 300)




