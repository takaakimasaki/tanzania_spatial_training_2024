pacman::p_load(tidyverse,sf,tmap)

# Simple examples -------------------------------------------------------
# A 'bowtie' polygon:
p1 <- st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
st_is_valid(p1)
p1_valid <- st_make_valid(p1)
plot(p1)
plot(p1_valid)

# Square with wrong orientation:
p2 <- st_as_sfc("POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))")
st_is_valid(p2)
plot(p2)

# Inner ring with one edge sharing part of an edge of the outer ring:
p3 <- st_as_sfc("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0),(5 2,5 7,10 7, 10 2, 5 2))")
st_is_valid(p3)
p3_valid <- st_make_valid(p3)
plot(p3)
plot(p3_valid)

# Dangling edge:
p4 <- st_as_sfc("POLYGON((0 0, 10 0, 15 5, 10 0, 10 10, 0 10, 0 0))")
st_is_valid(p4)
p4_valid <- st_make_valid(p4)
plot(p4)
plot(p4_valid)

# Outer ring not closed:
p5 <- st_as_sfc("POLYGON((0 0, 10 0, 10 10, 0 10))")
st_is_valid(p5)
plot(p5)
p5_line <- st_cast(p5,"LINESTRING")
plot(p5_line)

# Two adjacent inner rings:
p6 <- st_as_sfc("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 8, 3 8, 3 1, 1 1), (3 1, 3 8, 5 8, 5 1, 3 1))")
st_is_valid(p6)
p6_valid <- st_make_valid(p6)
plot(p6)
plot(p6_valid)

# Polygon with an inner ring inside another inner ring:
p7 <- st_as_sfc("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (2 8, 5 8, 5 2, 2 2, 2 8), (3 3, 4 3, 3 4, 3 3))")
st_is_valid(p7)
p7_valid <- st_make_valid(p7)
plot(p7)
plot(p7_valid)

# Empty polygon
p8 <- st_polygon()
st_is_valid(p8)
st_is_empty(p8)

# Examples from Kondoa ----------------------------------------------------
# load data 
kondoa_ea_sf <- st_read("data-raw/Kondoa EAs_EUTF/Kondoa_EAs.shp")
example_invalid <- kondoa_ea_sf %>% 
  filter(!st_is_valid(.))

# let's see which one is invalid 
tmap_mode("view")
tm_shape(kondoa_ea_sf) + tm_borders() + tm_shape(example_invalid) + tm_fill("red") 


# create vertices 
example_valid <- st_make_valid(example_invalid)
vertices_valid <- example_valid %>% 
  st_cast("POINT")

vertices_invalid <- example_invalid %>% 
  st_cast("POINT")

# save 
st_write(example_valid,
         dsn="data-raw/validity_examples",
         layer="valid.shp",
         driver = "ESRI Shapefile",
         append=FALSE)

st_write(example_invalid,
         dsn="data-raw/validity_examples",
         layer="invalid.shp",
         driver = "ESRI Shapefile",
         append=FALSE)

st_write(vertices_valid,
         dsn="data-raw/validity_examples",
         layer="vertices_valid.shp",
         driver = "ESRI Shapefile",
         append=FALSE)

st_write(vertices_invalid,
         dsn="data-raw/validity_examples",
         layer="vertices_invalid.shp",
         driver = "ESRI Shapefile",
         append=FALSE)
