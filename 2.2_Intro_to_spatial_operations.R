# We can carry out spatial operations in R (e.g. buffering, cropping, overlaying
# points on polygons, etc.). We can also produce pretty good-looking maps of at least
# 3 types

# Spatial operations (vector) in R with sf.

# Downloading 'combined statistical areas' from {tigris}
cbsa = tigris::combined_statistical_areas(cb = TRUE)

# Pull out any that mention Minnesota ("MN")
cbsa_minn = cbsa %>% filter(str_detect(NAME, '.*, MN.*'))

ggplot() +
  geom_sf(data = minn) +
  geom_sf(data = cbsa_minn, aes(fill = NAME))
# The most common spatial operations that I use:

ggplot() +
  geom_sf(data = minn)

# 0. Convert a table with coordinates to a spatial object!
my_dat = data.frame(lat = 49,
           lng = -120,
           a = 'Hello!')

my_sf = my_dat %>%
  st_as_sf(
  coords = c('lng','lat'),
  crs = 4326
)

# 1. Buffering. 'dist' is distance in meters.
minn_b = sf::st_buffer(x = minn, dist = 10000)

ggplot() +
  geom_sf(data = minn_b)

# 2. Reprojecting a vector from one CRS (coordinate reference system)
# to another.
minn_crs_4326 = minn %>%
  st_transform(crs = 4326) # 4326 = EPSG:WG83(?)

# 3. Finding the bounding box of a vector.
minn_bb = st_bbox(minn) %>% as.data.frame()

# 3b. Turning a bbox into a rectangle.
minn_bb_poly = minn_bb %>% st_as_sfc()

ggplot() +
  geom_sf(data = minn) +
  geom_sf(data = minn_bb_poly, fill = 'transparent')

# 4. Doing a 'spatial join' to match points to polygons that
# 'contains' them.

st_join()

# 5. Trimming one polygon with another polygon. First object
# is the object to be trimmed, the second option is the object to
# clip with.

cbsa_trimmed = st_intersection(cbsa_minn, minn)

ggplot() +
  geom_sf(data = minn) +
  geom_sf(data = cbsa_trimmed, aes(fill = NAME))

# 6. To read and write spatial objects:
write_sf(cbsa_trimmed, 'data/cbsa_trimmed.gpkg')
read_sf('data/cbsa_trimmed.gpkg')
