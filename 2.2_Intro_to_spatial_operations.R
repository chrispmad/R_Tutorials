# We can carry out spatial operations in R (e.g. buffering, cropping, overlaying
# points on polygons, etc.). We can also produce pretty good-looking maps of at least
# 3 types: static (ggplot2), static but interactive (ggiraph, or 'girafe'), and fully interactive (leaflet)

# Spatial operations (vector) in R with sf.

# Key packages:
library(bcmaps) # Fantastic for BC-specific spatial layers.
library(sf) # This package is great for working with 'vector' data (points, lines, polygons)
library(terra) # This package is great for working with raster (e.g. elevation pixels) and vector data.
library(ggplot2) # Nice for plotting spatial files
library(ggiraph) # Cool for making slightly interactive maps.
library(leaflet) # Great for interactive maps; can extend with {leaflet.extras} and {leaflet.extras2}
library(dplyr) # From the {tidyverse} - gives us tools to make new columns, combine tables etc.

# What does the package {bcmaps} include?
# Boundary of the province (note: it's actually 151 rows long, because of little islands)
bc = bcmaps::bc_bound()

plot(bc)

# Cities in BC
bcmaps::bc_cities()

# Natural resource districts and regions
bcmaps::nr_districts()
bcmaps::nr_regions()

# BEC zones - this one can take a while to load!
# bcmaps::bec()

# Plus ecoprovinces, census tracts, timber supply areas, watercourses, and more!

# Let's use the BC boundary, plus natural resource regions.
bc_bound = bcmaps::bc_bound()
regs = bcmaps::nr_regions()

# The most common spatial operations that I use:

# =======================================================
# Reading spatial data
# i. a shapefile
sp_dat = sf::read_sf('data/opaque_file.shp') # Select the .shp file

# ii. or a geopackage
foo = sf::read_sf('data/mysterious_spatial_file.gpkg')

# iii. or a KML / KMZ (if KMZ, use unzip first)
my_kml = sf::read_sf(unzip('data/mysterious_files.kmz'))

# OR Converting a table of data with coordinates into a spatial object.
my_dat = data.frame(lat = 52, lng = -122, observation = 'Great poutine!')

my_sf = sf::st_as_sf(
  my_dat,
  coords = c('lng','lat'), #replace these with your column names, if different; horizontal coordinate always first.
  crs = 4326 # If your coordinates are in BC Albers, use 'crs = 3005'; UTM zone 10N, use 'crs = 32610'
)

# Let's take a look! I like to use ggplot for this, as using base plot()
# will make a plot for every variable in the dataset.
# E.g.
plot(regs)

my_plot = ggplot() +
  geom_sf(data = bc_bound) +
  geom_sf(data = regs, aes(fill = REGION_NAME)) +
  geom_sf(data = sp_dat, fill = 'green', alpha = 0.5) +
  geom_sf(data = foo, col = 'blue') +
  geom_sf(data = my_sf, col = 'purple')

my_plot

# We can adjust the projection used in a ggplot with the 'coord_sf()' function.
my_plot +
  coord_sf(crs = 4326)

north_pole = st_as_sf(data.frame(lng = 0, lat = 90),
                      coords = c('lng','lat'),
                      crs = 4326)

library(rnaturalearth)
library(rnaturalearthdata)

russia = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(name == 'Russia') |>
  sf::st_make_valid()

argentina = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(name == 'Argentina')

my_plot +
  geom_sf(data = north_pole) +
  geom_sf(data = russia, fill = 'pink') +
  geom_sf(data = argentina, fill = 'blue') +
  coord_sf(crs = 3995)

st_area(russia)
st_area(argentina)

#Note: you can add labels/text with geom_sf_label/geom_sf_text.
# I find them a bit hard to place on the map... takes a lot of tinkering.
  #geom_sf_text(data = my_sf, aes(label = observation))

# =======================================================
# Reprojecting a vector from one CRS (coordinate reference system)
# to another, e.g. from lat/long to BC Albers.
my_sf_nad = my_sf |>
  st_transform(crs = 3005) # 4326 = EPSG:WG83(?)

# Check out the coordinate column now.
my_sf_nad

# Also make a NAD 83 version of BC border.
bc_nad = st_transform(bc_bound, 3005)

ggplot() +
  geom_sf(data = bc_nad) +
  geom_sf(data = my_sf_nad, col = 'blue')

# =======================================================
# Buffering. 'dist' is distance in meters.
my_sf_buffer = sf::st_buffer(my_sf, dist = 10000)

plot(my_sf_buffer)
# =======================================================
# Merging spatial objects.
# i. Merging 2+ polygons into one with 'summarise()'.
sw_regs = regs |>
  filter(REGION_NAME %in% c("West Coast Natural Resource Region",
                            "South Coast Natural Resource Region"))

plot(sw_regs$geometry)

summarise(sw_regs)

sw_regs = sw_regs |>
  summarise(id = 'southwest nr') #Optional: make a new column, like this 'id' one.

plot(sw_regs)
# One polygon now!
# Note that, since this is the dplyr function 'summarise', you can combine
# it with other dplyr functions like 'group_by', etc.

# ii. Combining two spatial tables into one larger spatial table; no merge.
row_to_add = data.frame(observation = 'MOOSE!', lng = -130.1, lat = 57) |>
  st_as_sf(coords = c("lng","lat"), crs = 4326)

all_sf = my_sf |>
  bind_rows(row_to_add)

ggplot() +
  geom_sf(data = bc_bound) +
  geom_sf(data = all_sf, aes(col = observation))

# iii. Converting points to lines, etc. This rarely comes up, to be honest.
road_trip = all_sf |>
  summarise() |>        # Produces a single-row table with 'MULTIPOINT' geometry
  st_cast("LINESTRING") # Take the multipoint (has 2 points) and make into a line.

ggplot() +
  geom_sf(data = bc_bound) +
  geom_sf(data = road_trip,
          linetype=2,  # linetype of 2 is a kind of dashed line.
          lwd = 1,     # This argument sets the width of the line.
          col = 'purple') +
  geom_sf(data = all_sf, aes(col = observation))

# =======================================================
# Doing a 'spatial join' to match points to polygons that
# 'contain' them.
regs_84 = regs |>
  # Note: a dplyr 'select()' both selects columns to keep (or discard if you prefix
  # with the '-' operator) AND it can rename columns; two birds, one stone!
  dplyr::select(region_name = REGION_NAME) |>
  st_transform(4326) # Reproject to lat/long (i.e. WGS 84)

foo_w_region = foo |>
  st_join(regs_84, st_intersects)

# Above:
# First argument: the spatial layer to match with.
# Second argument: the kind of spatial match to use; could be
# st_intersects, st_nearest_feature, etc...

foo_w_region |>
  # Drop the geometry column before the 'count()'
  st_drop_geometry() |>
  count(region_name)

# 6. To write spatial objects:
write_sf(foo_w_region, 'data/goldfish_with_nr_region.gpkg') # A geopackage; a single file!
write_sf(foo_w_region, 'data/goldfish_with_nr_region.shp') # A shapefile; 4+ files :(
# We could also write data out as a KML.

# EXTRA CONTENT - INTERACTIVE MAPS! These are great in markdown files or Shiny apps.
# =======================================================

# 1. Girafe.
library(ggiraph)

my_plot = ggplot() +
  geom_sf(data = bc_bound) +
  geom_sf_interactive(data = regs, aes(fill = REGION_NAME,
                                       tooltip = REGION_NAME,
                                       data_id = REGION_NAME)) +
  theme(legend.position = 'none')

girafe(ggobj = my_plot)
# Note that we can add (interactive) rasters!

# 2. Leaflet.
# Note: spatial files must be in lat/long (i.e. EPSG WGS 84)
# Note: lines of leaflet 'logic' are linked together with 'pipes' (i.e. '|>')

leaflet() |> # Initialize blank leaflet map.
  addTiles() |> # Add Google Street View
  addPolygons(
    label = ~region_name, # Remember to add tilde for variables (this guy: ~)
    data = regs_84
  )

# Defaults are that blue colour, and fairly high transparency.
# We can customize all that... let's see how many things we can add to
# this leaflet map ;)

my_colour_palette = colorFactor(palette = 'Spectral',
                                domain = regs_84$region_name)

leaflet() |> # Initialize blank leaflet map.
  addTiles(group = 'GSV') |> # Add Google Street View Basemap.
  addProviderTiles(provider = providers$CartoDB, group = 'Carto') |> # A 2nd basemap.
  addPolygons(
    color = 'black',
    weight = 2,
    fillColor = ~my_colour_palette(region_name),
    fillOpacity = 0.8,
    popup = 'You clicked me!',
    group = 'Resource Regions', #Setting a 'group' id so that we can toggle this layer on/off.
    label = ~region_name, # Remember to add that tilde for variables (this guy: ~)
    data = regs_84
  ) |>
  addLegend(pal = my_colour_palette,
            values = regs_84$region_name) |>
  addScaleBar('bottomright') |>
  leaflet::addLayersControl(baseGroups = c('Carto','GSV'), # Setting basemap options; only 1 active at a time.
                            overlayGroups = 'Resource Regions', # Can toggle any/all of these at a time.
                            options = layersControlOptions(collapsed = F),
                            position = 'bottomleft') |>
  leaflet.extras::addSearchFeatures(options = leaflet.extras::searchFeaturesOptions(moveToLocation = FALSE,
                                                                                    zoom = 5),
                                    targetGroups = 'Resource Regions')

# EXTRA CONTENT - rasters and elevations
# =======================================================
foo_elev = elevatr::get_elev_raster(foo,
                                    z = 8)

plot(foo_elev)
# What the heck! Ocean floor. So cool! But let's set to 0.
foo_elev[foo_elev <= 0] = 0
plot(foo_elev)

foo_elev_c = terra::crop(foo_elev, # Raster layer to be cropped.
                         st_buffer(foo,10000) #Cropping layer; a 10km buffer around our goldfish points 'foo'.
)
plot(foo_elev_c)

# To plot in ggplot, we have to convert the raster into a table...
foo_elev_t = as.data.frame(foo_elev_c, xy = TRUE) |>
  as_tibble() |>
  purrr::set_names(c("x","y","elev"))

# foo_elev_t = foo_elev_t |>
#   mutate(elev = ifelse(elev <= 2, NA, elev))

ggplot() +
  geom_raster(data = foo_elev_t,
              aes(x=x, y=y, fill = elev)) +
  geom_sf(data = foo, col = 'lightpink') +
  scale_fill_gradientn(colours = c("#2e9447","#075737","#cca952","#824d27","white"))
# scale_fill_gradientn(colours = terrain.colors(6))
