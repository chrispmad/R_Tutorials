# This script uses an online data source for the latest info on bigfoot sightings,
# corrects a few errors in the data, then makes some fancy leaflet maps.

# Script Author: Chris Madsen
# Contact info:
# i. email = chris.madsen@gov.bc.ca
# ii. Github = chrispmad

# Script last modified: 2024-01-09

# # # # # # # # # # # # #
#       PART ONE        #
# # # # # # # # # # # # #

# =======================
#      INTRODUCTION
# =======================

# Leaflet is a Javascript library (https://leafletjs.com/) developed by Volodymyr Agafonkin,
# and 'ported' to R by the RStudio team. Thus, we can write R code that gets translated
# behind the scenes into Javascript code.

# Leaflet maps are great because they are easy to create in R, are interactive in
# HTML documents / Shiny apps, and look great.

# In addition to RStudio's {leaflet} package, there are many additional packages that
# extend the functionality by porting 'plugins' written by other folks in Javascript to R.
# Two main ones are {leaflet.extras} and {leaflet.extras2}

# Let's dive in!

library(tidyverse) # A bundle of useful data wrangling packages
library(sf) # common vector spatial object functions
library(leaflet) # base leaflet package
# library(leaflet.extras) # leaflet extensions
# library(leaflet.extras2) # leaflet extensions
library(leafpop) # leaflet extension for tables/figures as popups

# =======================
#  Intro to Leaflet maps
# =======================

# To initialize a leaflet map, we call "leaflet()". We then add
# basemaps (i.e. backgrounds) and layers. Note that the output
# of this function can either be visualized immediately, e.g.
leaflet() |>
  addTiles()

# Or saved to an object, e.g.
l = leaflet() |>
  addTiles()

l

# Toy dataset of earthquakes
quakes_sf = st_as_sf(quakes, coords = c("long","lat"), crs = 4326)

# If we want to add stuff to our leaflet map, we can do so in a single step or
# in multiple steps.
l |>
  addCircleMarkers(
    data = quakes_sf
  )

# There are many kinds of markers (circles, 'Awesome' Markers, custom markers, etc.)
# We can also add polygons and rasters and lines.
# Each of these has attributes like color, opacity, weight, etc.
# Let's try changing those.
l |>
  addCircleMarkers(
    weight = 1, # thickness of outline
    color = 'purple', # colour of outline
    opacity = 1, # Opacity of outline
    fillColor = 'darkblue', # colour of inside of shape
    fillOpacity = 0.95, # opacity of inside of shape
    label = ~mag, # Info that shows up on mouse hover
    popup = ~glue::glue('Stations: {stations}, Magnitude: {mag}'), # Shows up on click
    data = quakes_sf
  )

# We can also make attributes like colour or size dependent on variables in the dataset!
# It'd be nice to colour points based on their values, e.g. magnitude of earthquakes.

# To do this, we make a colour palette. Leaflet has functions to make palettes
# for numerical variables, character / factor variables, binning of
# numerical variables into groups, etc.

mag_colour_palette = colorNumeric(
  palette = 'Spectral', # Many options; consider viridis, magma, RdYlGn etc.
  domain = quakes_sf$mag,
  reverse = T
  )

l |>
  addCircleMarkers(
    data = quakes_sf,
    color = ~mag_colour_palette(mag)
  ) |>
  addLegend(pal = mag_colour_palette,
            values = quakes_sf$mag,
            title = "Magnitude")
# Note that we can flip the order of legend values + colours so that
# highest number is on top. We'll do this later in the tutorial...


# =======================
#  Data Case Study: Bigfoot
# =======================

# 1. Data gathering step.

# Bigfoot are elusive... has anyone reported any on iNaturalist?
library(rinat) # Package for querying iNaturalist from R.
bigfoot_inat = rinat::get_inat_obs(query = 'Sasquatch|Bigfoot',
                                   maxresults = 10000)
# One result!!

# Looks like we must dig deeper to find more records.
# Fortunately, Timothy Renner has pulled together historical sighting data from
# this reputable website (https://www.bfro.net/); check out his data here:
# https://data.world/timothyrenner/bfro-sightings-data

bigf = readr::read_csv(file = 'data/bfro_locations.csv')

# Combine the iNaturalist record(s) and Timothy's dataset.
bigf = dplyr::bind_rows(
  # A bit of renaming columns is necessary so that these tables have the same
  # column names and data types.
  bigfoot_inat |> dplyr::summarise(title = tag_list,
                                   number = as.numeric(stringr::str_extract(url,'[0-9]*$')),
                                   classification = 'Class A',
                                   timestamp = lubridate::as_datetime(bigfoot_inat$datetime),
                                   latitude,
                                   longitude),
  bigf
) |>
  as_tibble()

# 2. Data cleaning step

# Let's take a glance at some columns in the dataset.
ggplot(bigf) +
  geom_histogram(aes(timestamp))

ggplot(bigf) +
  geom_point(aes(x = longitude, y = latitude))
# Yikes - we have longitude coordinates in 2+ projections, but latitude is all good.

# Let's take the data that have longitude values outside the range of possible
# WGS 84 values.
oddball_data = bigf |>
  filter(longitude < -180)
# Just 10 such rows.

View(oddball_data)

# Here's some painful coordinate correction ...
# This could definitely be done in excel, but sometimes
# it's nice to have data cleaning available as code so that
# the original data file remains unmodified.
oddball_data = oddball_data |>
  dplyr::mutate(longitude = case_when(
    stringr::str_detect(title, 'Alaire') ~ -74.13133,
    stringr::str_detect(title, 'Humptulips') ~ -123.76296,
    stringr::str_detect(title, 'Monongahela') ~ -79.90177,
    stringr::str_detect(title, 'Atwater') ~ -81.179366,
    stringr::str_detect(title, 'Columbia and Duck River') ~ -87.003917,
    stringr::str_detect(title, 'Sugar Camp') ~ -89.31625,
    stringr::str_detect(title, 'Appleton') ~ -88.43552,
    stringr::str_detect(title, 'October Mountain') ~ -73.21531,
    stringr::str_detect(title, 'Salt Fork') ~ -81.51686,
    T ~ longitude
  )) |>
  dplyr::mutate(latitude = case_when(
    stringr::str_detect(title, 'Monongahela') ~ 38.5353,
    T ~ latitude
  )) |>
  filter(!stringr::str_detect(title, 'Great Depression'))

# Rejoin our cleaned 'oddball' data table with the rest of the data.
bigf = bigf |>
  dplyr::filter(longitude >= -180) |>
  dplyr::bind_rows(
    oddball_data
  )
# Note the use of bind_rows here; a benefit of this function vs. "rbind()"
# is that if a column is only found in one of the tables, it can still perform
# this binding; rbind will error. This is nice if you know you have a couple
# columns in only "Table 1" and you aren't concerned about having NA values for
# "Table 2" - you just want them joined!


# 3. Spatialize data and get more spatial data

bigf_sf = bigf |>
  st_as_sf(coords = c("longitude","latitude"),
           crs = 4326) # This means the data we're feeding in to st_as_sf is WGS 84 lat/long.

# Let's grab polygons for Canada and the USA.
library(rnaturalearth)

# Let's get polygons for Canada, the USA and Mexico.
NA_countries = rnaturalearth::countries110 |>
  # Convert a non-sf object (it's an older sp format initially, I think)
  st_as_sf() |>
  # Just pull out a couple columns; the geometry column is implicitly included.
  dplyr::select(NAME_EN, NAME_KO, NAME_HI, NAME_ZHT) |>
  # Filter rows for just the following countries.
  dplyr::filter(NAME_EN %in% c("Canada","United States of America","Mexico"))

# As an aside, I almost always simplify the geometry of big polygons
# before plotting them (in ggplot or leaflet or whatever) - if you have
# very precise work, this may not be advisable, but for visualization purposes,
# the benefits of using smaller file sizes are appreciable!
NA_countries_s = rmapshaper::ms_simplify(NA_countries,
                                         keep = 0.5) #Proportion of points to keep
# In this case, the original polygon set is already so simple,
# there's no need to simplify it.

# Combine Canada, the US, and Mexico into one single polygon using
# dplyr's summarise function.
NA_monolith = NA_countries |>
  dplyr::summarise()

ggplot() +
  geom_sf(data = NA_countries, fill = 'antiquewhite') +
  geom_sf(data = bigf_sf)

# Let's drop any records that are in the ocean.
bigf_sf = bigf_sf |>
  dplyr::filter(sf::st_intersects(geometry, NA_monolith, sparse = F))

ggplot() +
  geom_sf(data = NA_countries, fill = 'antiquewhite') +
  geom_sf(data = bigf_sf)

# Let's snag provinces, territories and states to classify these observations.
statprov = rnaturalearth::ne_states(country = c("Canada","United States of America")) |>
  st_as_sf() |>
  dplyr::select(state_prov = name,
                country = admin)

bigf_sf = bigf_sf |>
  st_join(statprov)

ggplot() +
  geom_sf(data = NA_countries, fill = 'antiquewhite') +
  geom_sf(data = bigf_sf, aes(col = state_prov,
                              shape = country)) +
  labs(title = 'We ran out of colours!') +
  theme(legend.position = 'none') # Too many colours - the legend is huge!

# =======================
#   GRIDS AND HEATMAPS
# =======================
# Let's make a simple heatmap - many ways to do this.

# I'm going to make a grid of polygons for North American sf polygon
NA_grid_geometry = sf::st_make_grid(NA_monolith, cellsize = c(1,1))

NA_grid = tibble(cell_id = 1:length(NA_grid_geometry)) |>
  st_set_geometry(NA_grid_geometry)

rm(NA_grid_geometry)

# Drop cells in the ocean.
NA_grid_masked = NA_grid |>
  dplyr::filter(
    sf::st_intersects(
      sf::st_centroid(geometry),
      NA_monolith, sparse = F)
    )

ggplot() +
  geom_sf(data = NA_monolith) +
  geom_sf(data = NA_grid_masked) +
  labs(title = 'This looks pretty cool')

# Get count of sightings in each cell.
sightings_by_grid_cell = NA_grid |> st_join(
  bigf_sf |> select(number,title)
) |>
  filter(!is.na(title)) |>
  sf::st_drop_geometry() |>
  count(cell_id, name = 'number_sightings')

NA_grid_masked = NA_grid_masked |>
  left_join(sightings_by_grid_cell)

ggplot() +
  geom_sf(data = NA_monolith) +
  geom_sf(data = NA_grid_masked,
          aes(fill = number_sightings,
              alpha = 0.85),
          col = 'transparent') +
  labs(title = 'This looks even cooler') +
  scale_fill_gradient2(midpoint = 50,
                       low = 'darkgreen',
                       mid = 'gold',
                       high = 'red') +
  scale_alpha_continuous(guide = 'none') # This is another way to drop a legend item.

# We can also interpolate a *real* heatmap using a raster.
library(gstat)
library(terra)

# Make a raster that spans the bounding box of NA_monolith object, with pixel resolution
# of 1 degree.
r <- terra::rast(NA_monolith, res = 1)

# Fill it with dummy vars, just so we can plot it and check it out.
r$dummy_var = sample(c(1:3), size = terra::ncell(r), replace = T)
# 8211 cells

# Mask to our NA monolith polygon.
r <- terra::mask(r, vect(NA_monolith))

plot(r)

# Turn our vector data into a raster using the 'r' raster as a template.
bigf_r = terra::rasterize(x = bigf_sf, y = r, fun = 'count', touches = TRUE)

plot(bigf_r)

# Find centroids of raster cells, use these in interpolation functions below.
bigf_v = terra::centroids(terra::as.polygons(bigf_r))

interp_1 = terra::interpNear(r, bigf_v, radius = 5, field = 'count')
plot(interp_1)

interp_2 = terra::interpIDW(r, bigf_v, radius = 5, field = 'count')
plot(interp_2)

# 4. Back to Leaflet (phew!)

# Let's make a raster colour palette.
bigf_r_colpal = colorNumeric(palette = 'Spectral',
                             domain = terra::values(interp_2),
                             na.color = 'transparent',
                             reverse = T)

bigf_r_colpal_legend = colorNumeric(palette = 'Spectral',
                                    domain = terra::values(interp_2),
                                    na.color = 'transparent',
                                    reverse = F)

leaflet() |>
  addTiles() |>
  addRasterImage(colors = bigf_r_colpal,
                 x = interp_2) |>
  addLegend(pal = bigf_r_colpal_legend,
            values = terra::values(interp_2),
            # Here's how we can reorder legend so that high values are on top.
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

# Make popup table for each point. These tables are in HTML - hard to read, but
# great for visualizations / Shiny apps etc.
bigf_popup_tables = leafpop::popupTable(bigf_sf |>
                      sf::st_drop_geometry() |>
                      dplyr::select(classification,
                                    timestamp,
                                    state_prov,
                                    title,
                                    country))


# Let's set up a leaflet map with multiple basemaps, layers control etc.
# ============================
#  Initialize leaflet map
l = leaflet() |>
  # ============================
  #  Add background base layers
  addTiles(group = 'OSM') |>
  addProviderTiles(provider = providers$OpenTopoMap, group = 'TopoMap') |>
  addProviderTiles(provider = providers$CartoDB, group = 'cartoDB') |>
  # ============================
  # Set view (lat, long, and zoom)
  setView(lng = -105, lat = 45, zoom = 3) |>
  # ============================
  #  Add map details and controls
  addScaleBar('bottomright') |>
  addLayersControl(
    position = 'bottomright',
    baseGroups = c("OSM","cartoDB","TopoMap"),
    overlayGroups = c("circles","heatmap","IDW_heatmap"),
    options = layersControlOptions(collapsed = F)
  )

l

# Let's add our bigfoot data to the map.
l |>
  addCircleMarkers(data = bigf_sf,
                   label = ~title,
                   popup = lapply(bigf_popup_tables,
                                  shiny::HTML),
                   group = 'circles')
# Too crowded! How about with smaller circles?

l |>
  addCircleMarkers(data = bigf_sf,
                   label = ~title,
                   radius = 1,
                   weight = 0.5,
                   popup = lapply(bigf_popup_tables,
                                  shiny::HTML),
                   group = 'circles')

# We could also try clustering the markers
l |>
  addCircleMarkers(data = bigf_sf,
                   label = ~title,
                   popup = lapply(bigf_popup_tables,
                                  shiny::HTML),
                   group = 'circles',
                   clusterOptions = markerClusterOptions(maxClusterRadius = 25))
