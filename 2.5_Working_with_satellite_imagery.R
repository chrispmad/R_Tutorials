# This tutorial is largely based on the tutorial of Mateusz Rydzik found here on r-bloggers:
# https://www.r-bloggers.com/2024/01/an-overview-of-the-rsi-r-package-for-retrieving-satellite-imagery-and-calculating-spectral-indices/

# =======================
#  PACKAGES
# =======================

library(sf) # For working with vector spatial data
library(terra) # For working with rasters, mostly
library(leaflet) # For interactive 2-D maps
library(lubridate) # For working with dates
library(rayshader) # For 3-D plots and movies
library(rsi) # Built on top of the {rstac} package; simplifies workflow
library(rstac) # Provides access to various STAC (spatial-temporal asset catalogue) APIs

# =======================
#  PREP WORK
# =======================

# Get the 'Awesome Spectral Indices' reference dataset.
# This tibble has many options of types of satellite imagery
asi = spectral_indices(download_indices = T)

# Make a spatial point for Victoria; buffer it by 15km.
vic = st_as_sf(data.frame(lat = 48.525,
                          lng = -123.42),
               coords = c('lng','lat'),
               crs = 4326) |>
  sf::st_transform(crs = 3005) |>
  sf::st_buffer(dist = 15000)

# Create a 'bounding box' rectangle around our buffered Victoria point.
vic_bbox = vic |>
  # Reproject our 'vic' circle to leaflet-friendly EPSG:WGS84
  sf::st_transform(crs = 4326) |>
  # Find the bounding box around the circle.
  sf::st_bbox() |>
  # Convert the bounding box object to a normal sf shape.
  sf::st_as_sfc()

# Check out our bounding box around Victoria with a Leaflet map.
leaflet() |>
  addTiles() |>
  addPolygons(
    data = vic_bbox
  )

# =======================
#  DOWNLOADING SATELLITE IMAGES WITH {rsi}
#  i.e. a relatively simple way to download satellite images.
# =======================

# 1. LANDSAT

# We can get landsat imagery. All available satellite images for the specified
# area and timeframe are downloaded and merged using a 'composite function'
# (default function is Median)
if(file.exists('data/vic_landsat_r.tif')){
  vic_landsat_r = terra::rast('data/vic_landsat_r.tif')
} else {
vic_landsat = get_landsat_imagery(
  vic,
  start_date = "2023-05-01",
  end_date = "2023-5-10",
  output_filename = tempfile(fileext = ".tif")
)
vic_landsat_r = terra::rast(vic_landsat)
terra::writeRaster(vic_landsat_r, 'data/vic_landsat_r.tif')
}

terra::plotRGB(vic_landsat_r, r = 4, g = 3, b = 2, stretch = "lin")

# SENTINEL2

# Sentinel has more bands than Landsat, so it is more detailed but also
# tends to take a LONG time to download :O
# This is especially the case if you have a broad time interval between
# start and end date.
if(file.exists('data/vic_sentinel2_r.tif')){
  vic_sentinel2_r = terra::rast('data/vic_sentinel2_r.tif')
} else {
  vic_sentinel2 = get_sentinel2_imagery(
    vic,
    start_date = "2023-10-26",
    end_date = "2023-10-31",
    output_filename = tempfile(fileext = ".tif")
  )
vic_sentinel2_r = terra::rast(vic_sentinel2)
terra::writeRaster(vic_sentinel2_r, 'data/vic_sentinel2_r.tif')
}

terra::plotRGB(vic_sentinel2_r, r = 4, g = 3, b = 2, stretch = "lin")

# We can also choose to NOT take the median of all available
# images by specifying 'composite_function = NULL'. This will
# allow us to download ALL available satellite images for
# our area in the chosen timespan. This results in a pretty large download!

# =======================
#   SELECTING DATES + CUSTOM QUERIES IN {rstac} PACKAGE
#   More complicated, multistep process.
# =======================

### Selecting Dates

# We can see HOW MANY images are available in a given time period, before downloading them.
# This is probably a good idea if we want to test a timeframe to then choose
# an early and a late image for contrasting. To do this, we can use the
# r package {rstac}

# I'm going to switch our focus to just Langford (to reduce the waiting time)
# a bit when downloading these satellite images.

lang = st_as_sf(
  data.frame(lat = 48.45,
             lng = -123.5047),
  coords = c("lng","lat"),
  crs = 4326) |>
  st_buffer(dist = 4500) |>
  st_bbox() |>
  st_as_sfc()

leaflet() |> addTiles() |> addPolygons(data = lang)

# We start off the query by identifying a STAC source.
planetarycomp_source = attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "stac_source")

results = rstac::stac(planetarycomp_source) |>  # The stac source
  # Here is where we input our search parameters.
  rstac::stac_search(
    # The span of dates to search within
    datetime = '2018-09-01/2024-03-01',
    # Identify the collection; in this case, it's "sentinel-2-l2a".
    collections = attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "collection_name"),
    # Provide a bounding box to narrow search.
    bbox = sf::st_bbox(lang)) |>
  rstac::get_request()

results$features |>
  length()
# 250 Sentinel2 satellite images for Langford in this time frame!

results |>
  rstac::items_assets()
# Here are the different kinds of satellite images included in this collection.

### Custom Queries

# Even better, we can design our own search query to only return
# images if they meet certain criteria, e.g., less than 10% cloud cover.

s_obj = rstac::stac(attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "stac_source"))
collection_name = attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "collection_name")
# Note: this tile name needs the results of our previous stac_search() function call; this tells us
# which tile to search for.
tile_name = stringr::str_extract(results$features[[1]]$id, '(?<=R[0-9]{3}_T).{5}(?=_)')

results_q = s_obj |> # The stac source
  rstac::ext_filter(
    # These are our filtering conditions
    collection == {{collection_name}} &&
    `eo:cloud_cover` <= 10  &&
    `s2:mgrs_tile` == {{tile_name}}  &&
    anyinteracts(datetime, interval("2018-09-01", "2024-03-01"))
    ) |>
  rstac::post_request()

# Check out how many features were returned.
results_q$features |>
  length()
#137 satellite images between Sept 1, 2018 and Oct 31, 2023, for Victoria.

# What's the oldest image, and newest image, in this collection?
# We can use these dates to intelligently inform {rsi} downloads
# of satellite imagery.


# We'll use the oldest date and most recent date like this:
# past --X-----X-X-------X-------X--X---------X-----X--------X--> present
#      [  (oldest_date)  ] ------------------ [  (recent_date)  ]

oldest_date = results_q$features[[length(results_q$features)]]$id

oldest_date = stringr::str_remove(oldest_date, paste0('.*',tile_name,'_')) |>
  stringr::str_remove('T.*') |>
  lubridate::ymd()

oldest_date

recent_date = results_q$features[[1]]$id

recent_date = stringr::str_remove(recent_date, paste0('.*',tile_name,'_')) |>
  stringr::str_remove('T.*') |>
  lubridate::ymd()

recent_date

# We could plug these dates in to download sentinel2 Satellite imagery
# using the {rsi} 'get_sentinel2_imagery()' function.

# =======================
#  ELEVATION
# =======================

# We can get a DEM of the area.
# Many options for this, we can use {rsi} to do it, or {elevatr} package

vic_dem = rsi::get_dem(vic)

vic_dem = terra::rast(vic_dem)

plot(vic_dem)

# Let's take a look at the landsat and the sentinel2 images
par(mfrow = c(1, 3))
terra::plotRGB(vic_landsat_r, r = 4, g = 3, b = 2, stretch = "lin")
terra::plotRGB(vic_sentinel2_r, r = 4, g = 3, b = 2, stretch = "lin")
terra::plot(vic_dem)

# =======================
#  Custom query for {rsi} package
# =======================

# We can make a custom CQL query to narrow down
# the kind of satellite images that we download,
# e.g. if we only wish to retain landsat or sentinel-2 images with cloud cover
# of 25% or less, we could use the following query.
landsat_25cc_qf <- function(bbox, start_date, end_date, limit,
                              asset_names = rsi::landsat_band_mapping$planetary_computer_v1,
                              ...) {
  # This will be the spatial object or bounding box that we feed in.
  geom <- rstac::cql2_bbox_as_geojson(bbox)
  # Our start and end dates are converted to an interval.
  datetime <- rstac::cql2_interval(start_date, end_date)
  stac_source = attr(asset_names, "stac_source")
  collection = attr(asset_names, "collection_name")
  sign_function = attr(asset_names, "sign_function")
  # Here we design the query itself
  request <- rstac::ext_filter(
    rstac::stac(stac_source),
    # These are the CQL filter statements
    collection == {{collection}} &&
      t_intersects(datetime, {{datetime}}) &&
      s_intersects(geom, {{geom}}) &&
      `eo:cloud_cover` < 25 # We could alter this
  )
  rstac::items_fetch(rstac::post_request(request))
}

par(mfrow = c(1, 1))

# Download Landsat satellite imagery for Langford area for the
# oldest dates available (October 2020)
if(file.exists('data/old_lang_ls_r.tif')){
  old_lang_ls_r = terra::rast('data/old_lang_ls_r.tif')
} else {
  old_lang_ls = get_landsat_imagery(
    lang |> st_transform(crs = 3005),
    # Buffered oldest date to include up to 60 days closer to present day
    # so as to catch a good number of satellite images.
    start_date = as.character(oldest_date - 1),
    end_date = as.character(oldest_date + 60),
    output_filename = tempfile(fileext = ".tif"),
    query_function = landsat_25cc_qf
  )
  old_lang_ls_r = terra::rast(old_lang_ls)
  terra::writeRaster(old_lang_ls_r, 'data/old_lang_ls_r.tif', overwrite = T)
}

terra::plotRGB(old_lang_ls_r, r = 4, g = 3, b = 2, stretch = "lin")

old_langford_ndvi = calculate_indices(
  old_lang_ls_r,
  asi[asi$short_name == 'NDVI',],
  output_filename = tempfile(fileext = '.tif')
)

olndvi = terra::rast(old_langford_ndvi)

plot(olndvi)

# Download Landsat satellite imagery for Langford area for the
# most recent dates available (October 2023)
if(file.exists('data/recent_lang_ls_r.tif')){
  recent_lang_ls_r = terra::rast('data/recent_lang_ls_r.tif')
} else {
  recent_lang_ls = get_landsat_imagery(
    lang |> st_transform(crs = 3005),
    # Buffered recent date 100 days later so as to catch a good
    # number of satellite images.
    start_date = as.character(recent_date - 60),
    end_date = as.character(recent_date + 1),
    output_filename = tempfile(fileext = ".tif"),
    query_function = landsat_25cc_qf
  )
  recent_lang_ls_r = terra::rast(recent_lang_ls)
  terra::writeRaster(recent_lang_ls_r, 'data/recent_lang_ls_r.tif', overwrite = T)
}

terra::plotRGB(recent_lang_ls_r, r = 4, g = 3, b = 2, stretch = "lin")

recent_langford_ndvi = calculate_indices(
  recent_lang_ls_r,
  asi[asi$short_name == 'NDVI',],
  output_filename = tempfile(fileext = '.tif')
)

rlndvi = terra::rast(recent_langford_ndvi)

# Let's check them out
par(mfrow = c(1, 2))
terra::plot(olndvi, main = paste0(oldest_date,' to ',oldest_date+60), range = c(-1, 1))
terra::plot(rlndvi, main = paste0(recent_date-60,' to ', recent_date), range = c(-1, 1))

# Let's calculate the difference between the early and the late NDVI images.
dif = rlndvi - olndvi
par(mfrow = c(1,1))
terra::plot(dif)
hist(dif, main = "", xlab = "NDVI")

#What's the average change in NDVI?
# Not sure if this is statistically recommended...?
mean(terra::values(dif),na.rm=T)

# Let's make a ggplot from the raster, it would look nicer!
dif_df = terra::as.data.frame(dif, xy = T)

library(ggplot2)

ggplot(dif_df) +
  geom_raster(aes(x,y, fill = NDVI)) +
  scale_fill_gradient2(high = 'darkgreen') +
  theme(panel.background = element_rect(fill = 'white'))

# =======================
#  3D PLOTS WITH VISUAL OVERLAY
# =======================

# Can we use the DEM to make a rayshader plot, and then
# throw our NDVI plot on to colour the plot? Yes!

# 1. Victoria

#create bing png to overlay
png("output/overlay_vic_elev.png")

terra::plotRGB(vic_sentinel2_r, r = 4, g = 3, b = 2, stretch = "lin")

dev.off()
magick::image_read('output/overlay_vic_elev.png')
my_overlay = png::readPNG('output/overlay_vic_elev.png')

# Reduce raster complexity
vic_dem_r_s = terra::aggregate(vic_dem, fact = 2)

vic_el = rayshader::raster_to_matrix(vic_dem_r_s)

# Here's a 2D version of the map.
vic_el |>
  sphere_shade(texture = "desert") |>
  add_water(detect_water(vic_el), color = "desert") |>
  add_shadow(ray_shade(vic_el), 0.5) |>
  plot_map()

# And here's a 3D version, with the PNG added as an overlay!
vic_el |>
  sphere_shade(texture = "desert") |>
  add_overlay(my_overlay) |>
  add_water(detect_water(vic_el), color = "desert") |>
  add_shadow(ray_shade(vic_el, zscale = 3), 0.5) |>
  add_shadow(ambient_shade(vic_el), 0) |>
  plot_3d(vic_el, zscale = 10, fov = 0, theta = 30, zoom = 0.75, phi = 35, windowsize = c(1000, 800))

# If you'd like, you can make a little GIF video.

# filename_movie = tempfile()

# render_movie(filename = filename_movie, type = "oscillate",
#              frames = 60, phi = 35, zoom = 0.8, theta = 30,
#              title_text = "Victoria, BC")

# 2. Langford

lang_el = terra::crop(vic_dem, sf::st_transform(lang,3005))

plot(lang_el)

png("output/overlay_langford.png", width = 7, height = 6,
    units = 'in', res = 100)
ggplot(dif_df) +
  geom_raster(aes(x,y, fill = NDVI)) +
  scale_fill_gradient2(high = 'darkgreen') +
  ggthemes::theme_map() +
  theme(legend.position = 'none')
dev.off()

langford_overlay = png::readPNG('output/overlay_langford.png')

magick::image_read('output/overlay_langford.png')

lang_el = rayshader::raster_to_matrix(lang_el)

lang_el |>
  sphere_shade(texture = "imhof1") |>
  add_overlay(langford_overlay) |>
  add_water(detect_water(lang_el), color = "desert") |>
  add_shadow(ray_shade(lang_el, zscale = 3), 0.5) |>
  add_shadow(ambient_shade(lang_el), 0) |>
  plot_3d(lang_el, zscale = 10,
          theta = 30, zoom = 0.75, phi = 35,
          # fov = 67,
          windowsize = c(1000, 800))

render_snapshot(filename = 'output/render_test.png')
