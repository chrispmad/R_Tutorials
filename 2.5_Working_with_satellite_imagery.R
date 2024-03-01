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
i_want_to_wait = FALSE

if(i_want_to_wait){
  if(file.exists('data/vic_sentinel2_sep_1_r.tif')){
    vic_early = terra::rast('data/vic_sentinel2_sep_12_r.tif')
    vic_late = terra::rast('data/vic_sentinel2_sep_1_r.tif')
  } else {
    vic_sentinel2_sep = get_sentinel2_imagery(
      vic,
      start_date = "2023-09-01",
      end_date = "2023-10-31",
      output_filename = tempfile(fileext = ".tif"),
      composite_function = NULL
    )
    for(i in 1:length(vic_sentinel2_sep)){
      r = terra::rast(vic_sentinel2_sep[i])
      terra::writeRaster(r, paste0('data/vic_sentinel2_sep_',i,'_r.tif'))
    }
    vic_early = terra::rast(vic_sentinel2_sep[12])
    vic_late = terra::rast(vic_sentinel2_sep[1])
  }

  vic_sentinel2_sep_r
}

# =======================
#   SELECTING DATES + CUSTOM QUERIES IN {rstac} PACKAGE
# =======================

### Selecting Dates

# We can see HOW MANY images are available in a given time period, before downloading them.
# This is probably a good idea if we want to test a timeframe to then choose
# an early and a late image for contrasting.

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

results = rstac::stac(attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "stac_source")) %>% # The stac source
  rstac::stac_search(
    datetime = '2018-09-01/2023-10-31',
    collections = attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "collection_name"),
    bbox = sf::st_bbox(lang)) %>%
  rstac::get_request()

results$features |>
  length()
# 250 satellite images in this time-span!

### Custom Queries

# Even better, we can design our own search query to only return
# images if they have less than, e.g., 10% cloud cover.

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
    anyinteracts(datetime, interval("2018-09-01", "2023-10-31"))
    ) |>
  rstac::post_request()

# Check out how many features were returned.
results_q$features |>
  length()
#136 satellite images between Sept 1, 2018 and Oct 31, 2023, for Victoria.

# What's the oldest image, and newest image, in this collection?
# We can use these dates to intelligently inform {rsi} downloads
# of satellite imagery.
oldest_date = results_q$features[[length(results_q$features)]]$id
recent_date = results_q$features[[1]]$id

# We'll use the oldest date and most recent date like this:
# past -----------------------------------------------> present
# X X X [oldest_date ==X====X====X==X=X==> recent_date] X X

oldest_date = stringr::str_remove(oldest_date, paste0('.*',tile_name,'_')) |>
  stringr::str_remove('T.*') |>
  lubridate::ymd()

recent_date = stringr::str_remove(recent_date, paste0('.*',tile_name,'_')) |>
  stringr::str_remove('T.*') |>
  lubridate::ymd()

par(mfrow = c(1, 1))

if(file.exists('data/late_lang_ls_r.tif')){
  late_lang_ls_r = terra::rast('data/late_lang_ls_r.tif')
} else {
  late_lang_ls = get_landsat_imagery(
    lang |> st_transform(crs = 3005),
    # Add a buffer of 15 days to start date so that we can snag a couple images
    # to average over; otherwise, we might end up with a partial image.
    start_date = as.character(oldest_date - 15),
    end_date = as.character(oldest_date + 1),
    output_filename = tempfile(fileext = ".tif")
  )
  late_lang_ls_r = terra::rast(late_lang_ls)
  terra::writeRaster(late_lang_ls_r, 'data/late_lang_ls_r.tif')
}

terra::plotRGB(late_lang_ls_r, r = 4, g = 3, b = 2, stretch = "lin")

late_langford_ndvi = calculate_indices(
  late_lang_ls_r,
  asi[asi$short_name == 'NDVI',],
  output_filename = tempfile(fileext = '.tif')
)

llndvi = terra::rast(late_langford_ndvi)
plot(llndvi)

# if(file.exists('data/late_lang_sent2_r.tif')){
#   late_lang_sent2_r = terra::rast('data/late_lang_sent2_r.tif')
# } else {
#   late_lang_sent2 = get_sentinel2_imagery(
#     lang,
#     # Add a buffer of 50 days to start date so that we can snag a couple images
#     # to average over; otherwise, we might end up with a partial image.
#     start_date = as.character(oldest_date - 100),
#     end_date = as.character(oldest_date + 1),
#     output_filename = tempfile(fileext = ".tif")
#   )
#   late_lang_sent2_r = terra::rast(late_lang_sent2)
#   terra::writeRaster(late_lang_sent2_r, 'data/late_lang_sent2_r.tif')
# }

if(file.exists('data/early_lang_ls_r.tif')){
  early_lang_ls_r = terra::rast('data/early_lang_ls_r.tif')
} else {
  early_lang_ls = get_landsat_imagery(
    lang |> st_transform(crs = 3005),
    # Add a buffer of 15 days to start date so that we can snag a couple images
    # to average over; otherwise, we might end up with a partial image.
    start_date = as.character(recent_date - 1),
    end_date = as.character(recent_date + 45),
    output_filename = tempfile(fileext = ".tif")
  )
  early_lang_ls_r = terra::rast(early_lang_ls)
  terra::writeRaster(early_lang_ls_r, 'data/early_lang_ls_r.tif')
}

terra::plotRGB(early_lang_ls_r, r = 4, g = 3, b = 2, stretch = "lin")

early_langford_ndvi = calculate_indices(
  early_lang_ls_r,
  asi[asi$short_name == 'NDVI',],
  output_filename = tempfile(fileext = '.tif')
)

elndvi = terra::rast(early_langford_ndvi)
plot(elndvi)

# if(file.exists('data/early_vic_r.tif')){
#   early_vic_r = terra::rast('data/early_vic_r.tif')
# } else {
#   early_vic = get_sentinel2_imagery(
#     vic,
#     start_date = as.character(recent_date - 1),
#     end_date = as.character(recent_date + 100),
#     output_filename = tempfile(fileext = ".tif")
#   )
#   early_vic_r = terra::rast(early_vic)
#   terra::writeRaster(early_vic_r, 'data/early_vic_r.tif')
# }

par(mfrow = c(1, 2))
terra::plotRGB(late_lang_ls_r, r = 4, g = 3, b = 2, stretch = "lin")
terra::plotRGB(early_lang_ls_r, r = 4, g = 3, b = 2, stretch = "lin")

# =======================
#  ELEVATION
# =======================

# And finally, we can get a DEM of the area.
# Many options for this, but we can use {rsi} to
# do it.
vic_dem = get_dem(
  vic,
  output_filename = tempfile(fileext = ".tif"),
)
# Note: we could also use the {elevatr} package to get elevation data!

vic_dem_r = terra::rast(vic_dem)

# Let's take a look at the landsat and the sentinel2 images
par(mfrow = c(1, 3))
terra::plotRGB(vic_landsat_r, r = 4, g = 3, b = 2, stretch = "lin")
terra::plotRGB(vic_sentinel2_r, r = 4, g = 3, b = 2, stretch = "lin")
terra::plot(vic_dem_r)

# =======================
#  Custom query for {rsi} package
# =======================

# We can make a custom CQL query to narrow down
# the kind of satellite images that we download,
# e.g. if we only wish to retain sentinel-2 images with cloud cover
# of 25% or less, we could use the following query.
sentinel2_25cc_qf <- function(bbox, start_date, end_date, limit,
                              asset_names = rsi::sentinel2_band_mapping$planetary_computer_v1,
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
    collection == "sentinel-2-l2a" &&
      t_intersects(datetime, {{datetime}}) &&
      s_intersects(geom, {{geom}}) &&
      platform == "Sentinel-2B" &&
      `eo:cloud_cover` < 25 # We could alter this
  )
  rstac::items_fetch(rstac::post_request(request))
}

# We could use the above customized query function like so:
if(file.exists('data/vic_sentinel2_25cc.tif')){

  vic_sentinel2_25cc_r = terra::rast('data/vic_sentinel2_25cc.tif')

  } else {
  vic_sentinel2_25cc = get_sentinel2_imagery(
    vic,
    start_date = "2023-09-01",
    end_date = "2023-10-31",
    output_filename = tempfile(fileext = ".tif"),
    query_function = sentinel2_25cc_qf
  )

  vic_sentinel2_25cc_r = terra::rast(vic_sentinel2_25cc)

  terra::writeRaster(vic_sentinel2_25cc_r, 'data/vic_sentinel2_25cc.tif')
}

# We can select one or more indices from that asi table that we created
# at the top of the script.
# e.g. NDVI
ndvi = asi[asi$short_name == 'NDVI',]

# Now we can calculate the index / indices on our satellite images.
vic_early_ndvi = calculate_indices(
  early_vic_r,
  ndvi,
  output_filename = tempfile(fileext = '.tif')
)

vic_late_ndvi = calculate_indices(
  late_vic_r,
  ndvi,
  output_filename = tempfile(fileext = '.tif')
)

# Let's check them out
par(mfrow = c(1, 2))
vic_early_ndvi_r = terra::rast(vic_early_ndvi)
vic_late_ndvi_r = terra::rast(vic_late_ndvi)
terra::plot(vic_early_ndvi_r, range = c(-1, 1))
terra::plot(vic_late_ndvi_r, range = c(-1, 1))

# Let's calculate the difference between the early and the late NDVI images.
par(mfrow = c(1, 2))
dif = vic_late_ndvi_r - vic_early_ndvi_r
terra::plot(dif)
hist(dif, main = "", xlab = "NDVI")

# Let's save the early and late victoria images to a single raster 'stack'
stack = stack_rasters(
  c(
    vic_early_ndvi,
    vic_late_ndvi
  ),
  tempfile(fileext = ".vrt")
)

stack_rast = terra::rast(stack)
names(stack_rast) = c("NDVI_vic_early", "NDVI_vic_late")
stack_rast

# Can we use the DEM to make a rayshader plot, and then
# throw our NDVI plot on to colour the plot?

#create bing png to overlay
png("overlay_vic_elev.png")

terra::plotRGB(vic_sentinel2_r, r = 4, g = 3, b = 2, stretch = "lin")

dev.off()
magick::image_read('overlay_vic_elev.png')
my_overlay = png::readPNG('overlay_vic_elev.png')

# Reduce raster complexity
vic_dem_r_s = terra::aggregate(vic_dem_r, fact = 2)

vic_el = rayshader::raster_to_matrix(vic_dem_r_s)

vic_el |>
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(vic_el), color = "desert") %>%
  add_shadow(ray_shade(vic_el), 0.5) %>%
  plot_map()

vic_el %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(my_overlay) |>
  add_water(detect_water(vic_el), color = "desert") %>%
  add_shadow(ray_shade(vic_el, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(vic_el), 0) %>%
  plot_3d(vic_el, zscale = 10, fov = 0, theta = 30, zoom = 0.75, phi = 35, windowsize = c(1000, 800))

# filename_movie = tempfile()

# render_movie(filename = filename_movie, type = "oscillate",
#              frames = 60, phi = 35, zoom = 0.8, theta = 30,
#              title_text = "Victoria, BC")

# =======================
#  BOGOTA, COLOMBIA
# =======================

# Let's do this all again for Bogota!

bog = st_as_sf(
  data.frame(lat = c(4.83920, 4.470315),
             lng = c(-74.25, -73.95)),
  coords = c('lng','lat'),
  crs = 4326
) |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(crs = 21817)

leaflet() |>
  addTiles() |>
  addPolygons(data = bog |>
                st_transform(4326))

bog_el_filepath = rsi::get_dem(
  bog,
  output_filename = tempfile(fileext = ".tif")
  )

# How about a version where we zoom way out from Bogota?
bog_z = bog |>
  st_buffer(dist = 300000) |>
  st_transform(4326) |>
  st_bbox() |>
  st_as_sfc()

leaflet() |>
  addTiles() |>
  addPolygons(data = bog_z |>
                st_transform(4326))

bog_z_bbox = st_bbox(
  bog_z |>
    st_transform(4326)
)

bog_zoomedout = terra::rast(
  elevatr::get_elev_raster(
  locations = data.frame(x = c(bog_z_bbox$xmin,bog_z_bbox$xmax),
                         y = c(bog_z_bbox$ymin,bog_z_bbox$ymax)),
  prj = st_crs(bog |> st_transform(4326)),
  z = 8)
)

bog_zoomedout = terra::mask(bog_zoomedout, vect(st_as_sfc(bog_z_bbox)))
bog_zoomedout = terra::crop(bog_zoomedout, vect(st_as_sfc(bog_z_bbox)), mask=TRUE)

terra::plot(bog_zoomedout)

# bog_el = terra::rast(bog_el_filepath)
# # bog_z_el = terra::rast(bog_zoomedout)
#
# # 'Tare' the elevation to the min value for this raster.
# bog_el_values = values(bog_el)
# ordered_bog_els = bog_el_values[order(bog_el_values)]
# min_bog_el = ordered_bog_els[ordered_bog_els > 0][1]
#
# # Correct the 0s that are likely due to missing data.
# bog_el[bog_el < min_bog_el] <- min_bog_el
#
# # Subtract the minimum bog elevation from all values.
# new_bog_values = values(bog_el) - min_bog_el
# bog_el[] <- new_bog_values

# 'Tare' the elevation to the min value for this raster.
bog_zoomedout_v = values(bog_zoomedout)
bog_zoomedout_v_o = bog_zoomedout_v[order(bog_zoomedout_v)]
min_bog_zoomedout_v = bog_zoomedout_v_o[bog_zoomedout_v_o > 0][1]

# Correct the 0s that are likely due to missing data.
bog_zoomedout[bog_zoomedout < min_bog_zoomedout_v] <- min_bog_zoomedout_v

# Subtract the minimum bog elevation from all values.
new_bog_values = values(bog_zoomedout) - min_bog_zoomedout_v
bog_zoomedout[] <- new_bog_values

terra::plot(bog_zoomedout)
# Can we reduce the resolution of these pixels?
bog_z_a = terra::aggregate(bog_zoomedout,
                 fact = 4)

terra::plot(bog_z_a)

bog_m = rayshader::raster_to_matrix(bog_z_a)

bog_m |>
  sphere_shade(texture = "imhof1") %>%
  plot_map()

bog_m |>
  sphere_shade(texture = "imhof1") %>%
  # add_overlay(my_overlay) |>
  # add_shadow(ray_shade(bog_m, zscale = 3), 0.5) %>%
  # add_shadow(ambient_shade(bog_m), 0) %>%
  plot_3d(bog_m, zscale = 90, fov = 0, theta = 30, zoom = 0.75, phi = 35,
          windowsize = c(1000, 800)#,
          # baseshape = "hex"
          )

bog_coords = st_centroid(bog) |>
  st_transform(crs = 4326) |>
  st_coordinates() |> as.data.frame()

med_coords = c(6.24931631744868, -75.57297670469256)
buc_coords = c(7.115281699926891, -73.11839509263349)

render_label(bog_m,
             lat = bog_coords$Y,
             long = bog_coords$X,
             extent = terra::ext(bog_z_a),
             altitude = 5000, zscale = 90,
             text = "Bogota", textsize = 2, linewidth = 5)
render_label(bog_m,
             lat = med_coords[1],
             long = med_coords[2],
             extent = terra::ext(bog_z_a),
             altitude = 5000, zscale = 90,
             text = "Medellin", textsize = 2, linewidth = 5)
render_label(bog_m,
             lat = buc_coords[1],
             long = buc_coords[2],
             extent = terra::ext(bog_z_a),
             altitude = 5000, zscale = 90,
             text = "Bucaramanga", textsize = 2, linewidth = 5)

filename_movie = tempfile()

#You can change to an oscillating orbit. The magnification is increased and azimuth angle set to 30.
# A title has also been added using the title_text argument.

#Un-comment the following to run:
render_movie(filename = filename_movie, type = "oscillate",
            frames = 60,  phi = 30, zoom = 0.8, theta = -90,
            title_text = "Andes Mountains in Colombia")
# Let's get some satellite imagery...

# Get tile name for Bogota.
search_obj = rstac::stac(attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "stac_source")) %>% # The stac source
  rstac::stac_search(
    datetime = '2023-09-01/2023-10-31',
    collections = attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "collection_name"),
    bbox = sf::st_bbox(bog |> st_transform(crs = 4326)),
    limit = 1) %>%
  rstac::get_request()

s_obj = rstac::stac(attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "stac_source"))
collection_name = attr(rsi::sentinel2_band_mapping$planetary_computer_v1, "collection_name")
tile_name = stringr::str_extract(search_obj$features[[1]]$id, '(?<=R[0-9]{3}_T).{5}(?=_)')
bog_wgs = st_transform(bog, 4326)

it_obj = s_obj |> # The stac source
  rstac::ext_filter(
    # These are our filtering conditions
    collection == {{collection_name}} &&
      `eo:cloud_cover` <= 10  &&
      `s2:mgrs_tile` == {{tile_name}}  &&
      anyinteracts(datetime, interval("2018-09-01", "2023-10-31"))
  ) |>
  rstac::post_request()

# Check out how many features were returned.
it_obj

# When was the earliest satellite image from?
it_obj$features[length(it_obj$features)]

early_date = stringr::str_remove(it_obj$features[length(it_obj$features)][[1]]$id, paste0('.*',tile_name,'_')) |>
  stringr::str_remove('T.*') |>
  lubridate::ymd()


