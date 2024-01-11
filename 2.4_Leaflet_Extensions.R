# This script continues '2.3_Leaflet_Bigfoot.R' and adds some extra bells and whistles
# to our leaflet map.

# Script Author: Chris Madsen
# Contact info:
# i. email = chris.madsen@gov.bc.ca
# ii. Github = chrispmad

# Script last modified: 2024-01-10

# # # # # # # # # # # # #
#       PART TWO        #
# # # # # # # # # # # # #

library(tidyverse) # A bundle of useful data wrangling packages
library(sf) # common vector spatial object functions
library(leaflet) # base leaflet package
library(leaflet.extras) # leaflet extensions
library(leaflet.extras2) # leaflet extensions
library(leafpop) # leaflet extension for tables/figures as popups
library(htmltools) # Gives us tools to produce and modify HTML code

# 1. Read in data from last script.
bigfoot_dat = readRDS('data/bigfoot_data.Rds')
bigf_sf = bigfoot_dat$bigf_sf
NA_monolith = bigfoot_dat$NA_monolith
l = bigfoot_dat$l
bigf_popup_tables = bigfoot_dat$bigf_popup_tables
choropleth = bigfoot_dat$choropleth

# 2. Map Panes --------------------------------
# If you have multiple overlay groups and want to impose a certain vertical order
# on them, you can use map panes.
quakes_sf = st_as_sf(quakes, coords = c("long","lat"), crs = 4326)
quakes_polygon = st_as_sfc(st_bbox(quakes_sf))

leaflet() |>
  addTiles() |>
  leaflet::addMapPane('marker_pane', zIndex = 500) |>
  leaflet::addMapPane('polygon_pane', zIndex = 400) |>
  addPolygons(
    fillColor = 'purple',
    fillOpacity = 1,
    color = 'purple',
    label = htmltools::HTML('<b style = "font-size: larger; color: red;">I exist to obstruct your view</p>'),
    data = quakes_polygon,
    options = pathOptions(pane = 'polygon_pane'),
    group = 'quake_polygon'
    ) |>
  leaflet::addCircles(data = quakes_sf,
                      options = pathOptions(pane = 'marker_pane'),
                      group = 'quake_circles') |>
  addLayersControl(overlayGroups = c('quake_circles','quake_polygon'),
                   options = layersControlOptions(collapsed = F))

# Next, let's make some heatmaps. Who doesn't love heatmaps?

# 3. Grids and Heatmaps --------------------------------

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

# Back to Leaflet (phew!)

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

# Add in our circle markers to our map template object 'l'
l = l |>
  # Add markers for the bigfoot point vector data.
  leaflet::addCircleMarkers(
    data = bigf_sf,
    fillColor = 'purple',
    color = 'black',
    weight = 2,
    radius = 3,
    label = ~title,
    popup = lapply(bigf_popup_tables, HTML),
    group = 'circles'
  )

# 4. Exploring leaflet.extras and leaflet.extras2 -------------------------

l |>
  # Leaflet has built-in heatmap function
  # (but it recalculates when you zoom in / out, which I don't love)
  leaflet.extras::addHeatmap(data = bigf_sf,
                             blur = 20,
                             max = 0.05,
                             radius = 15,
                             group = 'heatmap') |>
  # Remove this heatmap on map launch - it's a bit too loud.
  hideGroup('heatmap') |>
  # We can add in our own raster image.
  addRasterImage(colors = bigf_r_colpal,
                 x = interp_2,
                 group = 'IDW_heatmap') |>
  # Add legend(s) to map
  addLegend(pal = bigf_r_colpal_legend,
            values = terra::values(interp_2),
            # To put highest colours at the top of the legend, we have to
            # use a custom sorting function...
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
  leaflet.extras::addResetMapButton()

# Allow the user to add things to the map: their GPS location, drawings, etc.
l |>
  leaflet.extras::addControlGPS() |>
  leaflet::addMeasure(primaryLengthUnit = 'kilometers') |>
  leaflet.extras::addDrawToolbar(editOptions = editToolbarOptions()) |>
  leaflet.extras::addSearchFeatures(targetGroups = 'circles',
                                    options = searchFeaturesOptions(propertyName = 'label')) |>
  leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(zoom = 8))


# Add some extra visuals.
l |>
  leaflet.extras::addFullscreenControl() |>
  addMiniMap('bottomleft') |>
  leaflet.extras2::addEasyprint(options = leaflet.extras2::easyprintOptions(hideControlContainer = F)) |>
  leaflet.extras2::addSpinner() # This only shows up in shiny apps...


# If you have paths with timestamps, you can animate the points
# through time!

data <- sf::st_as_sf(leaflet::atlStorms2005[1:5,]) |>
  dplyr::mutate(Name = as.character(Name)) |>
  sf::st_cast("POINT")

data$time <- unlist(lapply(rle(data$Name)$lengths, function(x) {
  seq.POSIXt(as.POSIXct(Sys.Date()-2), as.POSIXct(Sys.Date()), length.out = x)
}))

data

data$time <- as.POSIXct(data$time, origin="1970-01-01")
data$label <- paste0("Time: ", data$time)
data$popup = sprintf("<h3>Customized Popup</h3><b>Name</b>: %s<br><b>Time</b>: %s",
                     data$Name, data$time)
data <- split(data, f = data$Name)

leaflet() |>
  addTiles() |>
  leaflet.extras2::addPlayback(
    data = data,
    popup = ~popup,
    label = ~label,
    popupOptions = popupOptions(offset=c(0,-35)),
    labelOptions = labelOptions(noHide = TRUE), # We can make labels ALWAYS visible.
    options = playbackOptions(radius = 3,
                              tickLen = 1000000,
                              speed = 10000,
                              maxInterpolationTime = 10000,
                              transitionpopup = FALSE,
                              transitionlabel = FALSE,
                              playCommand = "Let's go",
                              stopCommand = "Stop it!",
                              color = c("red","green","blue",
                                        "orange","yellow")),
    pathOpts = pathOptions(weight = 5))


# If you have the (unlikely) dataset of velocity data of e.g. wind or current,
# you can make maps like these!
wind_url = 'https://raw.githubusercontent.com/onaci/leaflet-velocity/master/demo/wind-global.json'

leaflet() |>
  addTiles(group = "base") |>
  setView(145, -20, 4) |>
  addVelocity(content = wind_url, group = "velo", layerId = "veloid",
              options = velocityOptions(velocityScale = 0.05)) |>
  addLayersControl(baseGroups = "base", overlayGroups = "velo")

# 5. custom easyButtons with JavaScript --------------------------------

# A final note:
# leaflet maps are, in the end, javascript; we can write our own custom
# javascript functions and add them to our maps either through
# the addition of 'easyButtons' or through functions like 'on_render()',
# which can be useful for things like layer ordering etc.

# Let's make some custom buttons.
freeze_points_button = easyButton(
  icon = 'fa-snowflake-o',
  title = 'Freeze points',
  onClick = JS("function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
)

zoom_to_globe_button = easyButton(
  icon="fa-globe", title="Zoom to Level 1",
  onClick=JS("function(btn, map){ map.setZoom(1); }"))

find_me_button = easyButton(
  icon="fa-crosshairs", title="Locate Me",
  onClick=JS("function(btn, map){ map.locate({setView: true}); }"))

leaflet() |>
  addTiles() |>
  addAwesomeMarkers(data = quakes_sf,
                    clusterOptions = markerClusterOptions(maxClusterRadius = 25),
                    clusterId = "quakesCluster") |>
  addEasyButton(freeze_points_button) |>
  addEasyButton(zoom_to_globe_button) |>
  addEasyButton(find_me_button)

# 6. Leaflet in Shiny ---------------------------

# Leaflet maps are awesome to include in R Shiny apps. One interesting
# usecase is to use these maps as selectors - when people click on a part of the map,
# we can 'listen' to that click and see what was selected.

library(shiny)
library(bslib)

ui = bslib::page_fluid(
  layout_column_wrap(
    1/2,
    div(
    textOutput('selected_prov_state'),
    plotOutput('my_barplot')
    ),
    leafletOutput('bigfoot_map')
  )
)

server = function(input, output, server){

  bf_pal = colorNumeric(palette = 'viridis',
                        domain = choropleth$bigfoot_sightings)

  output$bigfoot_map = renderLeaflet({
    l |>
      hideGroup('circles') |>
      addPolygons(
        color = '#424447',
        weight = 2,
        fillColor = ~bf_pal(bigfoot_sightings),
        fillOpacity = 0.75,
        label = ~paste0(state_prov,': ',bigfoot_sightings),
        layerId = ~state_prov,
        data = choropleth
      ) |>
      addLegend(pal = bf_pal, values = choropleth$bigfoot_sightings) |>
      leaflet.extras2::addSpinner()
  })

  output$selected_prov_state = renderText({
    # The input name depends on what you call your map!
    input$bigfoot_map_shape_click$id

    # # Also note that you have access to:
    # map_bounds
    # map_center
    # map_zoom
    # map_groups
    # map_click
    # map_shape_mouseover
  })

  output$my_barplot = renderPlot({

    # Add column indicating which state_prov has been selected in leaflet map.
    if(!is.null(input$bigfoot_map_shape_click$id)){
      choropleth = choropleth |>
        dplyr::mutate(selected = ifelse(state_prov == input$bigfoot_map_shape_click$id, T, F))
    } else {
      choropleth$selected = FALSE
    }

    choropleth |>
      ggplot() +
      geom_col(aes(x = reorder(state_prov,-bigfoot_sightings),
                   y = bigfoot_sightings,
                   fill = selected)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  })
}

shiny::shinyApp(ui = ui, server = server)
