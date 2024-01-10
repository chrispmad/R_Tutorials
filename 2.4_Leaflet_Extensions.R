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



library(tidyverse) # A bundle of useful data wrangling packages
library(sf) # common vector spatial object functions
library(leaflet) # base leaflet package
library(leaflet.extras) # leaflet extensions
library(leaflet.extras2) # leaflet extensions
library(leafpop) # leaflet extension for tables/figures as popups


# Let's explore things we can add from the {leaflet.extras} package

l = l |>
  addCircleMarkers(data = bigf_sf,
                   label = ~title,
                   popup = lapply(bigf_popup_tables,
                                  shiny::HTML),
                   group = 'circles',
                   clusterOptions = markerClusterOptions(maxClusterRadius = 25))




# We can add raster layers too.
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
  # Add markers for the bigfoot point vector data.
  # We can add in our own raster image.
  addRasterImage(colors = bigf_r_colpal,
                 x = res2,
                 group = 'IDW_heatmap') |>
  # Add legend(s) to map
  addLegend(pal = bigf_r_colpal_legend,
            values = terra::values(res2),
            # To put highest colours at the top of the legend, we have to
            # use a custom sorting function...
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
  leaflet.extras::addResetMapButton()

# Allow the user to add things to the map: their GPS location, drawings, etc.
l |>
  leaflet.extras::addControlGPS() |>
  leaflet::addMeasure() |>
  leaflet.extras::addDrawToolbar() |>
  leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(zoom = 8))


# Add some extra visuals.
l |>
  leaflet.extras::addFullscreenControl() |>
  addMiniMap('bottomleft') |>
  leaflet.extras2::addEasyprint('topright') |>
  leaflet.extras2::addSpinner() # This only shows up in shiny apps...




data <- sf::st_as_sf(leaflet::atlStorms2005[1:5,])
data$Name <- as.character(data$Name)
data <- st_cast(data, "POINT")
data$time <- unlist(lapply(rle(data$Name)$lengths, function(x) {
  seq.POSIXt(as.POSIXct(Sys.Date()-2), as.POSIXct(Sys.Date()), length.out = x)
}))
data$time <- as.POSIXct(data$time, origin="1970-01-01")
data$label <- paste0("Time: ", data$time)
data$popup = sprintf("<h3>Customized Popup</h3><b>Name</b>: %s<br><b>Time</b>: %s",
                     data$Name, data$time)
data <- split(data, f = data$Name)

leaflet() %>%
  addTiles() %>%
  leaflet.extras2::addPlayback(
    data = data,
    popup = ~popup,
    label = ~label,
    popupOptions = popupOptions(offset=c(0,-35)),
    labelOptions = labelOptions(noHide = TRUE),
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



wind_url = 'https://raw.githubusercontent.com/onaci/leaflet-velocity/master/demo/wind-global.json'

saeesh = jsonlite::read_json(wind_url)

View(saeesh)

leaflet() %>%
  addTiles(group = "base") %>%
  setView(145, -20, 4) %>%
  addVelocity(content = wind_url, group = "velo", layerId = "veloid",
              options = velocityOptions(velocityScale = 0.05)) %>%
  addLayersControl(baseGroups = "base", overlayGroups = "velo")




# Map Panes #
quakes_polygon = st_as_sfc(st_bbox(quakes_sf))
leaflet() |>
  addTiles() |>
  addPolygons(
    fillColor = 'purple',
    fillOpacity = 1,
    color = 'purple',
    data = quakes_polygon,
    group = 'quake_polygon') |>
  leaflet::addMapPane('special_pane', zIndex = 500) |>
  leaflet::addCircles(data = quakes_sf,
                      # options = pathOptions(pane = 'special_pane'),
                      group = 'quake_circles') |>
  addLayersControl(overlayGroups = c('quake_circles','quake_polygon'))

# A final note:
# leaflet maps are, in the end, javascript; we can write our own custom
# javascript functions and add them to our maps either through
# the addition of 'easyButtons' or through functions like 'on_render()',
# which can be useful for things like layer ordering etc.

# library(htmltools)
# library(htmlwidgets)

leaflet() |>
  addTiles() |>
  addAwesomeMarkers(data = quakes_sf,
                    clusterOptions = markerClusterOptions(maxClusterRadius = 25),
                    clusterId = "quakesCluster") |>
  addEasyButton(
    easyButton(
      icon = 'fa-snowflake-o',
      title = 'Freeze points',
      onClick = JS("function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
    )
  ) |>
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 1",
    onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
