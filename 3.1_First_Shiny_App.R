library(shiny)
library(tidyverse)
library(sf)

# The UI of the page. All components are nested arguments
# inside whichever kind of 'page' you choose. Therefore,
# you have to put commas between each thing.
# fluidPage, fixedPage, fillPage, navbarPage, etc.

# Some foundational input / output types are:
# UI inputs: selectInput, checkboxInput, sliderInput.
# Server: renderPlot, renderText, renderPlotly, renderLeaflet, renderDT

# A recent upgrade to shiny's own UI options is the 
# package {bslib}. Highly recommended. It gives 
# more flexible page types, cards to put content in,
# and some short-cuts for quickly theming your page
# just how you like it.

ui <- fluidPage(
  
  selectInput(
    inputId = 'select_colour',
    label = 'BC Fill Colour',
    choices = c('white','red','blue','purple'),
    selected = 'white'
  ),
  
  selectInput(
    inputId = 'choose_sighting_legit',
    label = 'Choose Sighting Legitimacy',
    choices = c("All","Just Bona Fide"),
    selected = 'All'
  ),
  
  plotOutput('bc_map'),
  
  DT::DTOutput('data')
)

# The server functions like any R script (i.e.,
# lines of code are run sequentially)
server <- function(input, output, session) {
  
  # Load in 'raw'/static data
  bc = bcmaps::bc_bound()
  
  sasquatch_sightings = tibble(sighting_legit = rep(c(TRUE,FALSE),10),
                             lat = rnorm(n = 20, mean = 51, sd = 2),
                             lng = rnorm(n = 20, mean = -120, sd = 2)) %>% 
    st_as_sf(coords = c('lng','lat'), crs = 4326)
  
  # Use inputs from UI to make a 'reactive' form of data.
  sasq_f = reactive({
    
    if(input$choose_sighting_legit == 'All') {
      return(sasquatch_sightings)
    }
    if(input$choose_sighting_legit != 'All'){
      sasquatch_sightings %>% 
      filter(sighting_legit == T)
    }
  })
  
  output$data = DT::renderDT(sasq_f())
  
  output$bc_map = renderPlot({
    ggplot() +
      geom_sf(data = bc, fill = input$select_colour) +
      geom_sf(data = sasq_f(), aes(col = sighting_legit))
  })
}

# The UI and the server 'talk' to each other via inputs and 
# outputs. We have inputs in the UI and use those in 
# the server to render outputs, which we then embed
# in the UI.

shinyApp(ui, server)