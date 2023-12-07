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

fluidpage_ui <- fluidPage(

  sidebarLayout(

    sidebarPanel = sidebarPanel(
      h3("Sidebar!"),

      selectInput(
        inputId = 'select_colour',
        label = 'BC Fill Colour',
        choices = c('white','red','blue','purple'),
        selected = 'white'
      ),

      checkboxGroupInput(
        inputId = 'choose_sighting_legit',
        label = 'Sighting Legitimacy',
        choices = c(TRUE,FALSE),
        selected = c(TRUE,FALSE)
      )
    ),

    mainPanel = mainPanel(

      plotOutput('bc_map'),

      DT::DTOutput('data')
    )
  )
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

    sasquatch_sightings |>
      dplyr::filter(sighting_legit %in% input$choose_sighting_legit)

    # This section of code can be much more complicated, e.g. using a bunch of
    # if/then conditions.
  })

  output$data = DT::renderDT({
    sasq_f()
    })

  output$bc_map = renderPlot({
    ggplot() +
      geom_sf(data = bc, fill = input$select_colour) +
      geom_sf(data = sasq_f(), aes(col = sighting_legit)) +
      labs(color = 'Sighting Legitimacy')
  })
}

# The UI and the server 'talk' to each other via inputs and
# outputs. We have inputs in the UI and use those in
# the server to render outputs, which we then embed
# in the UI.

shinyApp(fluidpage_ui, server)








# Let's check out a different page style.

my_sidebar = sidebarPanel(
  h3("Sidebar!"),

  selectInput(
    inputId = 'select_colour',
    label = 'BC Fill Colour',
    choices = c('white','red','blue','purple'),
    selected = 'white'
  ),

  checkboxGroupInput(
    inputId = 'choose_sighting_legit',
    label = 'Sighting Legitimacy',
    choices = c(TRUE,FALSE),
    selected = c(TRUE,FALSE)
  )
)

my_main_panel = mainPanel(
  plotOutput('bc_map'),
  DT::DTOutput('data')
)






navbarPage_ui = shiny::navbarPage(
  title = 'Sasquatch',
  navbarMenu(
    title = 'Menu',
    tabPanel(
      title = 'Map and Datatable',
      sidebarLayout(
        sidebarPanel = my_sidebar,
        mainPanel = my_main_panel
      )
    ),
    tabPanel(
      title = 'About Sasquatch',
      fluidRow(
        column(width = 6,
               div(id = 'sasquatch_photo',
                   style = 'background-image: url("https://kuow-prod.imgix.net/store/7cc0f6fc2c3cc014d25a3d84a6da2ec0.jpg?ixlib=rails-2.1.4&auto=format&crop=faces&fit=clip&h=634&w=924");
                   background-size: contain; background-repeat: no-repeat; width: 100%; height: 300px;')
        ),
        column(width = 6,
               h3("Title for Text"),
               p(paste0(rep("Lorem ipsum",50), collapse = ', '))
        )
      )
    )
  )
)

shinyApp(navbarPage_ui, server)
