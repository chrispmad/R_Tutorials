library(shiny)

# A big challenge in designing a complicated shiny app is 
# organization / cleanliness. Some good things to do:
# 1. Find parts of your code that are dense and hard to read
#    and put them in separate script(s) as their own functions.
# 2. Do the same thing for reactive parts of code by putting
#    them in their own 'modules'
# 3. Put your UI defining section into another script.

# What is a module? It's like a tiny shiny app that we 
# can embed inside our main app. This can helpfully
# partition our reactive code into semi-isolated
# chunks, which really facilitates trouble-shooting
# problems (THIS HAPPENS ALL THE TIME)

# FUNCTIONS # 

# Make fake data.
generate_fake_points = function(number_points){
  tibble(sighting_legit = rep(c(TRUE,FALSE),number_points/2),
         lat = rnorm(n = number_points, mean = 51, sd = 2),
         lng = rnorm(n = number_points, mean = -120, sd = 2)) %>% 
    st_as_sf(coords = c('lng','lat'), crs = 4326)
}

data_filtering_module_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    selectInput(
      inputId = ns('choose_sighting_legit'),
      label = 'Choose Sighting Legitimacy',
      choices = c("All","Just Bona Fide"),
      selected = 'All'
    )
    
  )
}

data_filtering_module_server <- function(id, raw_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Use inputs from UI to make a 'reactive' form of data.
      sasq_f = reactive({
        if(input$choose_sighting_legit == 'All') {
          return(raw_data)
        }
        if(input$choose_sighting_legit != 'All'){
          raw_data %>% 
            filter(sighting_legit == T)
        }
      })
      
     return(sasq_f)
    }
  )
}


ui <- fluidPage(
  
  selectInput(
    inputId = 'select_colour',
    label = 'BC Fill Colour',
    choices = c('white','red','blue','purple'),
    selected = 'white'
  ),
  
  # Filter raw data with module...
  data_filtering_module_UI(id = 'data_filter'),
  
  # Plot output
  plotOutput('bc_map'),
  
  # Data.Table output:
  DT::DTOutput('data')
)

server <- function(input, output, session) {
  
  # Load in 'raw'/static data
  bc = bcmaps::bc_bound()
  
  # 
  sasquatch_sightings = generate_fake_points(number_points = 20)
  
  # Filter raw data with module...
  sasq_f = data_filtering_module_server(id = 'data_filter',
                                        raw_data = sasquatch_sightings)
  
  output$data = DT::renderDT(sasq_f())
  
  output$bc_map = renderPlot({
    ggplot() + 
      geom_sf(data = bc, fill = input$select_colour) + 
      geom_sf(data = sasq_f(), aes(col = sighting_legit))
  })
}

shinyApp(ui, server)