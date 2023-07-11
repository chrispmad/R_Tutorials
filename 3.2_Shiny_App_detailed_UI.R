library(shiny)
library(bslib)
library(shinyWidgets)

my_theme = bslib::bs_theme(bootswatch = 'lumen',
                heading_font = 'Merriweather,serif',
                base_font = 'Source Sans Pro',
                # primary = "#EA80FC", 
                # secondary = "#48DAC6",
                "sidebar-bg" = '#ADD8E7')

the_sidebar = sidebar(
  h5("Sidebar Title"),
  layout_column_wrap(
    width = 1,
    card(
      card_body(
        checkboxInput(
          'a',
          label = 'Checkbox',
          value = F
        )
      )
    ),
    card(
      card_body(
        switchInput(
          'b',
          label = 'Switch',
          value = F
        )
      )
    ),
    card(
      card_body(
        selectizeInput(
          'c',
          label = 'Selectize',
          choices = unique(iris$Species)
        )
      )
    )
  )
)

main_content = navs_pill_card(
  nav('A',h5("Some content")),
  nav('B',h5("What's in here?")),
  nav('C',h5("Hello!")),
)

ui <- page_fillable(
  
  theme = my_theme,
  
  layout_sidebar(
    sidebar = the_sidebar,
    main_content
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)