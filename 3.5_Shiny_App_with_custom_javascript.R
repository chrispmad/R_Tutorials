library(shiny)

# This javascript code is a bit dense to read (for me);
# It adds functionality to our app such that:
# 1. We can add an attribute 'data-proxy-click' to any piece of the user interface
#    (usually action buttons) so that it will be activated by keystroke 13 (Enter key)

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

ui <- fluidPage(
  # The following line is one way to include custom javascript in our shiny app.
  tags$head(tags$script(HTML(jscode))),
  # Define an action button, but set its CSS style to 'hidden'; this hides our
  # magician's trick from the audience.
  actionButton("enter_proxy", "", style = 'visibility:hidden;'),

  # Set up the text input box. Add an attribute to it that specifies what shiny
  # input name we want to use to respond to its javascript-enabled sensitivity to
  # someone pressing key 13 (Enter).
  textInput("text", 'Name here please:', "") |>
    tagAppendAttributes(`data-proxy-click` = "enter_proxy"), # We can refer to input$enter_proxy now.
  textOutput('name')
)

server <- function(input, output, session) {

  # Reactive expression that only evaluates when a certain input is clicked/entered,
  # in this case, the enter_proxy action button.
  entered_text = eventReactive(input$enter_proxy, {
    input$text
  })

  # Render the text as an output so that we can print to the UI.
  output$name = renderText({
    entered_text()
  })
}

shinyApp(ui, server)
