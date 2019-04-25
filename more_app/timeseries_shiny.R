library(shiny)


load("data/all_code.rda")
codes <- all_code$Code

ui <- fluidPage(

  titlePanel("Plot Temperature History"),

  sidebarPanel(
    selectInput("code", "airport code", choices = codes  )
  ),

  mainPanel(
    plotOutput("plot_temp")
  )
)



server <- function(input, output) {
  output$plot_temp <- renderPlot({
    plot_temp_history(input$code)
  })
}



shinyApp(ui, server)
