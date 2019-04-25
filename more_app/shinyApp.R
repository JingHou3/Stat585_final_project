library(dplyr)
library(leaflet)
library(tidyr)
load(file = 'data/ID.rda')
load(file = 'data/type.rda')
ui <- navbarPage("Airport Weather",
                 sidebarPanel(
                   selectInput("airport",
                               "Airport ID:",
                               c(ID),
                               multiple = TRUE, selected = "KDHN"),

                   selectInput("type",
                               "Weather element:",
                               c(unique(as.character(type))),
                               multiple = TRUE, selected = "temp_f")
                 ),
                 # Main panel for displaying outputs ----
                 mainPanel(
                   # Output: Tabset w/ plot, summary, and table ----
                   tabsetPanel(type = "tabs",
                               tabPanel("Map", leafletOutput("map")),
                               tabPanel("Information", tableOutput("table"))
                   )
                 )
)

server <- function(input, output) {
    #only 2 input by now, could add lat,lon if needed.

        d1<-reactive ({
          df<-current_weather_more(id_vector=input$airport,type=input$type) %>%
          mutate_if(is.factor,as.character) %>%
          unite("info",-c("location","latitude","longitude","observation_time"),sep = "<br/>",remove = FALSE)
          df
          })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(data = d1()) %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(~ longitude, ~ latitude, label = ~location) %>%
        leaflet::addPopups(~ longitude, ~ latitude, ~info)
    })

      output$table <- renderTable({
        d1()[,-1]
      })

}

# Run the application
shinyApp(ui = ui, server = server)
