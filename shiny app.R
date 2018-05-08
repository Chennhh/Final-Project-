#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

server <- function(input, output) {
  
  # build data with 2 places
  data=data.frame(x=c(-71.06625,-71.06077,-71.05627,-71.08737,-71.06256,-71.1288,-71.08543,-71.06144,-71.07789,-71.07557),
                  y=c(42.34382,42.35113,42.36059,42.34577,42.35171,42.35246,42.34848,42.35098,42.36625,42.3469),
                  s=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'),
                  id=c("Myer's Chang", "Shojo","Koy","PhoBsil","Q restaurant","Dolphin Bay Taiwanese","santouku","New dong khanh","Shabu&Mien","douzo"))
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng=-71.0589 , lat =42.3601, zoom=14) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data=data, ~x , ~y, layerId=~id, popup=~id, radius=8 , color="white",  fillColor="black", stroke = TRUE, fillOpacity = 1.0)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })

  
  output$text <- renderText({
    restaurant = data_of_click$clickedMarker$id
    restaurant_style = data_of_click$clickedMarker$s
    if(is.null(restaurant)){restaurant="null"}
    if(is.null(restaurant_style)){restaurant_style='@'}
    paste(paste("Restaurant: ", restaurant), restaurant_style)
    })
}


ui <- fluidPage(
  fluidRow(
    column(8,leafletOutput("map", height=700)),
    column(4, textOutput("text"))
  )
)

shinyApp(ui = ui, server = server)

