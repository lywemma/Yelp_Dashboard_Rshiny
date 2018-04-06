library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyr)
library(dplyr)

#yelp_business <- read.csv("yelp_business.csv")
#yelp_sep <- separate_rows(yelp_business, categories,sep=';')

#saveRDS(yelp_sep,"yelp_data.RDS")
# yelp_data <- readRDS("yelp_data.RDS")
# yelp_Rest <- yelp_data %>% filter(categories == "Restaurants")
# 
# set.seed(12345)
# yelp <- yelp_Rest[sample.int(nrow(yelp_Rest), 10000),]

#saveRDS(yelp,"yelp_Restaurants_sample.RDS")

yelp <- readRDS("yelp_Restaurants_sample.RDS")

ui <- bootstrapPage(
  h3("Map View for Yelp Restaurants"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Stars", min(yelp$stars), max(yelp$stars),
                            value = range(yelp$stars), step = 0.1
                ),
                radioButtons("measure", "Size by:", 
                             choices = list("stars","review_count" ),
                             selected= "stars"),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                p(code("Color by Stars"))
  )
)


server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    yelp[yelp$stars >= input$range[1] & yelp$stars <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, yelp$stars)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(yelp) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    sizeby <- input$measure
    if(sizeby == "stars"){
      radius_selected <- 10^yelp[[sizeby]]/10
    }
    else{
      radius_selected <-yelp[[sizeby]]*10
    }
    
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~radius_selected, weight = 1, color = "#777777",
                 fillColor = ~pal(yelp$stars), fillOpacity = 0.7, popup = ~paste(yelp$stars)
      )
  })
  
}

shinyApp(ui, server)