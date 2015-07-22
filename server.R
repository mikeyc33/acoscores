library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

acodata <- allacos

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(acodata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(acodata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- acodata[[colorBy]]
    pal <- colorBin("YlOrRd", colorData, 7, pretty = FALSE)
    
    if (sizeBy %in% c("CAHPS_score","ACO.1","ACO.2","ACO.3","ACO.4","ACO.5","ACO.6","ACO.7")){
      radius <- acodata[[sizeBy]] / max(acodata[[sizeBy]], na.rm=T) * 30000
    }
    else if (sizeBy %in% c("bench_minus_assign_bene_exp","exp","benchmark_exp")){
      radius <- acodata[[sizeBy]] / max(acodata[[sizeBy]], na.rm=T) * 150000
    }
    else if (sizeBy %in% c("ACO.8.","ACO.9.","ACO.10.")){
      radius <- acodata[[sizeBy]] / max(acodata[[sizeBy]], na.rm=T) * 50000
    }
    else if (sizeBy == "benes"){
      radius <- acodata[[sizeBy]] / max(acodata[[sizeBy]], na.rm=T) * 60000
    }
    else{
      radius <- acodata[[sizeBy]] / max(acodata[[sizeBy]], na.rm=T) * 50000
    }
    
    legend_name <- as.character(legend[legend$var==colorBy,2])

    leafletProxy("map", data = acodata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomright", pal=pal, values=colorData, title=legend_name,
                layerId="colorLegend")

  })
  
  # Show a popup at the given location
  showACOPopup <- function(zipcode, lat, lng) {
    selectedACO <- allacos[allacos$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4(selectedACO$aco),
      sprintf("CAHPS QUALITY POINTS (0-14): %.2f", round(selectedACO$CAHPS_score, digits=2)),tags$br(),
      sprintf("ACO Formation Start Date: %s", format(selectedACO$Agreement.Start.Date,big.mark=',')), tags$br(),
      sprintf("ACO States Served: %s", format(selectedACO$ACO.Service.Area, big.mark=',')), tags$br(),
      sprintf("Total No. of Assigned Beneficiaries: %s", format(selectedACO$benes,big.mark=','))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showACOPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showACOPopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
   
})