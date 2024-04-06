#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(readxl)
library(shiny)
library(dplyr)
library(htmlwidgets)
library(sf)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  observe({
      
      # Data -----------------------------------------------------------------------
      documents <- read_excel("./db/db_new.xlsx", sheet = "documents", col_names = T)
      transactions <-  read_excel("./db/db_new.xlsx", sheet = "transactions", col_names = T)
      icons <-  read_excel("./db/db_new.xlsx", sheet = "icons", col_names = T)
      polygons <- read_excel("./db/db_new.xlsx", sheet = "polygons", col_names = T)
      
      if(is.null(input$donor)){donor_filter <- unique(documents$donor)} else {donor_filter <- input$donor}
      if(is.null(input$recipient)){recipient_filter <- unique(documents$recipient)} else {recipient_filter <- input$recipient}
      if(is.null(input$asset_type)){asset_filter <- unique(transactions$asset)} else {asset_filter <- input$asset_type}
      
      data <- transactions %>%
        left_join(documents,  by = c("doc_id" = "id")) %>%
        left_join(icons,  by = c("icon_id" = "id"))
      
      data <- data %>%
        filter(donor %in% donor_filter) %>%
        filter(recipient %in% recipient_filter) %>%
        filter(asset %in% asset_filter)
      
      startDate <- which(as.numeric(do.call(rbind, strsplit(data$start_date, split = "-"))[,1]) >= input$range[1])
      endDate <- which(as.numeric(do.call(rbind, strsplit(data$end_date, split = "-"))[,1]) <= input$range[2])
      
      date_filter <- intersect(startDate, endDate)
      data <- data[date_filter,]
      
      data$start_date <- paste(
                                as.numeric(do.call(rbind, strsplit(data$start_date, split = "-"))[,3]),
                                as.numeric(do.call(rbind, strsplit(data$start_date, split = "-"))[,2]),
                                as.numeric(do.call(rbind, strsplit(data$start_date, split = "-"))[,1]),
                                sep="/")
      data$end_date <- paste(
                              as.numeric(do.call(rbind, strsplit(data$end_date, split = "-"))[,3]),
                              as.numeric(do.call(rbind, strsplit(data$end_date, split = "-"))[,2]),
                              as.numeric(do.call(rbind, strsplit(data$end_date, split = "-"))[,1]),
                              sep="/")
      # Build Polygons ----
      # from coordinated to points (geometry) to polygons
      polygons <- st_as_sf(polygons, coords = c("long", "lat"), crs = 4326) %>%
        group_by(transaction_id) %>%
        summarise(geometry = st_combine(geometry)) %>%
        mutate(geometry = st_cast(geometry, "POLYGON"))
      
      # Add polygons to the data
      data <- data %>%
        left_join(polygons, by = c("id" = "transaction_id"))
        
      
      # Function to show a modal dialog on the right side ----
      showRightSideModal <- function(content) {
        showModal(modalDialog(
          title = NULL,
          tagList(
            tags$button(type = "button", class = "close", "data-dismiss" = "modal", "aria-label" = "Close", tags$span("aria-hidden" = "true", HTML("&times;"))),
            content
          ),
          size = "l", # Large modal
          easyClose = TRUE, # Can be closed by clicking outside the modal
          footer = NULL, # No footer
        ))
      }
      
      # Map ----------------------------------------------------------------------------
      output$mymap <- renderLeaflet({

        #* Render Map -------------------
        leaflet(data = data) %>% 
          addProviderTiles(providers$Stadia.StamenWatercolor) %>%
          addMarkers(lng = data$long, lat = data$lat,
                     icon = list(
                       iconUrl = data$path,
                       iconSize = c(65, 65),
                       iconAnchorY = 10
                     ),
                     layerId = ~row.names(data), # To identify the marker clicked
                     clusterOptions = markerClusterOptions())  %>%
          addPolygons(data = data$geometry, color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = "red",
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup=paste("<b>",
                                  ifelse(is.na(data$loc_name),data$ancient_loc_name,data$loc_name),
                                  "</b>")) %>%
                    onRender(
                      "function(el, x) {
                    L.control.zoom({
                      position:'bottomright'
                    }).addTo(this);
                  }")
      })
      
      # Observe clicks on the marker and show a modal dialog
      observeEvent(input$mymap_marker_click, {
        markerClick <- input$mymap_marker_click
        markerIndex <- as.numeric(markerClick$id)
        
        # Extracting information for the clicked marker.
        marker_data <- data[markerIndex, ]
        
        # Center the map on the clicked marker while maintaining current zoom level.
        lat <- markerClick$lat
        lng <- markerClick$lng
        leafletProxy("mymap") %>%
          setView(lng = (lng+0.3), lat = lat, zoom = input$mymap_zoom)
        
        # Custom modal content.
        content <- {paste(
          sprintf("<h2 class='popup-title'>%s (<span class='italic'>%s</span>)</h2>", 
                  marker_data$loc_name, 
                  marker_data$ancient_loc_name),
          sprintf("<div class='group'>
                    <div class='sub-group'>
                      <h3 class='sub-title'>Asset type:</h3> <p>%s</p>
                    </div>
                    <div class='sub-group'>  
                      <h3 class='sub-title'>Quantity:</h3> <p>%s %s</p>
                    </div>
                    <div class='sub-group'>
                      <h3 class='sub-title'>Penalty:</h3> <p>%s %s</p>
                    </div>
                   </div>", 
                  marker_data$asset_type, 
                  marker_data$quantity, 
                  ifelse(is.na(marker_data$quantity_measure), "", marker_data$quantity_measure),
                  ifelse(is.na(marker_data$penalty_amount), "No penalty", marker_data$penalty_amount),
                  ifelse(is.na(marker_data$penalty_currency), "", marker_data$penalty_currency)),
          sprintf("<div class='group'>
                    <div class='sub-group'>
                      <h3 class='sub-title'>Donor:</h3> <p>%s</p>
                    </div>
                    <div class='sub-group'>
                      <h3 class='sub-title'>Beneficiary:</h3> <p>%s</p>
                    </div>
                   </div>", 
                  marker_data$donor, 
                  marker_data$recipient),
          sprintf("<div class='sub-group'>
                      <h3 class='sub-title'>Date:</h3> <p>%s %s</p>
                   </div>", 
                  marker_data$start_date, 
                  ifelse(marker_data$end_date==marker_data$start_date,"", paste("-", marker_data$end_date, sep = " "))),
          sprintf("<h3 class='sub-title'>Notes:</h3> <p class='other-sources'>%s</p>", 
                  ifelse(is.na(marker_data$notes.x), "No notes available", marker_data$notes.x)),
          "<hr>",
          "<h2 class='popup-title'>Document Info</h2>",
          sprintf("<div class='sub-group'>
                    <h3 class='sub-title'>Source name:</h3>
                    <a href=%s target='_blank'>%s</a>
                  </div>", 
                  marker_data$link, 
                  marker_data$source_name),
          sprintf("<h3 class='sub-title'>Notes:</h3> <p class='other-sources'>%s</p>", 
                  ifelse(is.na(marker_data$notes.y), "No notes available", marker_data$notes.y)),
          sprintf("<h3 class='sub-title'>Other sources:</h3> <p class='other-sources'>%s</p>", 
                  ifelse(is.na(marker_data$other_sources), "No other sources available", marker_data$other_sources)),
          sep="")}
        
        # Display modal with content from clicked marker.
        showRightSideModal(
          content = HTML(content) # Use HTML function to render HTML content
        )
      })
      
    })
})
