#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Library ----
library(leaflet)
library(readxl)
library(shiny)
library(shinyWidgets)

# Import Data ----
documents <- read_excel("./db/db_new.xlsx", sheet = "documents", col_names = T)
transactions <-  read_excel("./db/db_new.xlsx", sheet = "transactions", col_names = T)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Style ----  
  tags$head(
    tags$style( # Font
      HTML('
            * { 
              margin: 0;
              padding: 0;
            }
            
            img {
              margin: 10px 0;
            }
            
            #dropdown-menu-MyDropDownN {    
              background-color: #dcdcdc !important;
              z-index: 1002;
            }
            
            .filters {
              display: flex;
              flex-direction: row;
              justify-content: space-between;
              margin: 0 0 20px 0;
            }
            
            #mymap {
              background: red;
              position: absolute;
              top:0px;
              right:0px;
              bottom:0px;
              left:0px;
              z-index: -1;
            }
            
            .leaflet-left .leaflet-control{
              visibility: hidden;
            }
            
            /* Popup Style */
            .modal-backdrop {
              background-color: transparent !important;
            }
            
            .modal-dialog {
              width: auto;
            }
            
            .modal-content {
              width: min-content !important;
            }
            
            .modal-dialog {
              position: fixed;
              top: 10px;
              right: 10px;
              bottom: 10px;
              margin: 0;
            }
            
            .popup-title {
              color: brown;
              margin-bottom: 15px;
            }
            
            .italic {
             font-style: italic;
            }
            
            .group {
              display: flex;
            }
            
            .sub-group {
             display: flex;
             margin: 0 10px 0 0;
            }
            
            .sub-title {
              font-size: 20px;
              width: max-content;
              color: #3b3b3b;
            }
            
            p {
              color: #3b3b3b;
              font-size: 18px;
              margin-left: 10px !important;
              margin-right: 10px !important;
              margin-bottom: 10px !important;
              margin: auto;
              transform: translateY(2px);
              width: max-content;
            }
            
            .other-sources {
              width: 100% !important;
            }
            
            a{
              font-size: 20px;
              margin: auto;
              margin-left: 10px;
              transform: translateY(6px);
            }
            
            @media screen and (max-width: 800px) {
            
              .popup-title {
                font-size: 13px
              }
            
              .sub-title, a {
                font-size: 11px;
              }
              
              p {
                font-size: 9px;
                margin-left: 2px !important;
                margin-right: 1px !important;
              }
              
            }
            '
          )
      ),
      tags$script(HTML("
            $(document).on('shiny:connected', function(event) {
                setTimeout(function() {
                    var map = Shiny.shinyapp.$values['mymap']; // Replace 'leaflet-map-id' with the actual id of your leaflet map output
                    map.on('popupopen', function(e) {
                        e.popup._container.className += ' custom-popup';
                    });
                }, 1000);
            });
        "))
    ),

    # Application title
    img(src = "logo.svg", height = 30),

    # Sidebar with a slider input for number of bins
    tags$div(class = "filters",
      dropdown(
        inputId = "MyDropDownN",
        tags$h3("Filters"),
        selectizeInput(inputId = "donor" ,
                       label = "Donor:" , 
                       choices = unique(documents$donor), 
                       multiple = TRUE, 
                       options = list(plugins= list('remove_button')) ), 
        selectizeInput(inputId = "recipient" ,
                       label = "Recipient:" , 
                       choices = unique(documents$recipient), 
                       multiple = TRUE, 
                       options = list(plugins= list('remove_button')) ),
        selectizeInput(inputId = "asset_type" ,
                       label = "Asset type:", 
                       choices = unique(transactions$asset), 
                       multiple = TRUE, 
                       options = list(plugins= list('remove_button')) ),
        sliderInput("range", "Time interval:",
                    min = 500, max = 1800,
                    value = c(800, 1500)),
        
        style="material-circle", 
        status = "danger", 
        icon = icon("filter"), 
        width = "300px",
        tooltip = tooltipOptions(title = "Select Filters")
      )),

    tags$div(class = "map", leafletOutput("mymap", height="100vh", width = "100vw"))
  )
)
