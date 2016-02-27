# UI for MLB Birthplace info interactive map 
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(leaflet)

#
# Shiny UI function
#

shinyUI(fluidPage(
  
  # Add a little CSS to modify floater aethetics
  tags$head(tags$style("
    .floater { background-color: rgb(255,255,255); padding: 8px; 
 border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }

")),
  
  # Leaflet Map output
  leafletOutput("map", width =1000, height = 600),
  
  # WAR/ Career WAR Slider Inputs
  absolutePanel(top = 10, left = 810, width = 200,
                uiOutput("war2"),uiOutput("cwar2")),
  
  # Select Inputs for year and dynamically rendered team list
  absolutePanel(top=200, left =890, width=125,
                selectInput("year", "Year :", 
                            c("2016 (Proj.)",2015:1876),
                            selected = 2015
                ),
                uiOutput("team2")
  ),
  
  # Show team legend and reset view panel
  absolutePanel(top = 330, left = 890, width = 125,
                checkboxInput("legend", "Show Teams", FALSE),
                actionButton("resetview", "Reset View")
  ),
  
 uiOutput("refs"),
  
  
  # Position point size legend on left if not showing team colors legend
  conditionalPanel(condition ="input.legend == false || (input.team2 != 'All Teams'&& input.team2 != 'AL Teams'&& input.team2 != 'NL Teams')",
                   absolutePanel(left = 25, top = 490, width = 135, class = "floater", style="opacity:0.7",
                                 div(style="margin:0",
                                     div(uiOutput("ptlegend"),
                                         div(style="",
                                             img(src = "PtSizeLegend.jpg", width = "124px", height= "68px"))
                                         
                                         
                                     )    
                                 ))
  ),
  
  # Position point size legend to right of team color legend when it is shown
  conditionalPanel(condition ="input.legend == true && (input.team2 == 'All Teams' || input.team2 == 'AL Teams' || input.team2 == 'NL Teams')",
                   absolutePanel(left = 120, top = 490, width = 135, class = "floater",style="opacity:0.7",
                                 div(style="margin:0",
                                     div(uiOutput("ptlegend2"),
                                         div(style="",
                                             img(src = "PtSizeLegend.jpg", width = "124px", height= "68px"))
                                         
                                         
                                     )    
                                 )   
                   )
  )
  
  ))
  
  
  


