# UI for MLB Birthplace info interactive graphs
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(ggvis)

#
# Shiny UI function
#

shinyUI(fluidPage(
  
  # ggvis
  ggvisOutput("ggvis"),
  # Slider and select inputs
  absolutePanel(top = 10, left = 810,width = 200,
                uiOutput("yrange2"),uiOutput("xrange2"),
                selectInput("dataset", "Dataset :",
                            c("Countries","States (US)","States (Ven.)","States (D.R.)"),
                            selected = "Countries"
                ),
                selectInput("yvar", "Choose y-axis :",
                              choices = c("% Players","Players","% WAR","WAR"), 
                              selected = "% Players")
  ),
  # Logo and header links to website and references
  absolutePanel(top = 432, left = 810, width = 200,
                # Header and logo
                div(style="background:white; opacity:0.7",
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Logo.png", width = "40px", height= "35px")),
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Header.png",width = "155px", height= "35px"))
                    
                ),
                # References
                div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:7px", 
                    div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:7px", 
                        "Source :", 
                        a( href = "http://www.baseball-reference.com/",
                           style ="text-decoration:none",target = "_blank",
                           "Baseball-Reference")))
                )
  )
  )







