# Server for MLB Birthplace info interactive graph 
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(ggvis)

# Load various datasets
statesUS <- readRDS("data/StatesMLBBirth.rds")
statesVen <- readRDS("data/StatesVenMLBBirth.rds")
statesDR <- readRDS("data/StatesDRMLBBirth.rds")
countries <- readRDS("data/CountryMLBBirth.rds")

# Fix column names with % sign
names(countries)[names(countries)=="PctWAR"] <- "% WAR"
names(statesUS)[names(statesUS)=="PctWAR"] <- "% WAR"
names(statesVen)[names(statesVen)=="PctWAR"] <- "% WAR"
names(statesDR)[names(statesDR)=="PctWAR"] <- "% WAR"
names(countries)[names(countries)=="PctPlayers"] <- "% Players"
names(statesUS)[names(statesUS)=="PctPlayers"] <- "% Players"
names(statesVen)[names(statesVen)=="PctPlayers"] <- "% Players"
names(statesDR)[names(statesDR)=="PctPlayers"] <- "% Players"

#
# Shiny Server Function
#

shinyServer(
  function(input, output) {
    # Reactive Dataset
    dataSource <- reactive({
      switch(input$dataset,
             "Countries" = countries[,],
             "States (US)" = statesUS[,],
             "States (Ven.)" = statesVen[,],
             "States (D.R.)" = statesDR[,])
    })
    
    # Reactive for place column header
    place <- reactive({
      switch(input$dataset,
              
             "Countries" = "country",
             "state")
    })
    
    # Subset data based on user inputs
    my_subset_data <- reactive({        
      
      # Here check if the column names correspond to the dataset
      # States
      if( any(input$yvar %in% names(dataSource()))  &
          input$dataset %in% c("States (US)","States (Ven.)","States (D.R.)"))
      { 
        df <- subset(dataSource(), select = c("state", "Year",input$yvar))
        names(df) <- c("place","Year","y")
        return(df)
        # Countries
      }else if( any(input$yvar %in% names(dataSource()))  &
                input$dataset == "Countries")
      {
        df <- subset(dataSource(), select = c("country","Year", input$yvar))
        names(df) <- c("place","Year","y")
        return(df)
      }
      
    })
    
    # Y axis as character reactive
    yaxis <- reactive({
      as.character(input$yvar)
    })
    
    # Dynamically render the slider bars based on filtered data
    # Y axis range
    output$yrange2 <- renderUI({
      
        sliderInput("yrange", paste(input$yvar," Range :", sep=""),
                    round(min(my_subset_data()[, "y"]),0), 
                    round(max(my_subset_data()[, "y"]), 0),
                    value = range(my_subset_data()[, "y"]), 
                    step = 1)
      
      
      
    })
    # Year range
    output$xrange2 <- renderUI({
      
      sliderInput("xrange", paste("Year Range :", sep=""), sep="",
                  min(my_subset_data()[, "Year"]), 
                  max(my_subset_data()[, "Year"]),
                  value = c( 1960,2015), 
                  step = 1)
      
      
      
    })
    
    # Observe all reactives and make ggvis plot
    observe({
      datafull <- dataSource()
      data <- my_subset_data()
      pl <- place()
      
      # Function for HTML pop up on hover
      htmltext <- function(x){
        # Check if null
        if(is.null(x)) return(NULL)
        # subset to hover data
        row <- datafull[datafull[,pl] == x$place,  ]
        
        # HTML to output on hover
        paste(div( style = "background-color:white; text-align:center",
              # Headers
              h4(x$place)),
              strong(style = "text-decoration:underline; text-align:left","Career WAR Leaders :"),
              # Player list
              div(style = "text-align:left",
                  if(!is.na(row$name1[1])){
                    # If only 1 player
                    if(is.na(row$name2[1])){
                      p(style = "margin:0", strong("1 : "),paste(row$name1[1], " (",round(row$war1[1],1),")", sep=""))
                   # 2 player...
                       }else if(is.na(row$name3[1])){
                      div(p(style = "margin:0",strong("1 : "),paste(row$name1[1]," (", round(row$war1[1],1),")", sep="")),
                          p(style = "margin:0",strong("2 : "),paste(row$name2[1], " (",round(row$war2[1],1),")", sep="")))
                   # 3 player..
                          }else if(is.na(row$name4[1])){
                      div(p(style = "margin:0",strong("1 : "),paste(row$name1[1]," (", round(row$war1[1],1),")", sep="")),
                          p(style = "margin:0",strong("2 : "),paste(row$name2[1]," (", round(row$war2[1],1),")", sep="")),
                          p(style = "margin:0",strong("3 : "),paste(row$name3[1]," (", round(row$war3[1],1),")", sep="")))
                   # 4 player...
                             }else if(is.na(row$name5[1])){
                      div(p(style = "margin:0",strong("1 : "),paste(row$name1[1]," (", round(row$war1[1],1),")", sep="")),
                          p(style = "margin:0",strong("2 : "),paste(row$name2[1]," (", round(row$war2[1],1),")", sep="")),
                          p(style = "margin:0",strong("3 : "),paste(row$name3[1]," (", round(row$war3[1],1),")", sep="")),
                          p(style = "margin:0",strong("4 : "),paste(row$name4[1]," (", round(row$war4[1],1),")", sep="")))
                   # 5 player...
                                }else {
                      div(p(style = "margin:0",strong("1 : "),paste(row$name1[1]," (", round(row$war1[1],1),")", sep="")),
                          p(style = "margin:0",strong("2 : "),paste(row$name2[1]," (", round(row$war2[1],1),")", sep="")),
                          p(style = "margin:0",strong("3 : "),paste(row$name3[1]," (", round(row$war3[1],1),")", sep="")),
                          p(style = "margin:0",strong("4 : "),paste(row$name4[1]," (", round(row$war4[1],1),")", sep="")),
                          p(style = "margin:0",strong("5 : "),paste(row$name5[1]," (",round(row$war5[1],1),")", sep="")))
                    }
                  }
                  
                
              
              
              )
              
              )
      }
      
      # Test for null 
      if(!is.null(data) ){
        ## ggvis plot
        p <- data %>% 
          ggvis(~Year, ~y, stroke = ~place)%>%
          # Scales
          scale_numeric("y",clamp = TRUE, domain = c(input$yrange[1], input$yrange[2]) ) %>%
          scale_numeric("x",clamp = TRUE, domain = c(input$xrange[1], input$xrange[2]) ) %>%
         # lines
           layer_lines(opacity.hover:=1, opacity := 0.5,
                      strokeWidth.hover := 5, strokeWidth := 1.25,
                      stroke.hover= ~place, stroke:="grey") %>% 
          # Remove legend
          hide_legend("stroke")%>%
         # Axis
           add_axis("x", title="Year",  format="####",
                   properties = axis_props(
                     labels = list(  fontSize = 15),
                     title = list(  fontSize = 20)
                   ))%>%
          add_axis("y", title= yaxis(),title_offset = 50,
                   properties = axis_props(
                     labels = list(  fontSize = 15),
                     title = list(  fontSize = 20)
                   )) %>%
          # Tooltip and sizing
          add_tooltip(htmltext, "hover")%>%
          set_options(height = 500, width = 800, resizable = FALSE)
          
         
        
        
        
        # Send to shiny
        p  %>%bind_shiny("ggvis")
      }
    })
    
    
    
    
  }
)


