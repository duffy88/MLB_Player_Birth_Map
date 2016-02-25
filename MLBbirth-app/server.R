# Server for MLB Birthplace info interactive map 
# 
# By Doug Duffy 2016

# Load Packages
library(shiny)
library(leaflet)

# Load prepared dataset
birthlat <- readRDS("data/BirthMap(Total).rds")

# Label players without teams as free agents (this is only a handful of 
# free agents heading into 2016 with >0 WAR projected).
# These players cause a funky error where data points dont 
# get removed properly when switching layers
birthlat$Tm[is.na(birthlat$Tm) ] <- "FA"
birthlat$color[is.na(birthlat$Tm) ] <- "black"

# Define vector of team IDs
teams  <- c("CHW","SFG","SEA","ATL","NYM","BOS","NYY","CHC","OAK","CIN","WSN",
            "HOU","FLA","TBD","KCR","MIN","PHI","LAA","LAD","ARI","TBR","BAL",
            "CLE","TOR","COL","MIL","STL","DET","TEX","SDP","PIT","MON","MIA",
            "ANA","CAL","KCA","BRO","MLN","WSA","SEP","PHA","WSH","NYG","BSN",
            "SLB","WHS","SYR","CLV","LOU","PRO","HAR","SLM","BLN", "MLA",
            "IND","BLA","KCN","DTN","NYU","WOR","MLG","TRO","BUF","ATH","FA")
mycolors <- c()

# Make teams an ordered factor by our vector list
birthlat$Tm <- ordered(birthlat$Tm)
birthlat$Tm <- ordered(birthlat$Tm, levels = teams)

# Set colors vector for legend
for(i in 1:length(teams)){
  mycolors[i] <- switch(teams[i],
                        "CHW"= "#190707",
                        "SFG" = "#FF4000",
                        "SEA" = "#086A87",
                        "ATL" = "#BF0013",
                        "NYM" = "#EF9700",
                        "BOS" = "#E02626",
                        "NYY" = "#0F1140",
                        "CHC" = "#1259FF",
                        "OAK" = "#1C641B",
                        "CIN" = "#FF242F",
                        "WSN" = "#D20812",
                        "HOU" = "#E27929",
                        "FLA" = "#44E0F2",
                        "TBD" = "#1D2D7E",
                        "KCR" ="#0B26A9",
                        "MIN" = "#B2202A",
                        "PHI" = "#93000A",
                        "LAA" ="#D61A27",
                        "LAD" ="#3036F2",
                        "ARI" = "#AF00FF",
                        "TBR" ="#1F3085",
                        "BAL" = "#FF4400",
                        "CLE" = "#E33131",
                        "TOR" = "#469CC7",
                        "COL" = "#500380",
                        "MIL" = "#E3D000",
                        "STL" = "#FF0303",
                        "DET" = "#FF8324",
                        "TEX" = "#0E2BCF",
                        "SDP" = "#F0DA6F",
                        "PIT" = "#FFF700",
                        "MON" = "#8CDFDC",
                        "MIA" = "#06FCF4",
                        "ANA" ="#D71724",
                        "CAL" ="#D21925",
                        "KCA" = "#1D8305",
                        "BRO" = "#00529C",
                        "MLN" = "#CD0931",
                        "WSA" = "#C80A2E",
                        "SEP" = "#FFD451",
                        "PHA" = "#0033A0",
                        "WSH"= "#227AD4",
                        "NYG" = "#EF4000",
                        "BSN" = "#C60E2C",
                        "SLB" = "#5C2B2E",
                        "WHS"="#A06115",
                        "SYR" = "#4C008C",
                        "CLV" ="#383038",
                        "LOU" ="#F8FB05",
                        "PRO" ="#454545",
                        "HAR" ="#01018B",
                        "SLM" ="#840007",
                        "BLN" = "#010102",
                        "MLA" = "#002142",
                        "IND" = "#FEFD0C",
                        "BLA" ="#000002",
                        "KCN" = "#664225",
                        "DTN"="#314F49",
                        "NYU" = "#000178",
                        "WOR" = "#E0125F",
                        "MLG" = "#464646",
                        "TRO" ="#485421",
                        "BUF" = "#8A805E",
                        "ATH" = "#33603E",
                        "FA" = "black")
}

# Define teams in AL for filter
AL <- c("CHW","SEA","BOS","NYY","OAK",
        "HOU","TBD","KCR","MIN","LAA","TBR","BAL",
        "CLE","TOR","DET","TEX",
        "ANA","CAL","KCA","WSA","SEP","PHA","WSH","SLB","MLA","BLA")

#
# Shiny Server Function
#

server <- function(input, output, session) {
  
  # Ensure input year gets treated as numeric despite possible string
  inputYear <- reactive({
    if(input$year == "2016 (Proj.)"){
      return(2016)
    }else {
      return(as.integer(input$year))
    }
  })
  
  output$refs <- renderUI({
    # 2016 References includes Steamer
    if(inputYear()==2016){
      absolutePanel(top = 432, left = 815, width = 200, 
                    div(style="background:white; opacity:0.7",
                        a( href = "http://dougduffy.com/",target = "_blank",
                           img(src = "Logo.png", width = "40px", height= "35px")),
                        a( href = "http://dougduffy.com/",target = "_blank",
                           img(src = "Header.png",width = "155px", height= "35px")),
                    # Div for main styling
                    div(style = "text-align:right; background-color:white; 
                        opacity:0.7;font-size: 12px;padding-right:4px", 
                        div(style = "text-align:right; background-color:white; opacity:0.7;
                            font-size: 12px;padding-right:4px", 
                            "Source :", 
                            a( href = "http://www.baseball-reference.com/",
                               style ="text-decoration:none",target = "_blank",
                               "Baseball-Ref"),
                            " , ",
                            a( href = "http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer&team=0&lg=all&players=0",
                               style ="text-decoration:none",target = "_blank",
                               "Steamer")
                            ))
      )
      )                
      
      # Non 2016 References, just baseball ref
    }else {
      absolutePanel(top = 432, left = 815, width = 200,
       div(style="background:white; opacity:0.7",
          a( href = "http://dougduffy.com/",target = "_blank",
             img(src = "Logo.png", width = "40px", height= "35px")),
          a( href = "http://dougduffy.com/",target = "_blank",
             img(src = "Header.png",width = "155px", height= "35px")),
                    # Div for main styling
                   
                        div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:4px", 
                            "Source :", 
                            a( href = "http://www.baseball-reference.com/",
                               style ="text-decoration:none",target = "_blank",
                               "Baseball-Reference"))
      )
      )
    }
  })
  
  # First filtering of data to only desired year
  filteredData <- reactive({
     
      df <- birthlat[birthlat$Year == inputYear() & 
                       !is.na(birthlat$lon) ,]
      return(df)
    
    
    
  })
  
  # Dynamically render the team selction list based on teams active in input year
  output$team2 <- renderUI({
    
    selectInput("team2", paste("Team :", sep=""),
                
                # Modern Era has both AL and NL
                choices = if(inputYear() %in% 1901:2016){
                  c("All Teams","AL Teams","NL Teams",
                    as.character(unique(filteredData()$Tm))[order(as.character(unique(filteredData()$Tm)))])
                
                  # Pre-modern era only has NL
                  }else {
                  c("All Teams",
                    as.character(unique(filteredData()$Tm))[order(as.character(unique(filteredData()$Tm)))])
                }, 
                selected = "All Teams"
    )
  })
  
  # Second filtering of data based on input team selected
  filteredData2 <- reactive({
    # Check if team input null
    if(!is.null(input$team2) ){
      # Make sure AL isn't the selected option when it doesn't exist
      # and make sure other teams are active in the year selected
      if( !(input$team2=="AL Teams" & as.integer(inputYear()) < 1901) &
          (input$team2 %in% filteredData()$Tm | input$team2 %in% c("All Teams","AL Teams","NL Teams"))){
        # All teams input
        if(input$team2== "All Teams"){
          df <- filteredData()
          df <- df[order(df$ptsize, decreasing=TRUE),]
          return(df)
        # AL Teams
        }else if(input$team2== "AL Teams"){
          df <- filteredData()[filteredData()$Tm %in% AL,]
          df <- df[order(df$ptsize, decreasing=TRUE),]
          return(df)
        # NL Teams
        }else if(input$team2== "NL Teams"){
          df <- filteredData()[!(filteredData()$Tm %in% AL),]
          df <- df[order(df$ptsize, decreasing=TRUE),]
          return(df)
        # Single team selected
        }else { 
          df <- filteredData()[filteredData()$Tm ==input$team2,]
          df <- df[order(df$ptsize, decreasing=TRUE),]
          return(df)
        }
      }
    }
  })
  
  # Dynamically render the slider bars based on filtered data # 2
  # Yearly WAR
  output$war2 <- renderUI({
    sliderInput("war", paste(inputYear()," WAR :", sep=""),
                min(filteredData2()$WAR), max(filteredData2()$WAR),
                value = range(c(1,max(filteredData2()$WAR))), step =0.1
    )
  })
  # Career WAR
  output$cwar2 <- renderUI({
    sliderInput("cwar", paste("Career WAR :", sep=""),
                min(filteredData2()$careerWAR), max(filteredData2()$careerWAR),
                value = range(filteredData2()$careerWAR), step =0.1
    )
  })
  
  # Dynamically render header for the point size legend
  # Need two, as the ui.R function requires two conditionalPanel() calls 
  # in order to allow for two posibble positions
  output$ptlegend <- renderUI({
    h5(style = "text-align:center;margin:0",
       strong(paste(inputYear()," WAR", sep="")))
  })
  output$ptlegend2 <- renderUI({
    h5(style = "text-align:center;margin:0",
       strong(paste(inputYear()," WAR", sep="")))
  })
  
  # Build main body of leaflet map, basic tiles only, points added later
  output$map <- renderLeaflet({

    leaflet(birthlat) %>% addProviderTiles("CartoDB.Positron")
  })
  
  # Third filtering of data based on inputs of slider bars
  filteredData3 <- reactive({
    if(!is.null(filteredData2())){
      df <- filteredData2()[filteredData2()$WAR >= input$war[1] &
                              filteredData2()$WAR <= input$war[2] &
                              filteredData2()$careerWAR >= input$cwar[1] &
                              filteredData2()$careerWAR <= input$cwar[2],]
      return(df)
    }
    
  })
  
  # Observe reset view button
  observe({
    input$resetview
    if(!is.null(filteredData3())){
      leafletProxy("map", data = filteredData3())  %>%
        fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    }
    
  })
  
  # Observe legend
  observe({
    if(!is.null(filteredData3())){
      proxy <- leafletProxy("map", data = filteredData3())
      
      # Remove any existing legend, and only if the legend is
      # enabled and multiple teams plotted, create a new one.
      proxy %>% clearControls()
      if (input$legend & (input$team2 %in% c("All Teams","AL Teams","NL Teams"))) {
        
        
        proxy %>% addLegend("bottomleft", 
                            colors= ~mycolors[mycolors %in% filteredData3()$color], 
                            labels= ~teams[teams %in% filteredData3()$Tm], 
                            title="MLB Team",
                            opacity =1) 
      }
    }
    
  })
  
  # Observe the addition and removal of points based on filtered data #3
  observe({
    
    data <- filteredData3()
    
    # Remove points if null
    if(is.null(data)  ){ 
      leafletProxy("map", data = data) %>% clearMarkers()
    # If not null remove points and add new points
    }else {
      
      map2 <- leafletProxy("map", data = data) %>%
        clearMarkers() 
      
      if(nrow(data)>0){
        map2 %>% addCircleMarkers(radius = ~ptsize,  stroke = NA, color = ~color,
                                 fillOpacity = 0.5, layerId = ~bref_id ,
                                 
                                 # HTML to display on click
                                 popup = ~paste(
                                   paste("<div style = 'text-align:center'><img src='",pic,"' width = '75' />", sep= ""),
                                  
                                    # Switching between which website to link to base on year
                                   paste("<a href='", 
                                         link,
                                         "' style = 'color: black;' target = '_blank'><h5 style = 'padding:0'>",
                                         name,
                                         "</h5></a></div>", 
                                         sep=""
                                   ),
                                   paste("<div style = 'text-align:left'><p style=' margin: 0;'><span style = 'font-weight:bold;'> Birthday : </span>",
                                         bday,
                                         
                                         # If alive show age
                                         if( is.na(deathday) ){
                                           paste("<span style = 'font-weight:bold;'> Age : </span>",
                                                 age,
                                                 "</p>")
                                         }else {
                                           
                                         }
                                   ),
                                   paste("<p style=' margin-top: 0; margin-bottom:",
                                         
                                         # Formatting change if alive
                                         if(is.na(deathplace)){
                                           "7px"
                                         }else {
                                           "0"
                                         },
                                         "'><span style = 'font-weight:bold;'> Birthplace : </span>",
                                         birthplace, 
                                         "</p>", 
                                         
                                         # If dead, show death date
                                         if(!is.na(deathplace)){
                                           paste("<p style=' margin-top: 0; margin-bottom:7px'><span style = 'font-weight:bold;'> Died : </span>",
                                                 deathday, 
                                                 "</p>", 
                                                 sep="")
                                         }
                                   ),
                                   paste("<p style=' margin: 0;'><span style = 'font-weight:bold;'>Height :</span>",
                                         Height,
                                         "<span style = 'font-weight:bold;'>Weight :</span>",
                                         Weight, "</p>"),
                                   paste("<p style=' margin: 0;'><span style = 'font-weight:bold;'>Bat :</span>",
                                         substr(BatHand,1,1),
                                         "<span style = 'font-weight:bold;'>Throws :</span>",
                                         substr(ThrowHand,1,1), 
                                         "</p>"),
                                   paste("<p style=' margin-top: 0; margin-bottom:7px'><span style = 'font-weight:bold;'>Position :</span>",
                                         Pos, 
                                         "</p>"),
                                   paste("<p style=' margin: 0;'><span style = 'font-weight:bold;'>", 
                                         inputYear(),
                                         " Team : </span>",
                                         Tm, 
                                         "</p>"),
                                   paste("<p style=' margin: 0;'><span style = 'font-weight:bold;'>",
                                         inputYear(),
                                         " WAR : </span>",
                                         WAR, 
                                         "</p></div>", 
                                         sep=""), 
                                   paste("<p style=' margin: 0;'><span style = 'font-weight:bold;'> Career WAR : </span>",
                                         round(careerWAR, digits=1), 
                                         "</p></div>", 
                                         sep=""),
                                   sep ="")) %>%
          fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
      }
      
        
    }
    
  })
  
  # Add highlighting around point being clicked
  observe({
    click <- input$map_marker_click
    if(!is.null(click)  ){
      # Identify point clicked
      hl <- subset(filteredData3(), bref_id==click$id)
      # Remove previous highlighting
      lp <- leafletProxy("map", data = hl) %>% 
        removeMarker( layerId = "add") 
      
      # Add new highlighting only if there is a point to add
      if(nrow(hl) >0){
        lp %>% addCircleMarkers(radius = ~ptsize,  stroke = TRUE, color = ~color,weight=4,
                                fillOpacity = 0,opacity=1, layerId = "add")
      }
      
    }
    
  })
  
}
