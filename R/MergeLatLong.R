# Merge location info with cleaned play biographic data and perform
# final cleaning to prepare the dataset for the map visualization.
#
# Warning!!! :: The third for loop reults in a long run time.

# Load packages
library(plyr)
# Load cleaned player biographic data
cbirthinfo1 <- readRDS("data/CleanBirthInfo/CleanMLBPlayerBirthInfo(40to69).rds")
cbirthinfo2 <- readRDS("data/CleanBirthInfo/CleanMLBPlayerBirthInfo(Post1970).rds")
cbirthinfo3 <- readRDS("data/CleanBirthInfo/CleanMLBPlayerBirthInfo(1900to39).rds")
cbirthinfo4 <- readRDS("data/CleanBirthInfo/CleanMLBPlayerBirthInfo(Bef1900).rds")
cbirthinfo5 <- readRDS("data/2016/2016CleanMLBPlayerBirthInfo.rds")
# Fix KC team name
for(i in 1:nrow(cbirthinfo5)){
  if(cbirthinfo5$teamID[i]=="KCA"){
    cbirthinfo5$teamID[i] <- "KCR"
  }
}
# Fix col name
names(cbirthinfo5)[names(cbirthinfo5)=="teamID"] <- "Tm"
# Combine into one dataframe
cbirthinfo <- rbind(cbirthinfo1, cbirthinfo2, cbirthinfo3, cbirthinfo4, cbirthinfo5)

# Load latitude and and longitude info
temp1 <- read.csv("data/BirthLoc/birthloc(40to69-batch1).csv", stringsAsFactors = F)
temp2 <- read.csv("data/BirthLoc/birthloc(40to69-batch2).csv", stringsAsFactors = F)
temp3 <- read.csv("data/BirthLoc/birthloc(Post1970-batch1)new.csv", stringsAsFactors = F)
temp4 <- read.csv("data/BirthLoc/birthloc(Post1970-batch2)new.csv", stringsAsFactors = F)
temp5 <- read.csv("data/BirthLoc/birthloc(1900to39-batch1).csv", stringsAsFactors = F)
temp6 <- read.csv("data/BirthLoc/birthloc(1900to39-batch2).csv", stringsAsFactors = F)
temp7 <- read.csv("data/BirthLoc/birthloc(Bef1900-batch1).csv", stringsAsFactors = F)
temp8 <- read.csv("data/BirthLoc/birthloc(Bef1900-batch2).csv", stringsAsFactors = F)
temp9 <- read.csv("data/BirthLoc/birthloc(2016-batch1).csv", stringsAsFactors = F)
# Combine into one dataframe
# 2016 treated differently as some players don't have MLB ID's yet
temp <- rbind(temp1, temp2,temp3,temp4,temp5, temp6, temp7,temp8)
temp2016 <- temp9
# Merge location and bio data together
cbirthinfo <- merge(cbirthinfo, temp, by = "bref_id_mlb", all.x=TRUE)
cbirthinfo <- merge(cbirthinfo, temp2016, by = "bref_id", all.x=T)
# if only have location data from 2016 add it in
for(i in 1:nrow(cbirthinfo)){
  if(is.na(cbirthinfo$lon.x[i]) &
     !is.na(cbirthinfo$lon.y[i])){
    cbirthinfo$lon.x[i] <- cbirthinfo$lon.y[i]
    cbirthinfo$lat.x[i] <- cbirthinfo$lat.y[i]
    
  }
  
}
# Remove now unused columns and fix names
cbirthinfo$lon.y <- cbirthinfo$lat.y <- NULL
names(cbirthinfo)[names(cbirthinfo)=="lon.x"] <- "lon"
names(cbirthinfo)[names(cbirthinfo)=="lat.x"] <- "lat"

# Define point size based on WAR
# All players below 0 WAR get 3, if above point size
cbirthinfo$ptsize <- ifelse(cbirthinfo$WAR >= 0 & !is.na(cbirthinfo$WAR), 
                          3+(2*(cbirthinfo$WAR)),3 )

# Define vector of teams
teams  <- c("CHW","SFG","SEA","ATL","NYM","BOS","NYY","CHC","OAK","CIN","WSN",
            "HOU","FLA","TBD","KCR","MIN","PHI","LAA","LAD","ARI","TBR","BAL",
            "CLE","TOR","COL","MIL","STL","DET","TEX","SDP","PIT","MON","MIA",
            "ANA","CAL","KCA","BRO","MLN","WSA","SEP","PHA","WSH","NYG","BSN",
            "SLB","WHS","SYR","CLV","LOU","PRO","HAR","SLM","BLN", "MLA",
            "IND","BLA","KCN","DTN","NYU","WOR","MLG","TRO","BUF","ATH")
mycolors <- c()

# Make teams ordered factor
cbirthinfo$Tm <- ordered(cbirthinfo$Tm)
cbirthinfo$Tm <- ordered(cbirthinfo$Tm, levels = teams)

# Define color for each team
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
                              "plum2")
}


# Clean position labels to abbreviation and remove pinch hitter and other nonsense
cbirthinfo$Pos <- gsub("Pitcher","P", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("First Baseman","1B", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Rightfielder","RF", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Outfielder","OF", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Centerfielder","CF", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Second Baseman","2B", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Third Baseman","3B", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Leftfielder","LF", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Shortstop","SS", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Designated Hitter","DH", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Catcher","C", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Pinch Runner, "," ", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Pinch Runner and "," ", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Pinch Hitter, "," ", cbirthinfo$Pos)
cbirthinfo$Pos <- gsub("Pinch Hitter and "," ", cbirthinfo$Pos)

# Create progress bar
pb <- txtProgressBar(min = 1, max = nrow(cbirthinfo), style = 3)

# Long For loop defining color for each player season 
# In addition, certain players with erroneous Google location info are correted
cbirthinfo$color <- NA
for(i in 1:nrow(cbirthinfo)){
  # Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  # Define color
  cbirthinfo$color[i] <- mycolors[grepl(cbirthinfo$Tm[i], teams)]
  
  # If only 2 Pos listed replace , with and 
  # Occurs cause of removal of pinch hitter
  if(nchar(cbirthinfo$Pos[i]) <=7){
    cbirthinfo$Pos[i] <- gsub(","," and", cbirthinfo$Pos[i])
    
  }
  
  # Check if NA
  if(!is.na(cbirthinfo$bref_id_mlb[i])){
    
    # Google geocode error fixing Salem, Indiana vs Salem, India,
    # poorly translated Korean names, etc.
    if(cbirthinfo$bref_id_mlb[i]=="choji01"){
      cbirthinfo$lon[i] <- 127.131598
      cbirthinfo$lat[i] <- 35.830073
    }
    if(cbirthinfo$bref_id_mlb[i]=="mitchbr01"){
      cbirthinfo$lon[i] <- -79.6750
      cbirthinfo$lat[i] <- 36.3453
    }
    if(cbirthinfo$bref_id_mlb[i]=="sanchis01"){
      cbirthinfo$lon[i] <- -79.764159
      cbirthinfo$lat[i] <- 22.356748
    }
    if(cbirthinfo$bref_id_mlb[i]=="hernaja01"){
      cbirthinfo$lon[i] <- -80.966667
      cbirthinfo$lat[i] <- 22.766667
    }
    if(cbirthinfo$bref_id_mlb[i]=="alexado01"){
      cbirthinfo$lon[i] <- -87.184278
      cbirthinfo$lat[i] <- 33.761093
    }
    if(cbirthinfo$bref_id_mlb[i]=="pennibr01"){
      cbirthinfo$lon[i] <- -86.100593
      cbirthinfo$lat[i] <- 38.608614
    }
    if(cbirthinfo$bref_id_mlb[i]=="gomezpr01"){
      cbirthinfo$lon[i] <- -75.650078
      cbirthinfo$lat[i] <- 20.757496
    }
    if(cbirthinfo$bref_id_mlb[i]=="gonzato01"){
      cbirthinfo$lon[i] <-  -78.338476
      cbirthinfo$lat[i] <- 22.085368
    }
    if(cbirthinfo$bref_id_mlb[i]=="finnelo01"){
      cbirthinfo$lon[i] <-  -85.401689
      cbirthinfo$lat[i] <- 32.946103
    }
    if(cbirthinfo$bref_id_mlb[i]=="tayloto02"){
      cbirthinfo$lon[i] <-  -80.815037
      cbirthinfo$lat[i] <- 22.797714
    }
    if(cbirthinfo$bref_id_mlb[i]=="flemibi01"){
      cbirthinfo$lon[i] <-  -117.888576
      cbirthinfo$lat[i] <- 33.981000
    }
    if(cbirthinfo$bref_id_mlb[i]=="goletst01"){
      cbirthinfo$lon[i] <-  -80.859534
      cbirthinfo$lat[i] <- 40.120078
    }
    if(cbirthinfo$bref_id_mlb[i]=="boninlu01"){
      cbirthinfo$lon[i] <-  -87.110808
      cbirthinfo$lat[i] <- 40.409770
    }
    if(cbirthinfo$bref_id_mlb[i]=="judsoho01"){
      cbirthinfo$lon[i] <-  -88.432260
      cbirthinfo$lat[i] <- 42.472071
    }
    if(cbirthinfo$bref_id_mlb[i]=="nichoov01"){
      cbirthinfo$lon[i] <-  -86.100765
      cbirthinfo$lat[i] <- 38.608212
    }
    if(cbirthinfo$bref_id_mlb[i]=="richabi01"){
      cbirthinfo$lon[i] <-  -86.100765
      cbirthinfo$lat[i] <- 38.608212
    }
    if(cbirthinfo$bref_id_mlb[i]=="garciro01"){
      cbirthinfo$lon[i] <-  -67.591680
      cbirthinfo$lat[i] <- 10.244035
    }
    
    # Check fixes below here: 
    
    
    
  }
  
  # Fix a couple players w/o birthplace info and w/o MLB ID's
  if(is.na(cbirthinfo$bref_id_mlb[i])){
    
    if(cbirthinfo$bref_id[i]=="kim---004hye"){ # Hyeon-soo Kim
      cbirthinfo$birthplace[i] <-"Seoul, South Korea"
      cbirthinfo$lon[i] <-  127.000148
      cbirthinfo$lat[i] <- 37.537005
    }
    if(cbirthinfo$bref_id[i]=="park--000byu"){ # Byung-ho Park
      cbirthinfo$birthplace[i] <-"Buan, South Korea"
      cbirthinfo$lon[i] <-  126.734388
      cbirthinfo$lat[i] <- 35.725140
    }
  }
  
}
# Close progress bar
close(pb)
# Make color ordered factor 
cbirthinfo$color <- ordered(cbirthinfo$color)
# Calculate Age
cbirthinfo$age <- 2015-as.numeric(substr(cbirthinfo$bday, 
                                       (nchar(cbirthinfo$bday)-4),
                                       nchar(cbirthinfo$bday)))
# If WAR is NA set to 0
cbirthinfo$WAR[is.na(cbirthinfo$WAR) ] <- 0

# Split into MLB and non-MLB players
cbirthinfoMlb <- cbirthinfo[!is.na(cbirthinfo$bref_id_mlb), ]
cbirthinfoNonMlb <- cbirthinfo[ is.na(cbirthinfo$bref_id_mlb), ]
# Calculate player career WAR, expluding 2016 projected data
test <- ddply(subset(cbirthinfoMlb, Year!=2016), .(bref_id_mlb), summarize,
              careerWAR = sum(WAR))
cbirthinfoMlb <- merge(cbirthinfoMlb, test, by="bref_id_mlb")

# Set non-MLB player career WAR to 0
cbirthinfoNonMlb$careerWAR <- 0
# Re-combine MLB and nonMLB players
cbirthinfo <- rbind(cbirthinfoMlb, cbirthinfoNonMlb)

# Create NA column for city, state, country
cbirthinfo$country <- cbirthinfo$state <- cbirthinfo$city <- NA
# String cleaning of birth place into city, state, country columns
for(i in 1:nrow(cbirthinfo)){
  cbirthinfo$temp[i] <- strsplit(cbirthinfo$birthplace[i], ", ")
  # USA not listed by Baseball ref, just city, State
  if(length(cbirthinfo$temp[[i]])== 2){
    cbirthinfo$country[i] <- "United States"
    cbirthinfo$state[i] <-cbirthinfo$temp[[i]][2]
    cbirthinfo$city[i] <-cbirthinfo$temp[[i]][1]
  }else if(length(cbirthinfo$temp[[i]])== 3){
    cbirthinfo$country[i] <- cbirthinfo$temp[[i]][3]
    cbirthinfo$state[i] <-cbirthinfo$temp[[i]][2]
    cbirthinfo$city[i] <-cbirthinfo$temp[[i]][1]
  }
} 
# Remove temp column
cbirthinfo$temp <- NULL

# Define states and include territories Puerto Rico, Guam and Wash, DC
states <- c("MI","CA","IL","MA","Puerto Rico","GA","HI","NC","NJ","OR",
            "TX","AL","AZ","FL","MD","MN","MO","NY","OH","OK","VA","WA",
            "DE","IA","KS","KY","LA","ND","SC","TN","AR","CT","ID","IN",
            "MS","MT","NV","PA","WI","CO","NE","NM","NH","RI","SD","WV",
            "UT","AK","DC","ME","WY","VT","Guam")

# Correct country info for Virgin Islands and American Samoa
for(i in 1:nrow(cbirthinfo)){
  
  # Check if NA
  if(!(is.na(cbirthinfo$state[i])) & !(is.na(cbirthinfo$country[i])) ){
    # Corrects players mislabeled as from USA
    if(!(cbirthinfo$state[i] %in% states) & 
       cbirthinfo$country[i] =="United States"){
      cbirthinfo$country[i] <- cbirthinfo$state[i]
      cbirthinfo$state[i] <- NA
    }
    # Corrects one wrong spelling of Zulia
    if(cbirthinfo$country[i] =="Veneuela" ){
      if(cbirthinfo$state[i] == "Zuila"){
        cbirthinfo$state[i] <- "Zulia"
      }
      
    }
    # Fix US Virgin Islands
    if(!(is.na(cbirthinfo$state[i]))){
      if(cbirthinfo$country[i] =="U. S. Virgin Islands" ){
        
        if(cbirthinfo$state[i] =="St. Croix" ){
          cbirthinfo$city[i] <- cbirthinfo$state[i]
        }
        
        cbirthinfo$state[i] <- "Virgin Islands"
        cbirthinfo$country[i] <- "United States"
      }
    }
    
  }
  if(!(is.na(cbirthinfo$country[i]))){
    if(is.na(cbirthinfo$state[i]) & 
       cbirthinfo$country[i]=="U. S. Virgin Islands"){
      
      cbirthinfo$state[i] <- "Virgin Islands"
      cbirthinfo$country[i] <- "United States"
      
    }
    # Fix American Samoa
    if(is.na(cbirthinfo$state[i]) & 
       cbirthinfo$country[i]=="American Samoa"){
      
      cbirthinfo$state[i] <- "American Samoa"
      cbirthinfo$country[i] <- "United States"
      
    }
  }
  
}
# Fix San Fernando, CA returning a place in Spain
for(i in 1:nrow(cbirthinfo)){
  if(!(is.na(cbirthinfo$city[i])) & !(is.na(cbirthinfo$country[i]))){
    if(cbirthinfo$city[i]==" San Fernando"){
      cbirthinfo$lon[i] <- -118.433757
      cbirthinfo$lat[i] <- 34.289316
    }
    # Fix state names from abreviations to full
    if(cbirthinfo$country[i]=="United States"){
      cbirthinfo$state[i] <- switch(cbirthinfo$state[i],
                                    "MI"= "Michigan",
                                    "CA"= "California",
                                    "IL" = "Illinois",
                                    "MA"= "Massachusetts",
                                    "Puerto Rico" = "Puerto Rico",
                                    "GA" = "Georgia",
                                    "HI" = "Hawaii",
                                    "NC"= "North Carolina",
                                    "NJ" = "New Jersey",
                                    "OR" = "Oregon",
                                    "TX" = "Texas",
                                    "AL" = "Alabama",
                                    "AZ" = "Arizona",
                                    "FL" = "Florida",
                                    "MD" = "Maryland",
                                    "MN" = "Minnesota",
                                    "MO" = "Missouri",
                                    "NY" = "New York",
                                    "OH" = "Ohio",
                                    "OK" = "Oklahoma",
                                    "VA"= "Virginia",
                                    "WA"= "Washington",
                                    "DE"= "Delaware",
                                    "IA" = "Iowa",
                                    "KS" ="Kansas",
                                    "KY" ="Kentucky",
                                    "LA" ="Louisiana",
                                    "ND" = "North Dakota",
                                    "SC" = "South Carolina",
                                    "TN" = "Tennessee",
                                    "AR" = "Arkansas",
                                    "CT" = "Conneticut",
                                    "ID" = "Idaho",
                                    "IN" = "Indiana",
                                    "MS" = "Mississippi",
                                    "MT" = "Montana",
                                    "NV" = "Nevada",
                                    "PA" = "Pennsylvania",
                                    "WI" = "Wisconsin",
                                    "CO" = "Colorado",
                                    "NE" = "Nebraska",
                                    "NM" = "New Mexico",
                                    "NH" = "New Hampshire",
                                    "RI" = "Rhode Island",
                                    "SD" = "South Dakota",
                                    "WV" = "West Virginia",
                                    "UT" = "Utah",
                                    "AK" = "Alaska",
                                    "DC" = "Washington, DC",
                                    "ME" = "Maine",
                                    "WY" = "Wyoming",
                                    "VT" = "Vermont",
                                    "Guam" = "Guam",
                                    "Virgin Islands"= "Virgin Islands",
                                    "American Samoa" = "American Samoa")
    }
  }
  
}
# Define empy column for player website link
cbirthinfo$link <- NA
# Define player link
for(i in 1:nrow(cbirthinfo)){
  # Minor league players
  if(is.na(cbirthinfo$bref_id_mlb[i])){
    cbirthinfo$link[i] <- paste("http://www.baseball-reference.com/register/player.cgi?id=",
                                cbirthinfo$bref_id[i],
                                sep="")
    # MLB players
  }else {
    cbirthinfo$link[i] <- paste("http://www.baseball-reference.com/players/",
                                substr(cbirthinfo$bref_id_mlb[i],1,1),"/",cbirthinfo$bref_id_mlb[i],
                                ".shtml",
                                sep="")
  }
}
# Save to file and to Map app's file
saveRDS(cbirthinfo, "MLBbirth-app/data/BirthMap(Total).rds")
saveRDS(cbirthinfo, "data/BirthMap/BirthMap(Total).rds")

