# Calculates the count and total WAR accumulated by countries and states 
# (US, Ven, Dominican Republic)  over time. 
# In addition calculated %'s and top players by birth location


# Load Paackges
library(plyr)

# Load birth map data
temp <- readRDS("data/BirthMap/BirthMap(Total).rds")

# Remove 2016 projected data
temp  <- temp[ temp$Year!= 2016,]

# Remove John Magner, no listed birthplace, born way back in 1855
temp <- subset(temp, bref_id_mlb!="magnejo01")

# Define Venezuelan states
VenStates <- c("Amazonas","Anzoategui", "Apure","Aragua","Barinas", "Bolivar",
               "Carabobo","Cojedes","Delta Amacuro","Falcon","Guarico","Lara",
               "Merida","Miranda","Monagas","Nueva Esparta","Portuguesa",
               "Sucre","Tachira","Trujillo","Vargas","Yaracuy","Zulia")
# Define USA states, with territories and DC
UsStates <- c("Alabama","Alaska","American Samoa","Arizona","Arkansas","California",
              "Colorado","Conneticut","Delaware","Florida","Georgia","Guam",
              "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
              "Louisiana","Maine","Maryland","Massachusetts","Michigan",
              "Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
              "New Hampshire","New Jersey","New Mexico","New York","North Carolina",
              "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
              "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
              "Vermont","Virgin Islands","Virginia","Washington","Washington, DC",
              "West Virginia","Wisconsin","Wyoming")
# Define DR states
DrStates <- c("Azua","Barahona","Dajabon","Distrito Nacional","Duarte","El Seibo",
              "Elias Pina","Espaillat","Hato Mayor","Hermanas Mirabal","La Altagracia",
              "La Romana","La Vega","Maria Trinidad Sanchez","Monsenor Nouel",
              "Monte Cristi","Monte Plata","Peravia","Puerto Plata","Samana","San Cristobal",
              "San Juan","San Pedro de Macoris","Sanchez Ramirez","Santiago","Santiago Rodriguez",
              "Santo Domingo","Valverde")
# Define countires (only those with actual players from them)
CountriesV <- c( "Afghanistan","Aruba","Australia","Austria","Bahamas","Belgium",
                "Belize","Brazil","Canada","Colombia","Cuba","Curacao","Czech Republic",
                "Denmark","Dominican Republic","France","Germany","Greece",
                "Honduras","Indonesia","Ireland","Italy","Jamaica","Japan","Lithuania","Mexico",
                "Netherlands","Nicaragua","Norway","Panama","Philippines","Poland",
                "Russian Federation","Saudi Arabia","Singapore","Slovakia","South Korea",
                "Spain","Sweden","Switzerland","Taiwan","United Kingdom",
                "United States","Venezuela","Viet Nam")

# Remove space that appears first in city names
temp$city <- gsub(" ","", temp$city)
# Slight corrections to some city/state names
temp$city <- gsub("NuevaEsparta","Nueva Esparta", temp$city)
temp$state <- gsub("Zuila", "Zulia", temp$state)

# Check states and countries
for(i in 1:nrow(temp)){
  # Fix Venezuelan state problems
  if(!is.na(temp$country[i])){
    if(temp$country[i] == "Venezuela"){
      if(is.na(temp$state[i]) &
         temp$city[i] %in% VenStates){
        temp$state[i] <- temp$city[i]
      }
      if(is.na(temp$state[i]) &
         temp$city[i] == "SurEdoAragua"){
        temp$state[i] <- "Aragua"
      }
    }
  }
  
  # Fix DR state problems
  if(!is.na(temp$country[i])){
    if(temp$country[i] == "Dominican Republic"){
      if(is.na(temp$state[i]) &
         temp$city[i] %in% DrStates){
        temp$state[i] <- temp$city[i]
      }
      if(is.na(temp$state[i]) &
         temp$city[i] == "ElSeibo"){
        temp$state[i] <- "El Seibo"
      }
      if(is.na(temp$state[i]) &
         temp$city[i] == "Palenque"){
        temp$state[i] <- "San Cristobal"
        
      }
    }
  }
  
  
  # Fix players with only a country listed in birthplace
  if(is.na(temp$country[i])){
    if(!is.na(temp$birthplace[i])){
      if(substr(temp$birthplace[i],1,1)==" "){
        temp$birthplace[i] <- substr(temp$birthplace[i] ,2,nchar(temp$birthplace[i]))
      }
      if(temp$birthplace[i] %in% UsStates){
        temp$country[i] <- "United States"
        temp$state[i] <- temp$birthplace[i]
      }
      if(temp$birthplace[i] %in% CountriesV){
        temp$country[i] <- temp$birthplace[i]
      }
    }
    
  }
  
}

#
# Calculate country level data
#

# Sum players and WAR by each country and year
countries <- ddply(temp,
                  .(Year, country), summarize,
                  Players = length(bref_id_mlb),
                  WAR = sum(WAR),
                  avgWAR = sum(WAR)/length(bref_id_mlb))
# Sum players and WAR by year (to calculate %'s later)
ntot <- ddply(temp,
              .(Year), summarize,
              TotPlayers = length(bref_id_mlb),
              TotWAR = sum(WAR))

# For each country find top 5 players (brefID, name, career WAR)
temp2 <- unique(countries$country)
for(i in 1:length(temp2)){
  c <- temp2[i]
  
  # Subset to country and order by decreasing career WAR
  temp3 <- subset(temp, country==c)
  temp3 <- temp3[ order(temp3$careerWAR, decreasing=T), ]
  
  # Find unique players then match in their WAR
  temp4 <- unique(temp3$bref_id_mlb)
  temp5 <- unique(temp3$name)
  temp6 <- c(  subset(temp3, bref_id_mlb==temp4[1])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[2])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[3])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[4])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[5])[1,"careerWAR"]
  ) 
  
  # Combine into vector
  players <- c(temp4[1], temp5[1],temp6[1],
               temp4[2], temp5[2], temp6[2],
               temp4[3], temp5[3],temp6[3],
               temp4[4], temp5[4],temp6[4],
               temp4[5], temp5[5], temp6[5]
               
  )
  
  # Compile output
  if(i==1){
    topplay <- c( c, players)
  }else {
    topplay <- rbind( topplay, c(c, players))
  }
  
}
# Add names to data frame
topplay <- as.data.frame(topplay)
names(topplay) <- c("country","bref1","name1","war1",
                    "bref2","name2","war2",
                    "bref3","name3","war3",
                    "bref4","name4","war4",
                    "bref5","name5", "war5")
# Merge in year total data
countries <- merge(countries, ntot, by = "Year")
# Calculate %'s
countries$PctPlayers <- countries$Players / countries$TotPlayers*100
countries$PctWAR <- countries$WAR / countries$TotWAR*100
# Merge in top player data
countries <- merge(countries, topplay, by="country")

# Make strings
for(i in 6:ncol(countries)){
  countries[, i] <- as.character(countries[,i])
}

# Add in rows of zeros where country has missing data (i.e. no players in 1975)
for(j in 1:length(unique(countries$country))){ # 
  st <- unique(countries$country)[j]
  temp6 <- subset(countries, country==st)
  if(nrow(temp6)>2){
    # If missing data for given year fill with zeros and correct top player info
    for(i in min(temp6$Year):max(temp6$Year)){
      if(nrow(subset(temp6, Year==i))==0){
        countries <- rbind(countries, c("country" = st,
                                        "Year"= i,
                                        "Players" = 0,
                                        "WAR" = 0,
                                        "avgWAR" = 0,
                                        "TotPlayers" = 0,
                                        "PctPlayers" = 0,
                                        "TotWAR" = 0,
                                        "PctWAR" = 0,
                                        "bref1" = temp6[1,"bref1"],
                                        "name1" = temp6[1, "name1"],
                                        "war1" = temp6[1,"war1"],
                                        "bref2" = temp6[1,"bref2"],
                                        "name2" = temp6[1, "name2"],
                                        "war2" = temp6[1,"war2"],
                                        "bref3" = temp6[1,"bref3"],
                                        "name3" = temp6[1, "name3"],
                                        "war3" = temp6[1,"war3"],
                                        "bref4" = temp6[1,"bref4"],
                                        "name4" = temp6[1, "name4"],
                                        "war4" = temp6[1,"war4"],
                                        "bref5" = temp6[1,"bref5"],
                                        "name5" = temp6[1, "name5"],
                                        "war5" = temp6[1,"war5"]))
      }
    }
  }
  
}
# Convert career War back to numeric for rounding in graph app
for(i in c(2:9,12,15,18,21,24)){
  countries[, i] <- as.numeric(countries[,i])
}

#
# US State level data
#

# Sum players and war by Year and US State
statesUS<- ddply(subset(temp, country=="United States" ),
                   .(Year, state), summarize,
                   Players = length(bref_id_mlb),
                   WAR = sum(WAR),
                   avgWAR = sum(WAR)/length(bref_id_mlb))
# Sum players and WAR by year
ntot <- ddply(subset(temp, country=="United States"),
              .(Year), summarize,
              TotPlayers = length(bref_id_mlb),
              TotWAR = sum(WAR))
# For each unique state find top player info
temp2 <- unique(statesUS$state)
for(i in 1:length(temp2)){
  s <- temp2[i]
  # Subset and decreasing careeer WAR order
  temp3 <- subset(temp, state==s & country=="United States")
  temp3 <- temp3[ order(temp3$careerWAR, decreasing=T), ]
  # Find unique names, then match in careerWAR
  temp4 <- unique(temp3$bref_id_mlb)
  temp5 <- unique(temp3$name)
  temp6 <- c(  subset(temp3, bref_id_mlb==temp4[1])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[2])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[3])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[4])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[5])[1,"careerWAR"]
  ) 
  # Create vector 
  players <- c(temp4[1], temp5[1],temp6[1],
               temp4[2], temp5[2],temp6[2],
               temp4[3], temp5[3],temp6[3],
               temp4[4], temp5[4],temp6[4],
               temp4[5], temp5[5],temp6[5]
               
  )
  # Compile output
  if(i==1){
    topplay <- c( s, players)
  }else {
    topplay <- rbind( topplay, c(s, players))
  }
  
}
# Add dataframe names
topplay <- as.data.frame(topplay)
names(topplay) <- c("state","bref1","name1","war1",
                    "bref2","name2","war2",
                    "bref3","name3","war3",
                    "bref4","name4","war4",
                    "bref5","name5","war5")
# Merge in total Player and WAR data
statesUS <- merge(statesUS, ntot, by = "Year")
# Calculate %'s
statesUS$PctPlayers <- statesUS$Players / statesUS$TotPlayers*100
statesUS$PctWAR <- statesUS$WAR / statesUS$TotWAR*100
# Merge in top player info
statesUS <- merge(statesUS, topplay, by="state")

# Make strings
for(i in 6:ncol(statesUS)){
  statesUS[, i] <- as.character(statesUS[,i])
}
# For each unique state, if missing data add in row of zeros and top player info
for(j in 1:length(unique(statesUS$state))){ # 
  st <- unique(statesUS$state)[j]
  temp6 <- subset(statesUS, state==st)
  if(nrow(temp6)>2){
    for(i in min(temp6$Year):max(temp6$Year)){
      if(nrow(subset(temp6, Year==i))==0){
        statesUS <- rbind(statesUS, c("state" = st,
                                        "Year"= i,
                                        "Players" = 0,
                                        "WAR" = 0,
                                        "avgWAR" = 0,
                                      "TotPlayers" = 0,
                                      "PctPlayers" = 0,
                                      "TotWAR" = 0,
                                      "PctWAR" = 0,
                                      "bref1" = temp6[1,"bref1"],
                                        "name1" = temp6[1, "name1"],
                                      "war1" = temp6[1,"war1"],
                                        "bref2" = temp6[1,"bref2"],
                                        "name2" = temp6[1, "name2"],
                                      "war2" = temp6[1,"war2"],
                                        "bref3" = temp6[1,"bref3"],
                                        "name3" = temp6[1, "name3"],
                                      "war3" = temp6[1,"war3"],
                                        "bref4" = temp6[1,"bref4"],
                                        "name4" = temp6[1, "name4"],
                                      "war4" = temp6[1,"war4"],
                                        "bref5" = temp6[1,"bref5"],
                                        "name5" = temp6[1, "name5"],
                                      "war5" = temp6[1,"war5"]))
      }
    }
  }
  
}
# Make numeric 
for(i in c(2:9,12,15,18,21,24)){
  statesUS[, i] <- as.numeric(statesUS[,i])
}


#
#  Calculate Venezuelan state level data
#

# Sum players and war by year and Ven state
statesVen<- ddply(subset(temp, country=="Venezuela" ),
                 .(Year, state), summarize,
                 Players = length(bref_id_mlb),
                 WAR = sum(WAR),
                 avgWAR = sum(WAR)/length(bref_id_mlb))
# Sum players and WAR by year
ntot <- ddply(subset(temp, country=="Venezuela"),
              .(Year), summarize,
              TotPlayers = length(bref_id_mlb),
              TotWAR = sum(WAR))

# For each unique state find top players from there
temp2 <- unique(statesVen$state)
for(i in 1:length(temp2)){
  s <- temp2[i]
  # Subset and decrease order
  temp3 <- subset(temp, state==s & country=="Venezuela")
  temp3 <- temp3[ order(temp3$careerWAR, decreasing=T), ]
  # Find unique players and merge in career WAR
  temp4 <- unique(temp3$bref_id_mlb)
  temp5 <- unique(temp3$name)
  temp6 <- c(  subset(temp3, bref_id_mlb==temp4[1])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[2])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[3])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[4])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[5])[1,"careerWAR"]
  ) 
  # Make vector
  players <- c(temp4[1], temp5[1],temp6[1],
               temp4[2], temp5[2],temp6[2],
               temp4[3], temp5[3],temp6[3],
               temp4[4], temp5[4],temp6[4],
               temp4[5], temp5[5],temp6[5]
               
  )
  # Compile output
  if(i==1){
    topplay <- c( s, players)
  }else {
    topplay <- rbind( topplay, c(s, players))
  }
  
}
# Add names to data frame
topplay <- as.data.frame(topplay)
names(topplay) <- c("state","bref1","name1","war1",
                    "bref2","name2","war2",
                    "bref3","name3","war3",
                    "bref4","name4","war4",
                    "bref5","name5","war5")
# Merge in total data
statesVen <- merge(statesVen, ntot, by = "Year")
# Calculate %'s
statesVen$PctPlayers <- statesVen$Players / statesVen$TotPlayers*100
statesVen$PctWAR <- statesVen$WAR / statesVen$TotWAR*100
# Merge in top player info
statesVen <- merge(statesVen, topplay, by="state")

# Make strings
for(i in 6:ncol(statesVen)){
  statesVen[, i] <- as.character(statesVen[,i])
}
# For each state if missing data, add in row of zeros and top player info
for(j in 1:length(unique(statesVen$state))){ # 
  st <- unique(statesVen$state)[j]
  temp6 <- subset(statesVen, state==st)
  if(nrow(temp6)>2){
    for(i in min(temp6$Year):max(temp6$Year)){
      if(nrow(subset(temp6, Year==i))==0){
        statesVen <- rbind(statesVen, c("state" = st,
                                        "Year"= i,
                                        "Players" = 0,
                                        "WAR" = 0,
                                        "avgWAR" = 0,
                                        "TotPlayers" = 0,
                                        "PctPlayers" = 0,
                                        "TotWAR" = 0,
                                        "PctWAR" = 0,
                                        "bref1" = temp6[1,"bref1"],
                                        "name1" = temp6[1, "name1"],
                                        "war1" = temp6[1,"war1"],
                                        "bref2" = temp6[1,"bref2"],
                                        "name2" = temp6[1, "name2"],
                                        "war2" = temp6[1,"war2"],
                                        "bref3" = temp6[1,"bref3"],
                                        "name3" = temp6[1, "name3"],
                                        "war3" = temp6[1,"war3"],
                                        "bref4" = temp6[1,"bref4"],
                                        "name4" = temp6[1, "name4"],
                                        "war4" = temp6[1,"war4"],
                                        "bref5" = temp6[1,"bref5"],
                                        "name5" = temp6[1, "name5"],
                                        "war5" = temp6[1,"war5"]))
      }
    }
  }
  
}
# Make numeric for later rounding
for(i in c(2:9,12,15,18,21,24)){
  statesVen[, i] <- as.numeric(statesVen[,i])
}


#
# DR State level data
#

# Sum players and WAR by year and state
statesDR<- ddply(subset(temp, country=="Dominican Republic" ),
                  .(Year, state), summarize,
                  Players = length(bref_id_mlb),
                  WAR = sum(WAR),
                  avgWAR = sum(WAR)/length(bref_id_mlb))
# Sum players and WAR by year
ntot <- ddply(subset(temp, country=="Dominican Republic"),
              .(Year), summarize,
              TotPlayers = length(bref_id_mlb),
              TotWAR = sum(WAR))
# For each unique state find top player info 
temp2 <- unique(statesDR$state)
for(i in 1:length(temp2)){
  s <- temp2[i]
  # Subset and decrease order
  temp3 <- subset(temp, state==s & country=="Dominican Republic")
  temp3 <- temp3[ order(temp3$careerWAR, decreasing=T), ]
  # Find unique players and merge in career WAR
  temp4 <- unique(temp3$bref_id_mlb)
  temp5 <- unique(temp3$name)
  temp6 <- c(  subset(temp3, bref_id_mlb==temp4[1])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[2])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[3])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[4])[1,"careerWAR"], 
               subset(temp3, bref_id_mlb==temp4[5])[1,"careerWAR"]
  ) 
  # Make vector
  players <- c(temp4[1], temp5[1],temp6[1],
               temp4[2], temp5[2],temp6[2],
               temp4[3], temp5[3],temp6[3],
               temp4[4], temp5[4],temp6[4],
               temp4[5], temp5[5],temp6[5]
               
  )
  # Compile output
  if(i==1){
    topplay <- c( s, players)
  }else {
    topplay <- rbind( topplay, c(s, players))
  }
  
}
# Add names to data frame
topplay <- as.data.frame(topplay)
names(topplay) <- c("state","bref1","name1","war1",
                    "bref2","name2","war2",
                    "bref3","name3","war3",
                    "bref4","name4","war4",
                    "bref5","name5","war5")
# Merge in total data
statesDR <- merge(statesDR, ntot, by = "Year")
# Calculate %'s
statesDR$PctPlayers <- statesDR$Players / statesDR$TotPlayers*100
statesDR$PctWAR <- statesDR$WAR / statesDR$TotWAR*100
# Merge in top player info
statesDR <- merge(statesDR, topplay, by="state")

# Make strings
for(i in 6:ncol(statesDR)){
  statesDR[, i] <- as.character(statesDR[,i])
}
# For each state if missing data, add row of zeros with top player info
for(j in 1:length(unique(statesDR$state))){ # 
  st <- unique(statesDR$state)[j]
  temp6 <- subset(statesDR, state==st)
  if(nrow(temp6)>2){
    for(i in min(temp6$Year):max(temp6$Year)){
      if(nrow(subset(temp6, Year==i))==0){
        statesDR <- rbind(statesDR, c("state" = st,
                                        "Year"= i,
                                        "Players" = 0,
                                        "WAR" = 0,
                                        "avgWAR" = 0,
                                      "TotPlayers" = 0 ,
                                      "PctPlayers" = 0,
                                      "TotWAR" = 0 ,
                                      "PctWAR" = 0,
                                      "bref1" = temp6[1,"bref1"],
                                        "name1" = temp6[1, "name1"],
                                      "war1" =temp6[1, "war1"],
                                        "bref2" = temp6[1,"bref2"],
                                        "name2" = temp6[1, "name2"],
                                      "war2" =temp6[1, "war2"],  
                                      "bref3" = temp6[1,"bref3"],
                                        "name3" = temp6[1, "name3"],
                                      "war4" =temp6[1, "war4"],
                                        "bref4" = temp6[1,"bref4"],
                                        "name4" = temp6[1, "name4"],
                                      "war4" =temp6[1, "war4"],
                                        "bref5" = temp6[1,"bref5"],
                                        "name5" = temp6[1, "name5"],
                                      "war5" =temp6[1, "war5"]))
      }
    }
  }
  
}
# Make numeric for later rounding
for(i in c(2:9,12,15,18,21,24)){
  statesDR[, i] <- as.numeric(statesDR[,i])
}

# Save to file, and graph app file
saveRDS(statesUS, "data/graphstuff/StatesMLBBirth.rds")
saveRDS(statesVen, "data/graphstuff/StatesVenMLBBirth.rds")
saveRDS(statesDR, "data/graphstuff/StatesDRMLBBirth.rds")
saveRDS(countries, "data/graphstuff/CountryMLBBirth.rds")

saveRDS(statesUS, "MLBbirthGraph-app/data/StatesMLBBirth.rds")
saveRDS(statesVen, "MLBbirthGraph-app/data/StatesVenMLBBirth.rds")
saveRDS(statesDR, "MLBbirthGraph-app/data/StatesDRMLBBirth.rds")
saveRDS(countries, "MLBbirthGraph-app/data/CountryMLBBirth.rds")

