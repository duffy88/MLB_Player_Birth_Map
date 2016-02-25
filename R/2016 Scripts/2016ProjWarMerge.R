# Merge in Steamer projected 2016 WAR with BirthMap prepared dataset 
# for players born after 1940 to minimize players with duplicate names.
# (Steamer has a seperate player ID system, must try to match by first/last name, gets ugly...)

# Load Clean birth info and Steamer bat and pitcher projections
cbirthinfo <- readRDS("data/BirthMap/BirthMap(Post1940).rds")
steamB <- read.csv("/Users/Doug/Documents/Doug/Sports\ Analytics/Baseball/Fantasy\ Baseball\ 2016/data/Steamer/hitters.csv", stringsAsFactors = F)
steamP <- read.csv("/Users/Doug/Documents/Doug/Sports\ Analytics/Baseball/Fantasy\ Baseball\ 2016/data/Steamer/pitchers.csv", stringsAsFactors = F)

# Split name 
cbirthinfo$temp <- strsplit( cbirthinfo$name, " ")

# Determine Baseball Ref first and last name
for( i in 1:nrow(cbirthinfo)){
  cbirthinfo$nameFirstBref[i] <- cbirthinfo$temp[[i]][1]
  cbirthinfo$nameLastBref[i] <- cbirthinfo$temp[[i]][2]
  if(length(cbirthinfo$temp[[i]]) ==3){
    cbirthinfo$nameFirstBref[i] <- cbirthinfo$temp[[i]][1]
    cbirthinfo$nameLastBref[i] <- paste(cbirthinfo$temp[[i]][2], cbirthinfo$temp[[i]][3])
    
  }else if(length(cbirthinfo$temp[[i]]) ==4){
    cbirthinfo$nameFirstBref[i] <- cbirthinfo$temp[[i]][1]
    cbirthinfo$nameLastBref[i] <- paste(cbirthinfo$temp[[i]][2], cbirthinfo$temp[[i]][3], cbirthinfo$temp[[i]][4])
    
  }else if(length(cbirthinfo$temp[[i]]) ==5){
    cbirthinfo$nameFirstBref[i] <- cbirthinfo$temp[[i]][1]
    cbirthinfo$nameLastBref[i] <- paste(cbirthinfo$temp[[i]][2], cbirthinfo$temp[[i]][3], 
                                cbirthinfo$temp[[i]][4], cbirthinfo$temp[[i]][5])
    
  }
  
}
# Set Steamer first and last name to BRef's by default
cbirthinfo$nameFirstSt <- cbirthinfo$nameFirstBref
cbirthinfo$nameLastSt <- cbirthinfo$nameLastBref

# Remove accented symbols from Steamer names
cbirthinfo$nameFirstSt <- gsub("á","a", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("á","a", cbirthinfo$nameLastSt)
cbirthinfo$nameFirstSt <- gsub("é","e", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("é","e", cbirthinfo$nameLastSt)
cbirthinfo$nameFirstSt <- gsub("ó","o", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("ó","o", cbirthinfo$nameLastSt)
cbirthinfo$nameFirstSt <- gsub("í","i", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("í","i", cbirthinfo$nameLastSt)
cbirthinfo$nameFirstSt <- gsub("Á","A", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("Á","A", cbirthinfo$nameLastSt)
cbirthinfo$nameFirstSt <- gsub("ñ","n", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("ñ","n", cbirthinfo$nameLastSt)
cbirthinfo$nameFirstSt <- gsub("ú","u", cbirthinfo$nameFirstSt)
cbirthinfo$nameLastSt <- gsub("ú","u", cbirthinfo$nameLastSt)

# Make corrections to match the steamer names in the birth info
for(i in 1:nrow(cbirthinfo)){
  
  if(cbirthinfo$nameLastBref[i] =="Bradley" &
     cbirthinfo$nameFirstBref[i] =="Jackie"){
    cbirthinfo$nameLastSt[i] <- "Bradley Jr."
    cbirthinfo$nameFirstSt[i] <- "Jackie"
  }
  if(cbirthinfo$nameLastBref[i] =="DeShields" &
     cbirthinfo$nameFirstBref[i] =="Delino"){
    cbirthinfo$nameLastSt[i] <- "Deshields"
    cbirthinfo$nameFirstSt[i] <- "Delino"
  }
  if(cbirthinfo$nameLastBref[i] =="Ryan Murphy" &
     cbirthinfo$nameFirstBref[i] =="John"){
    cbirthinfo$nameLastSt[i] <- "Murphy"
    cbirthinfo$nameFirstSt[i] <- "JR"
  }
  if(cbirthinfo$nameLastBref[i] =="Ho Kang" &
     cbirthinfo$nameFirstBref[i] =="Jung"){
    cbirthinfo$nameLastSt[i] <- "Kang"
    cbirthinfo$nameFirstSt[i] <- "Jung-ho"
  }
  if(cbirthinfo$nameLastBref[i] =="Joyce" &
     cbirthinfo$nameFirstBref[i] =="Matthew"){
    cbirthinfo$nameLastSt[i] <- "Joyce"
    cbirthinfo$nameFirstSt[i] <- "Matt"
  }
  if(cbirthinfo$nameLastBref[i] =="Montas" &
     cbirthinfo$nameFirstBref[i] =="Frankie"){
    cbirthinfo$nameLastSt[i] <- "Montas"
    cbirthinfo$nameFirstSt[i] <- "Francelis"
  }
  if(cbirthinfo$nameLastBref[i] =="Ryu" &
     cbirthinfo$nameFirstBref[i] =="Hyun-jin"){
    cbirthinfo$nameLastSt[i] <- "Ryu"
    cbirthinfo$nameFirstSt[i] <- "Hyun-Jin"
  }
  if(cbirthinfo$nameLastBref[i] =="Gray" &
     cbirthinfo$nameFirstBref[i] =="Jon"){
    cbirthinfo$nameLastSt[i] <- "Gray"
    cbirthinfo$nameFirstSt[i] <- "Jonathan"
  }
  if(cbirthinfo$nameLastBref[i] =="De La Rosa" &
     cbirthinfo$nameFirstBref[i] =="Jorge"){
    cbirthinfo$nameLastSt[i] <- "de la Rosa"
    cbirthinfo$nameFirstSt[i] <- "Jorge"
  }
  if(cbirthinfo$nameLastBref[i] =="Wisler" &
     cbirthinfo$nameFirstBref[i] =="Matt"){
    cbirthinfo$nameLastSt[i] <- "Wisler"
    cbirthinfo$nameFirstSt[i] <- "Matthew"
  }
  if(cbirthinfo$nameLastBref[i] =="Montgomery" &
     cbirthinfo$nameFirstBref[i] =="Mike"){
    cbirthinfo$nameLastSt[i] <- "Montgomery"
    cbirthinfo$nameFirstSt[i] <- "Michael"
  }
  if(cbirthinfo$nameLastBref[i] =="Broadway" &
     cbirthinfo$nameFirstBref[i] =="Michael"){
    cbirthinfo$nameLastSt[i] <- "Broadway"
    cbirthinfo$nameFirstSt[i] <- "Mike"
  }
  if(cbirthinfo$nameLastBref[i] =="Goody" &
     cbirthinfo$nameFirstBref[i] =="Nick"){
    cbirthinfo$nameLastSt[i] <- "Goody"
    cbirthinfo$nameFirstSt[i] <- "Nicholas"
  }
  if(cbirthinfo$nameLastBref[i] =="Tropeano" &
     cbirthinfo$nameFirstBref[i] =="Nick"){
    cbirthinfo$nameLastSt[i] <- "Tropeano"
    cbirthinfo$nameFirstSt[i] <- "Nicholas"
  }
  if(cbirthinfo$nameLastBref[i] =="De La Rosa" &
     cbirthinfo$nameFirstBref[i] =="Rubby"){
    cbirthinfo$nameLastSt[i] <- "de la Rosa"
    cbirthinfo$nameFirstSt[i] <- "Rubby"
  }
  if(cbirthinfo$nameLastBref[i] =="Tuivailala" &
     cbirthinfo$nameFirstBref[i] =="Sam"){
    cbirthinfo$nameLastSt[i] <- "Tuivailala"
    cbirthinfo$nameFirstSt[i] <- "Samuel"
  }
  if(cbirthinfo$nameLastBref[i] =="Layne" &
     cbirthinfo$nameFirstBref[i] =="Tommy"){
    cbirthinfo$nameLastSt[i] <- "Layne"
    cbirthinfo$nameFirstSt[i] <- "Tom"
  }
  if(cbirthinfo$nameLastBref[i] =="Davies" &
     cbirthinfo$nameFirstBref[i] =="Zach"){
    cbirthinfo$nameLastSt[i] <- "Davies"
    cbirthinfo$nameFirstSt[i] <- "Zachary"
  }
  if(cbirthinfo$nameLastBref[i] =="Godley" &
     cbirthinfo$nameFirstBref[i] =="Zack"){
    cbirthinfo$nameLastSt[i] <- "Godley"
    cbirthinfo$nameFirstSt[i] <- "Zachary"
  }
  
}

# Take only useful data columns from Steamer data frames
steamB <- steamB[, c("Batters","Team", "WAR","playerid")]
steamP <- steamP[, c("Pitchers","Team","WAR", "playerid")]

# Split name
steamB$temp <- strsplit(steamB$Batters, " ")
# Determine first and last names for Steamer
for(i in 1:nrow(steamB)){
  steamB$nameFirstSt[i] <- steamB$temp[[i]][1]
  steamB$nameLastSt[i] <- steamB$temp[[i]][2]
  if(length(steamB$temp[[i]]) ==3){
    steamB$nameFirstSt[i] <- steamB$temp[[i]][1]
    steamB$nameLastSt[i] <- paste(steamB$temp[[i]][2], steamB$temp[[i]][3])
    
  }else if(length(steamB$temp[[i]]) ==4){
    steamB$nameFirstSt[i] <- steamB$temp[[i]][1]
    steamB$nameLastSt[i] <- paste(steamB$temp[[i]][2], steamB$temp[[i]][3], steamB$temp[[i]][4])
    
  }else if(length(steamB$temp[[i]]) ==5){
    steamB$nameFirstSt[i] <- steamB$temp[[i]][1]
    steamB$nameLastSt[i] <- paste(steamB$temp[[i]][2], steamB$temp[[i]][3], 
                                steamB$temp[[i]][4], steamB$temp[[i]][5])
    
  }
  
}
# Correct teams to team ID's
for(i in 1:nrow(steamB)){
  steamB$teamID[i] <- switch( steamB$Team[i], 
                              "Diamondbacks"= "ARI","Braves"= "ATL","Orioles"= "BAL","Red Sox"= "BOS","Cubs"= "CHC",
                              "Reds"="CIN","Indians"="CLE","Rockies"="COL","White Sox"="CHW","Tigers"="DET","Astros"="HOU",
                              "Royals"="KCA","Angels"="LAA","Dodgers"="LAD","Marlins"="MIA","Brewers"="MIL","Twins"="MIN",
                              "Mets"="NYM","Yankees"="NYY","Athletics"="OAK","Phillies"="PHI","Pirates"="PIT","Padres"="SDP", "Mariners"="SEA",
                              "Giants"="SFG", "Cardinals"="STL", "Rays"="TBR", "Rangers"="TEX", "Blue Jays"="TOR", "Nationals"="WSN", "FA"
  )
}
# Use Steamer names as initial guess for BRef name
steamB$nameLastBref <- steamB$nameLastSt
steamB$nameFirstBref <- steamB$nameFirstSt


#
# Pitchers
#

# Split name
steamP$temp <- strsplit(steamP$Pitchers, " ")
# Determine first/last name
for(i in 1:nrow(steamP)){
  steamP$nameFirstSt[i] <- steamP$temp[[i]][1]
  steamP$nameLastSt[i] <- steamP$temp[[i]][2]
  if(length(steamP$temp[[i]]) ==3){
    steamP$nameFirstSt[i] <- steamP$temp[[i]][1]
    steamP$nameLastSt[i] <- paste(steamP$temp[[i]][2], steamP$temp[[i]][3])
    
  }else if(length(steamP$temp[[i]]) ==4){
    steamP$nameFirstSt[i] <- steamP$temp[[i]][1]
    steamP$nameLastSt[i] <- paste(steamP$temp[[i]][2], steamP$temp[[i]][3], steamP$temp[[i]][4])
    
  }else if(length(steamP$temp[[i]]) ==5){
    steamP$nameFirstSt[i] <- steamP$temp[[i]][1]
    steamP$nameLastSt[i] <- paste(steamP$temp[[i]][2], steamP$temp[[i]][3], 
                                steamP$temp[[i]][4], steamP$temp[[i]][5])
    
  }
  
}
# Use Steamer name as first guess for Bref name
steamP$nameLastBref <- steamP$nameLastSt
steamP$nameFirstBref <- steamP$nameFirstSt

# Switch team name for team IDs
for(i in 1:nrow(steamP)){
  steamP$teamID[i] <- switch( steamP$Team[i], 
                              "Diamondbacks"= "ARI","Braves"= "ATL","Orioles"= "BAL","Red Sox"= "BOS","Cubs"= "CHC",
                              "Reds"="CIN","Indians"="CLE","Rockies"="COL","White Sox"="CHW","Tigers"="DET","Astros"="HOU",
                              "Royals"="KCA","Angels"="LAA","Dodgers"="LAD","Marlins"="MIA","Brewers"="MIL","Twins"="MIN",
                              "Mets"="NYM","Yankees"="NYY","Athletics"="OAK","Phillies"="PHI","Pirates"="PIT","Padres"="SDP", "Mariners"="SEA",
                              "Giants"="SFG", "Cardinals"="STL", "Rays"="TBR", "Rangers"="TEX", "Blue Jays"="TOR", "Nationals"="WSN", "FA"
  )
}
# Make sure team ID's match
unique(steamB$teamID)[!(unique(steamB$teamID) %in% unique(cbirthinfo$Tm))]
unique(steamP$teamID)[!(unique(steamP$teamID) %in% unique(cbirthinfo$Tm))]

# Take only players projected to produce more than 0 WAR
steamB <- subset(steamB, WAR > 0)
steamP <- subset(steamP, WAR > 0)
# Define Year
steamB$Year <- 2016
steamP$Year <- 2016

# Remove old players from cbirthinfo whose first and last names match other 
# younger players for 2016
rem <- 0
for(i in 1:nrow(cbirthinfo)){
  if(cbirthinfo$bref_id_mlb[i]=="powelbo01"){ # Older Boog Powell
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="murphto02"){ # Older Tom Murphy
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="bautijo01"){ # Older Jose Bautista
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="hernace01"){ # Older Cesar Hernandez
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="deshide01"){ # Older Delino Deshields 
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="perezca01"){ # Older Carlos Perez
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="cruzne01"){ # Older Nelson Cruz
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="eatonad01"){ # Older Adam Eaton
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="cartech01"){ # Older Chris Carter
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="reyesjo02"){ # Older Jose Reyes
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="taylomi01"){ # Older Michael Taylor
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="martica03"){ # Older Carlos Martinez#1
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="martica02"){ # Older Carlos Martinez#2
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="rodried01"){ # Older Eduardo Rodriguez
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="burgoen01"){ # Older Enrique Burgos
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="ramirer01"){ # Older Erasmo Ramirez
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="rodrifr04"){ # Older Francisco Rodriguez
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="owenshe01"){ # Older Henry Owens
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="johnsji03"){ # Older Jim Johnson
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="lambjo01"){ # Older John Lamb
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="alvarjo01"){ # Older Jose Alvarez
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="milleju01"){ # Older Justin Miller
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="mcculla01"){ # Older Lance McCullers
    rem <- c(rem, i)
  }
  if(cbirthinfo$bref_id_mlb[i]=="hernaro01"){ # Older Roberto Hernandez
    rem <- c(rem, i)
  }
  
}
# Remove the rows that match above
cbirthinfo <- cbirthinfo[-rem, ]

# Merge birth info with Steamer data frames
matchB <- merge(cbirthinfo, steamB[,c("nameFirstSt","nameLastSt","Year","WAR","playerid","teamID")],
                by=c("nameFirstSt","nameLastSt"), all.y=T)
matchP <- merge(cbirthinfo, steamP[,c("nameFirstSt","nameLastSt","Year","WAR","playerid","teamID")],
                by=c("nameFirstSt","nameLastSt"), all.y=T)

# Cleaning mis-matched hitters
rem <- 0 
for(i in 1:nrow(matchB)){
  if(!is.na(matchB$bref_id_mlb[i])){
    if( matchB$bref_id_mlb[i] == "youngch03"){ # Remove Chris Young the Picher 
      rem <- c(rem, i )
    }
    if( matchB$bref_id_mlb[i] == "ramirjo02"){ # Remove Jose Ramirez the Picher 
      rem <- c(rem, i )
    }
    if( matchB$bref_id_mlb[i] == "braunry01"){ # Remove Ryan Braun the Picher 
      rem <- c(rem, i )
    }
    if( matchB$bref_id_mlb[i] == "duffyma02" &
        matchB$playerid[i] == "13836"){ # Remove Matt Duffy HOU from Matt Duffy SFG
      rem <- c(rem, i )
    }
    if( matchB$bref_id_mlb[i] == "duffyma01" &
        matchB$playerid[i] == "12181"){ # Remove Matt Duffy SFG from Matt Duffy HOU
      rem <- c(rem, i )
    }
    
  }
  
}
# Remove the rows that match above
matchB <- matchB[-rem, ]

# Find unique playerid 
playidsB2016 <- unique(matchB$playerid)

# Find which playerids for 2016 have matched two previous players
twoplayers <- 0
for(i in 1:length(playidsB2016)){
  temp <- subset(matchB, playerid==playidsB2016[i])
  temp2 <- unique(temp$bref_id_mlb)
  
  if(length(temp2)>1){
    twoplayers <- c(twoplayers, i )
  }
}

# Replace previous Year and WAR columns then remove old columns
matchB$Year <- matchB$Year.y
matchB$Year.x <- matchB$Year.y <- NULL
matchB$WAR <- matchB$WAR.y
matchB$WAR.x <- matchB$WAR.y <- NULL

# Take only needed columns
matchB <- matchB[ , c("nameFirstSt","nameLastSt","playerid","Year","teamID","WAR",
                      "nameFirstBref","nameLastBref","bref_id_mlb","bref_id")]
# Now remove duplicated rows
matchB <- matchB[!duplicated(matchB),]

#
# Clean pitcher matching
#

# Remove mis-matched pitchers
rem <- 0 
for(i in 1:nrow(matchP)){
  if(!is.na(matchP$bref_id_mlb[i])){
    if( matchP$bref_id_mlb[i] == "johnsbr01"){ # Remove Brian Johnson the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "hatchch01"){ # Remove Chris Hatcher the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "johnser03"){ # Remove Erik Johnson the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "fernajo01"){ # Remove Jose Fernandez the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "fieldjo02"){ # Remove Josh Fields the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "garcilu02"){ # Remove Luis Garcis#1 the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "garcilu01"){ # Remove Luis Garcis#2 the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "gonzami04"){ # Remove Miguel Gonzalez the Hitter 
      rem <- c(rem, i )
    }
    if( matchP$bref_id_mlb[i] == "gonzami05" &
        matchP$playerid[i] == "7024"){ # Remove Miguel Alfredo Gonzalez PHI from Miguel Gonzalez BAL 
      rem <- c(rem, i )
    }
    
    
  }
  
}
# Remove rows matched above
matchP <- matchP[-rem, ]

# Determine unique playerids in matched pitcher data frame
playidsP2016 <- unique(matchP$playerid)

# Find which playerids for 2016 have matched two previous players
twoplayers <- 0
for(i in 1:length(playidsP2016)){
  temp <- subset(matchP, playerid==playidsP2016[i])
  temp2 <- unique(temp$bref_id_mlb)
  
  if(length(temp2)>1){
    twoplayers <- c(twoplayers, i )
  }
}

# Replace previous Year and WAR columns, remove unused columns
matchP$Year <- matchP$Year.y
matchP$Year.x <- matchP$Year.y <- NULL
matchP$WAR <- matchP$WAR.y
matchP$WAR.x <- matchP$WAR.y <- NULL

# Take needed columns
matchP <- matchP[ , c("nameFirstSt","nameLastSt","playerid","Year","teamID","WAR",
                      "nameFirstBref","nameLastBref","bref_id_mlb","bref_id")]

# Remove duplicated rows
matchP <- matchP[!duplicated(matchP),]

#
# Add bref info for missing/ only minor league players
#

# Manually define Baseball reference info for Steamer players without MLB BRef page

#Pitchers
for( i in 1:nrow(matchP)){
  if(is.na(matchP$bref_id[i])){
    if(matchP$playerid[i]=="sa549491"){
      matchP$nameFirstBref[i] <- "Aaron"
      matchP$nameLastBref[i] <- "Blair"
      matchP$bref_id[i] <- "blair-001aar"
      
    }
    if(matchP$playerid[i]=="18498"){
      matchP$nameFirstBref[i] <- "Kenta"
      matchP$nameLastBref[i] <- "Maeda"
      matchP$bref_id[i] <- "maeda-001ken"
      
    }
    if(matchP$playerid[i]=="sa657908"){
      matchP$nameFirstBref[i] <- "Lucas"
      matchP$nameLastBref[i] <- "Giolito"
      matchP$bref_id[i] <- "giolit000luc"
      
    }
    if(matchP$playerid[i]=="sa657852"){
      matchP$nameFirstBref[i] <- "Jose"
      matchP$nameLastBref[i] <- "Berrios"
      matchP$bref_id[i] <- "berrio000jos"
      
    }
    if(matchP$playerid[i]=="sa548151"){
      matchP$nameFirstBref[i] <- "Jameson"
      matchP$nameLastBref[i] <- "Taillon"
      matchP$bref_id[i] <- "taillo001jam"
      
    }
    if(matchP$playerid[i]=="sa597835"){
      matchP$nameFirstBref[i] <- "Tyler"
      matchP$nameLastBref[i] <- "Glasnow"
      matchP$bref_id[i] <- "glasno000tyl"
      
    }
    if(matchP$playerid[i]=="sa597792"){
      matchP$nameFirstBref[i] <- "Robert"
      matchP$nameLastBref[i] <- "Stephenson"
      matchP$bref_id[i] <- "stephe006rob"
      
    }
    if(matchP$playerid[i]=="sa501507"){
      matchP$nameFirstBref[i] <- "Buddy"
      matchP$nameLastBref[i] <- "Baumann"
      matchP$bref_id[i] <- "bauman001geo"
      
    }
    if(matchP$playerid[i]=="sa621583"){
      matchP$nameFirstBref[i] <- "Kyle"
      matchP$nameLastBref[i] <- "Zimmer"
      matchP$bref_id[i] <- "zimmer000kyl"
      
    }
    if(matchP$playerid[i]=="sa597786"){
      matchP$nameFirstBref[i] <- "Blake"
      matchP$nameLastBref[i] <- "Snell"
      matchP$bref_id[i] <- "snell-000bla"
      
    }
    if(matchP$playerid[i]=="sa526929"){
      matchP$nameFirstBref[i] <- "Jake"
      matchP$nameLastBref[i] <- "Thompson"
      matchP$bref_id[i] <- "thomps003jac"
      
    }
    if(matchP$playerid[i]=="sa501726"){
      matchP$nameFirstBref[i] <- "Mark"
      matchP$nameLastBref[i] <- "Appel"
      matchP$bref_id[i] <- "appel-001mar"
      
    }
    if(matchP$playerid[i]=="sa598097"){
      matchP$nameFirstBref[i] <- "Jacob"
      matchP$nameLastBref[i] <- "Faria"
      matchP$bref_id[i] <- "faria-000jac"
      
    }
    if(matchP$playerid[i]=="sa599246"){
      matchP$nameFirstBref[i] <- "Josh"
      matchP$nameLastBref[i] <- "Martin"
      matchP$bref_id[i] <- "martin046jos"
      
    }
    if(matchP$playerid[i]=="sa506912"){
      matchP$nameFirstBref[i] <- "Luis"
      matchP$nameLastBref[i] <- "Cessa"
      matchP$bref_id[i] <- "cessa-001lui"
      
    }
    if(matchP$playerid[i]=="sa548285"){
      matchP$nameFirstBref[i] <- "Nick"
      matchP$nameLastBref[i] <- "Kingham"
      matchP$bref_id[i] <- "kingha001nic"
      
    }
    if(matchP$playerid[i]=="sa658155"){
      matchP$nameFirstBref[i] <- "Chris"
      matchP$nameLastBref[i] <- "O'Grady"
      matchP$bref_id[i] <- "ogrady000chr"
      
    }
    if(matchP$playerid[i]=="sa658129"){
      matchP$nameFirstBref[i] <- "Daniel"
      matchP$nameLastBref[i] <- "Stumpf"
      matchP$bref_id[i] <- "stumpf000dan"
      
    }
    if(matchP$playerid[i]=="sa597956"){
      matchP$nameFirstBref[i] <- "Michael"
      matchP$nameLastBref[i] <- "Clevinger"
      matchP$bref_id[i] <- "clevin000mic"
      
    }
    if(matchP$playerid[i]=="sa621793"){
      matchP$nameFirstBref[i] <- "Nick"
      matchP$nameLastBref[i] <- "Wittgren"
      matchP$bref_id[i] <- "wittgr000nic"
      
    }
    if(matchP$playerid[i]=="sa596976"){
      matchP$nameFirstBref[i] <- "Adalberto"
      matchP$nameLastBref[i] <- "Mejia"
      matchP$bref_id[i] <- "mejia-000ada"
      
    }
    if(matchP$playerid[i]=="sa739551"){
      matchP$nameFirstBref[i] <- "Alexander"
      matchP$nameLastBref[i] <- "Reyes"
      matchP$bref_id[i] <- "reyes-000ale"
      
    }
    if(matchP$playerid[i]=="sa598555"){
      matchP$nameFirstBref[i] <- "Blake"
      matchP$nameLastBref[i] <- "McFarland"
      matchP$bref_id[i] <- "mcfarl000bla"
      
    }
    if(matchP$playerid[i]=="sa598491"){
      matchP$nameFirstBref[i] <- "Carlos"
      matchP$nameLastBref[i] <- "Estevez"
      matchP$bref_id[i] <- "esteve002car"
      
    }
    if(matchP$playerid[i]=="sa598302"){
      matchP$nameFirstBref[i] <- "Clayton"
      matchP$nameLastBref[i] <- "Blackburn"
      matchP$bref_id[i] <- "blackb000cla"
      
    }
    if(matchP$playerid[i]=="sa526393"){
      matchP$nameFirstBref[i] <- "Dean"
      matchP$nameLastBref[i] <- "Kiekhefer"
      matchP$bref_id[i] <- "kiekhe001dea"
      
    }
    if(matchP$playerid[i]=="sa502368"){
      matchP$nameFirstBref[i] <- "Derek"
      matchP$nameLastBref[i] <- "Law"
      matchP$bref_id[i] <- "law---001der"
      
    }
    if(matchP$playerid[i]=="sa549263"){
      matchP$nameFirstBref[i] <- "Evan"
      matchP$nameLastBref[i] <- "Rutckyj"
      matchP$bref_id[i] <- "rutcky001eva"
      
    }
    if(matchP$playerid[i]=="sa658247"){
      matchP$nameFirstBref[i] <- "Jeffrey"
      matchP$nameLastBref[i] <- "Wendelken"
      matchP$bref_id[i] <- "wendel000jef"
      
    }
    if(matchP$playerid[i]=="sa579271"){
      matchP$nameFirstBref[i] <- "Jacob"
      matchP$nameLastBref[i] <- "Barnes"
      matchP$bref_id[i] <- "barnes002jac"
      
    }
    if(matchP$playerid[i]=="sa597438"){
      matchP$nameFirstBref[i] <- "Jarlin"
      matchP$nameLastBref[i] <- "Garcia"
      matchP$bref_id[i] <- "garcia000jar"
      
    }
    if(matchP$playerid[i]=="sa328221"){
      matchP$nameFirstBref[i] <- "Jeff"
      matchP$nameLastBref[i] <- "Walters"
      matchP$bref_id[i] <- "walter001jef"
      
    }
    if(matchP$playerid[i]=="sa599378"){
      matchP$nameFirstBref[i] <- "Jharel"
      matchP$nameLastBref[i] <- "Cotton"
      matchP$bref_id[i] <- "cotton000jha"
      
    }
    if(matchP$playerid[i]=="sa597766"){
      matchP$nameFirstBref[i] <- "Joseph"
      matchP$nameLastBref[i] <- "Musgrove"
      matchP$bref_id[i] <- "musgro000joe"
      
    }
    if(matchP$playerid[i]=="sa599212"){
      matchP$nameFirstBref[i] <- "John"
      matchP$nameLastBref[i] <- "Gant"
      matchP$bref_id[i] <- "gant--000joh"
      
    }
    if(matchP$playerid[i]=="sa389911"){
      matchP$nameFirstBref[i] <- "Josh"
      matchP$nameLastBref[i] <- "Smoker"
      matchP$bref_id[i] <- "smoker001jos"
      
    }
    if(matchP$playerid[i]=="sa736324"){
      matchP$nameFirstBref[i] <- "Julio"
      matchP$nameLastBref[i] <- "Urias"
      matchP$bref_id[i] <- "urias-000jul"
      
    }
    if(matchP$playerid[i]=="sa597764"){
      matchP$nameFirstBref[i] <- "Michael"
      matchP$nameLastBref[i] <- "Fulmer"
      matchP$bref_id[i] <- "fulmer002mic"
      
    }
    if(matchP$playerid[i]=="sa599174"){
      matchP$nameFirstBref[i] <- "Rookie"
      matchP$nameLastBref[i] <- "Davis"
      matchP$bref_id[i] <- "davis-024wil"
      
    }
    if(matchP$playerid[i]=="sa598282"){
      matchP$nameFirstBref[i] <- "Ryan"
      matchP$nameLastBref[i] <- "Merritt"
      matchP$bref_id[i] <- "merrit000rya"
      
    }
    if(matchP$playerid[i]=="sa599698"){
      matchP$nameFirstBref[i] <- "Jacob"
      matchP$nameLastBref[i] <- "Lugo"
      matchP$bref_id[i] <- "lugo--000jac"
      
    }
    if(matchP$playerid[i]=="sa550187"){
      matchP$nameFirstBref[i] <- "Steven"
      matchP$nameLastBref[i] <- "Okert"
      matchP$bref_id[i] <- "okert-001ste"
      
    }
    if(matchP$playerid[i]=="sa597787"){
      matchP$nameFirstBref[i] <- "Taylor"
      matchP$nameLastBref[i] <- "Guerrieri"
      matchP$bref_id[i] <- "guerri000tay"
      
    }
    if(matchP$playerid[i]=="sa503168"){
      matchP$nameFirstBref[i] <- "Taylor"
      matchP$nameLastBref[i] <- "Rogers"
      matchP$bref_id[i] <- "rogers002tay"
      
    }
    if(matchP$playerid[i]=="sa658009"){
      matchP$nameFirstBref[i] <- "Ty"
      matchP$nameLastBref[i] <- "Blach"
      matchP$bref_id[i] <- "blach-000ty-"
      
    }
    if(matchP$playerid[i]=="sa578009"){
      matchP$nameFirstBref[i] <- "Tyler"
      matchP$nameLastBref[i] <- "Anderson"
      matchP$bref_id[i] <- "anders001tyl"
      
    }
    
  }
}

# Batters
for( i in 1:nrow(matchB)){
  if(is.na(matchB$bref_id[i])){
    if(matchB$playerid[i]=="6195"){
      matchB$nameFirstBref[i] <- "Ian"
      matchB$nameLastBref[i] <- "Kinsler"
      matchB$bref_id[i] <- "kinsle001ian"
      matchB$bref_id_mlb[i] <- "kinslia01"
      
    }
    if(matchB$playerid[i]=="9776"){
      matchB$nameFirstBref[i] <- "Jason"
      matchB$nameLastBref[i] <- "Kipnis"
      matchB$bref_id[i] <- "kipnis001jas"
      matchB$bref_id_mlb[i] <- "kipnija01"
      
    }
    if(matchB$playerid[i]=="18717"){
      matchB$nameFirstBref[i] <- "Byung-ho"
      matchB$nameLastBref[i] <- "Park"
      matchB$bref_id[i] <- "park--000byu"
      
    }
    if(matchB$playerid[i]=="18718"){
      matchB$nameFirstBref[i] <- "Hyeon-soo"
      matchB$nameLastBref[i] <- "Kim"
      matchB$bref_id[i] <- "kim---004hye"
      
    }
    if(matchB$playerid[i]=="sa737507"){
      matchB$nameFirstBref[i] <- "J.P."
      matchB$nameLastBref[i] <- "Crawford"
      matchB$bref_id[i] <- "crawfo000jp-"
      
    }
    if(matchB$playerid[i]=="sa596917"){
      matchB$nameFirstBref[i] <- "Orlando"
      matchB$nameLastBref[i] <- "Arcia"
      matchB$bref_id[i] <- "arcia-000orl"
      
    }
    if(matchB$playerid[i]=="sa599279"){
      matchB$nameFirstBref[i] <- "A.J."
      matchB$nameLastBref[i] <- "Reed"
      matchB$bref_id[i] <- "reed--000and"
      
    }
    if(matchB$playerid[i]=="sa737508"){
      matchB$nameFirstBref[i] <- "Tim"
      matchB$nameLastBref[i] <- "Anderson"
      matchB$bref_id[i] <- "anders003tim"
      
    }
    if(matchB$playerid[i]=="sa658509"){
      matchB$nameFirstBref[i] <- "Boog"
      matchB$nameLastBref[i] <- "Powell"
      matchB$bref_id[i] <- "powell000her"
      
    }
    if(matchB$playerid[i]=="sa392225"){
      matchB$nameFirstBref[i] <- "Jabari"
      matchB$nameLastBref[i] <- "Blash"
      matchB$bref_id[i] <- "blash-001jab"
      
    }
    if(matchB$playerid[i]=="sa658022"){
      matchB$nameFirstBref[i] <- "Joey"
      matchB$nameLastBref[i] <- "Wendle"
      matchB$bref_id[i] <- "wendle000jos"
      
    }
    if(matchB$playerid[i]=="sa549847"){
      matchB$nameFirstBref[i] <- "Aaron"
      matchB$nameLastBref[i] <- "Judge"
      matchB$bref_id[i] <- "judge-001aar"
      
    }
    if(matchB$playerid[i]=="sa547735"){
      matchB$nameFirstBref[i] <- "Alen"
      matchB$nameLastBref[i] <- "Hanson"
      matchB$bref_id[i] <- "hanson001ale"
      
    }
    if(matchB$playerid[i]=="sa505301"){
      matchB$nameFirstBref[i] <- "Alfredo"
      matchB$nameLastBref[i] <- "Gonzalez"
      matchB$bref_id[i] <- "gonzal004alf"
      
    }
    if(matchB$playerid[i]=="sa390416"){
      matchB$nameFirstBref[i] <- "Brett"
      matchB$nameLastBref[i] <- "Eibner"
      matchB$bref_id[i] <- "eibner001bre"
      
    }
    if(matchB$playerid[i]=="sa658005"){
      matchB$nameFirstBref[i] <- "Brett"
      matchB$nameLastBref[i] <- "Phillips"
      matchB$bref_id[i] <- "philli000bre"
      
    }
    if(matchB$playerid[i]=="sa597793"){
      matchB$nameFirstBref[i] <- "Charlie"
      matchB$nameLastBref[i] <- "Tilson"
      matchB$bref_id[i] <- "tilson000cha"
      
    }
    if(matchB$playerid[i]=="sa526841"){
      matchB$nameFirstBref[i] <- "Colin"
      matchB$nameLastBref[i] <- "Walsh"
      matchB$bref_id[i] <- "walsh-001col"
      
    }
    if(matchB$playerid[i]=="sa621658"){
      matchB$nameFirstBref[i] <- "Joey"
      matchB$nameLastBref[i] <- "Rickard"
      matchB$bref_id[i] <- "rickar000joe"
      
    }
    if(matchB$playerid[i]=="sa551307"){
      matchB$nameFirstBref[i] <- "Jorge"
      matchB$nameLastBref[i] <- "Alfaro"
      matchB$bref_id[i] <- "alfaro001jor"
      
    }
    if(matchB$playerid[i]=="12148"){
      matchB$nameFirstBref[i] <- "Kyle"
      matchB$nameLastBref[i] <- "Kubitza"
      matchB$bref_id[i] <- "kubitz000kyl"
      matchB$bref_id_mlb[i] <- "kubitky01"
      
    }
    if(matchB$playerid[i]=="sa597765"){
      matchB$nameFirstBref[i] <- "Trevor"
      matchB$nameLastBref[i] <- "Story"
      matchB$bref_id[i] <- "story-000tre"
      
    }
    if(matchB$playerid[i]=="sa502772"){
      matchB$nameFirstBref[i] <- "Tyler"
      matchB$nameLastBref[i] <- "Naquin"
      matchB$bref_id[i] <- "naquin001tyl"
      
    }
    if(matchB$playerid[i]=="sa508176"){
      matchB$nameFirstBref[i] <- "Willson"
      matchB$nameLastBref[i] <- "Contreras"
      matchB$bref_id[i] <- "contre002wil"
      
    }
    
    
  }
}

# Combine matched batters and pitchers into one data frame
players2016 <- rbind(matchB, matchP)

# Save to file
saveRDS(players2016, "data/2016/Players2016FangraphsBrefIDs.rds")
