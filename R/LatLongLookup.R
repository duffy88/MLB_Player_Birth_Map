# Looks up latitude and longitude based on birthplace string from 
# clean MLB player season data and biographic data.
#
# Note : If using this, Google limits you to 2500 geocodes per day.

# Load Packages
library(ggmap)

# Load cleaned player info
birth2 <- readRDS("data/CleanBirthInfo/CleanMLBPlayerBirthInfo(Post1970).rds")

# Determine unique players
birthids <- unique(birth2$bref_id_mlb)
birthids <- birthids[order(birthids)]
# For each player clean birthplace info slightly
# then use Google's geocode service to return latitude and longitude info
for(i in 1:length(birthids)){ #
  birthplace <- subset(birth2, bref_id_mlb==birthids[i])[1,"birthplace"]
  if(!is.na(birthplace)){
    if(substr(birthplace,1,1)==" "){
      birthplace <- substring(birthplace, 2)
    }
      
    lonlat <- geocode(birthplace) 
    }else {
      lonlat <- geocode(birthplace) 
    }
    
    # Compile output of locations
    if(i ==1){
      temp2 <- cbind(birthids[i], lonlat)
      print(i)
    }else {
      temp2 <- rbind(temp2, cbind(birthids[i], lonlat))
      print(i)
    }
    
  
}
# Fix col name
names(temp2)[1] <- "bref_id_mlb"
# Save to file
write.csv(temp2, "data/birthloc(Post1970-batch2)new.csv",row.names = F)



