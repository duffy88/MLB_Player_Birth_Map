# Looks up latitude and longitude based on birthplace string from 
# clean MLB player season data and biographic data.
#
# Note : If using this, Google limits you to 2500 geocodes per day.

# Load packages
library(ggmap)

# Load Cleaned player info
birth2 <- readRDS("data/2016/2016CleanMLBPlayerBirthInfo.rds")
  
# Determine unique players
birthids <- unique(birth2$bref_id)
birthids <- birthids[order(birthids)]

# Put NA as string to pass to google
birth2 <- as.data.frame(birth2)
birth2$birthplace[is.na(birth2$birthplace) ] <- "Not available."
  
# For each player, look up latitude and longitude
for(i in 1:length(birthids)){ #
  birthplace <- subset(birth2, bref_id==birthids[i])[1,"birthplace"]
  if(!is.na(birthplace)){
    if(substr(birthplace,1,1)==" "){
      birthplace <- substring(birthplace, 2)
      }
      
    lonlat <- geocode(birthplace) 
   }else {
    lonlat <- geocode(birthplace) 
  }
    
    # Compile output
    if(i ==1){
      temp2 <- cbind(birthids[i], lonlat)
      print(i)
    }else {
      temp2 <- rbind(temp2, cbind(birthids[i], lonlat))
      print(i)
    }
    
  
}
# Fix col name
names(temp2)[1] <- "bref_id"
  
# Save to file
write.csv(temp2, "data/BirthLoc/birthloc(2016-batch2).csv",row.names = F)
