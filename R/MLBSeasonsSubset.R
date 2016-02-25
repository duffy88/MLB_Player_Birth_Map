# Load stats downloaded from baseball reference and organized by player birth year.
# This data is then subsetted to only retain the id info and year, team and WAR info
# and then saved to data/Batters or data/Pitchers folder. 

# Must do this for 4 player sets: Before 1900, 1900 to 1939, 1940 to 1969 and After 1970

# Batter Data
# organized in sincle csv's organized by first letter last name
for(i in 1:length(letters)){#
   if(i!=24){ # See if there is are X's in this set
     # Load file
    file <- paste("/Users/Doug/Documents/Doug/Sports\ Analytics/Baseball/Baseball\ Reference/data/Hitters(Bef1900)/",
                  letters[i],"\'s.csv",sep="")
    BRefBat <- read.csv(file, stringsAsFactors=F)
    
    # Remove Minors data and combined player seasons
    table.MLB <- subset(BRefBat, DBsource=="MLB" &
                          Tm !="TOT" &
                          Lev %in%c("AL","NL"))
    
    # Compile output table
    if(i==1){
      BRefBat.MLB <- table.MLB
    }else {
      BRefBat.MLB <- rbind(BRefBat.MLB, table.MLB)
    }
  }
} 
# Subset to just data needed for this project
BRefBat.MLB <- BRefBat.MLB[ ,c("Year","bref_id","bref_id_mlb","WAR","Tm")]

# Save to file
saveRDS( BRefBat.MLB, "data/Batters/Batters(Bef1900).rds")

# Pitchers Data
# organized in sincle csv's organized by first letter last name
for(i in 1:length(letters)){#
   if(i!=24){ # X's ?
     # Load file
    file <- paste("/Users/Doug/Documents/Doug/Sports\ Analytics/Baseball/Baseball\ Reference/data/Pitchers(Bef1900)/",
                  letters[i],"\'s.csv",sep="")
    BRefPitch <- read.csv(file, stringsAsFactors=F)
    
    # Remove minors data, and combined player seasons
    table.MLB <- subset(BRefPitch, DBsource=="MLB" &
                          Tm !="TOT" &
                          Lev %in%c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefPitch.MLB <- table.MLB
    }else {
      BRefPitch.MLB <- rbind(BRefPitch.MLB, table.MLB)
    }
   }
}  
# Subset to just data need for this project
BRefPitch.MLB <- BRefPitch.MLB[ ,c("Year","bref_id","bref_id_mlb","WAR","Tm")]
# Save to file
saveRDS(BRefPitch.MLB, "data/Pitchers/Pitchers(Bef1900).rds")





