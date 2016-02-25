# Cleans various pieces of info for MLB players and merges with player season data
#
# Load player lists
bat <- readRDS("data/Batters/Batters(70to15).rds")
pitch <- readRDS("data/Pitchers/Pitchers(70to15).rds")

# Load player birth and other info
birthtable <- readRDS("data/BirthInfo/MLBPlayerBirthInfo(Post1970).rds")
# Fix col name
names(birthtable)[1] <- "bref_id_mlb"


# Combine hitters and pitchers into one data frame
birth <- rbind(bat, pitch)

# Deal with players with both hitting and pitching data 
# in same season. Babe Ruth fix...
#
# 1822 fixes in Bef 1900, 626 fixes 1900to39, 465 in 40to69, 338 in 70to15
#
# Fix order first to put same season hitting pitching data for same team
# in adjacent rows.
birth <- birth[ order(birth$Tm), ]
birth <- birth[ order(birth$Year), ]
birth <- birth[ order(birth$bref_id_mlb), ]

# Define empty vector for players with both hitting and pitching data
hps <- 0
# For each row see if year, player id and team are same as previous row, 
# then store row number
for(i in 2:nrow(birth)){
  if(birth$Year[i]==birth$Year[i-1] &
     birth$bref_id_mlb[i]==birth$bref_id_mlb[i-1] &
     birth$Tm[i]==birth$Tm[i-1] 
  ){
    hps <- c(hps, i )
  }
}
# For each of the rows found above, sum the adjacent rows of WAR data
for(i in 2:length(hps)){
  birth[hps[i], "WAR"] <- birth[hps[i], "WAR"] + birth[hps[i]-1, "WAR"]
}
# Define vector of rows to remove now that we've added them to the other row
rem <- hps[2:length(hps)] - 1
# Remove those rows
birth <- birth[-rem, ]


# Cut born/died label from info
birthtable$birthinfo <- substring(birthtable$birthinfo, 7)
birthtable$deathinfo <- substring(birthtable$deathinfo, 7)

# Progress bar
pb <- txtProgressBar(min = 1, max = nrow(birthtable), style = 3)
# For each row of table perform the needed string splits and cleaning
for(i in 1:nrow(birthtable) ){ #
  # Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  # Birth info
  birthtable$bday[i] <- strsplit(birthtable$birthinfo, " in ")[[i]][1]
  birthtable$birthplace[i] <- strsplit(birthtable$birthinfo, " in ")[[i]][2]
  # Death info
  birthtable$deathday[i] <- strsplit(birthtable$deathinfo, " in ")[[i]][1]
  birthtable$deathplace[i] <- strsplit(birthtable$deathinfo, " in ")[[i]][2]
  # Split info by line
  birthtable$line1[i] <- strsplit(birthtable$info, "\n")[[i]][1]
  birthtable$line2[i] <- strsplit(birthtable$info, "\n")[[i]][2]
  birthtable$line3[i] <- strsplit(birthtable$info, "\n")[[i]][3]
  # Position
  birthtable$Pos[i] <- strsplit(birthtable$line1, ": ")[[i]][2]
  # Handedness
  birthtable$BatHand[i] <- strsplit(birthtable$line2, ",")[[i]][1]
  birthtable$ThrowHand[i] <- strsplit(birthtable$line2, ",")[[i]][2]
  birthtable$BatHand[i] <- strsplit(birthtable$BatHand, ": ")[[i]][2]
  birthtable$ThrowHand[i] <- strsplit(birthtable$ThrowHand, ": ")[[i]][2]
  # Height and Weight
  birthtable$Height[i] <- strsplit(birthtable$line3, ",")[[i]][1]
  birthtable$Weight[i] <- strsplit(birthtable$line3, ",")[[i]][2]
  birthtable$Height[i] <- strsplit(birthtable$Height, ": ")[[i]][2]
  birthtable$Weight[i] <- strsplit(birthtable$Weight, ": ")[[i]][2]
  
}
# Close progress bar
close(pb)
# Merge player season data with birth and other biographic data
birth <- merge(birth, birthtable[, c("bref_id_mlb","name","bday","birthplace","deathday","deathplace",
                                     "pic","Pos","BatHand","ThrowHand","Height","Weight")],
               by = "bref_id_mlb", all.x = T)

# Save to file
saveRDS(birth, "data/CleanBirthInfo/CleanMLBPlayerBirthInfo(Post1970).rds")


