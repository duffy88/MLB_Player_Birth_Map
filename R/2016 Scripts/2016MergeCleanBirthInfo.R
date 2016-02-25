# Cleans various pieces of info for MLB players and merges with player season data
#
# Slightly different for 2016, cleaning of info different for Minor league page


# Load player lists
birth <- readRDS("data/2016/Players2016FangraphsBrefIDs.rds")
birth <- birth[ ,c("Year","bref_id","bref_id_mlb","WAR","teamID")]

# Load scraped player birth info
birthtable <- readRDS("data/2016/2016MLBPlayerBirthInfo.rds")

# Remove label for born/died
birthtable$birthinfo <- substring(birthtable$birthinfo, 7)
birthtable$deathinfo <- substring(birthtable$deathinfo, 7)

# Create progress bar
pb <- txtProgressBar(min = 1, max = nrow(birthtable), style = 3)

# Initial cleaning of assorted information
for(i in 1:nrow(birthtable) ){ #
  
  # Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  # Birth
  birthtable$bday[i] <- strsplit(birthtable$birthinfo, " in ")[[i]][1]
  birthtable$birthplace[i] <- strsplit(birthtable$birthinfo, " in ")[[i]][2]
  # Death
  birthtable$deathday[i] <- strsplit(birthtable$deathinfo, " in ")[[i]][1]
  birthtable$deathplace[i] <- strsplit(birthtable$deathinfo, " in ")[[i]][2]
  # Lines of biographic info
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

# Initial cleaning messes up info scraped from minor league pages, this fixes it
for(i in 1:nrow(birthtable) ){ #nrow(birthtable)
  # Check if player has no MLB ID and is missing the suspected data
  if(is.na(birthtable$bref_id_mlb[i]) &
     is.na(birthtable$line3[i])){
    # Shift data columns and fix string splitting error from above
    birthtable$line3[i] <- birthtable$line2[i]
    birthtable$line2[i] <- strsplit(birthtable$line1, "Bats: ")[[i]][2]
    birthtable$line1[i] <- strsplit(birthtable$line1, "Bats: ")[[i]][1]
    birthtable$line2[i] <- paste("Bats: ",birthtable$line2[i], sep="")
    
    # Use new cleaned info to correct data from above
    # Pos
    birthtable$Pos[i] <- strsplit(birthtable$line1, ": ")[[i]][2]
    # Handedness
    birthtable$BatHand[i] <- strsplit(birthtable$line2, ",")[[i]][1]
    birthtable$ThrowHand[i] <- strsplit(birthtable$line2, ",")[[i]][2]
    birthtable$BatHand[i] <- strsplit(birthtable$BatHand, ": ")[[i]][2]
    birthtable$ThrowHand[i] <- strsplit(birthtable$ThrowHand, ": ")[[i]][2]
    # Height and weight
    birthtable$Height[i] <- strsplit(birthtable$line3, ",")[[i]][1]
    birthtable$Weight[i] <- strsplit(birthtable$line3, ",")[[i]][2]
    birthtable$Height[i] <- strsplit(birthtable$Height, ": ")[[i]][2]
    birthtable$Weight[i] <- strsplit(birthtable$Weight, ": ")[[i]][2]
    
  }
}

# Merge cleaned birth info with player season data (in this case projections)
birth <- merge(birth, birthtable[, c("bref_id","name","bday","birthplace","deathday","deathplace",
                                     "pic","Pos","BatHand","ThrowHand","Height","Weight")],
               by = "bref_id", all.x = T)

# Save to file
saveRDS(birth, "data/2016/2016CleanMLBPlayerBirthInfo.rds")


