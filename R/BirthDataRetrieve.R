# Scrape information from baseball reference and save to file.
#
# Retrieves : Name, Birth Info, Death info, Ht, Weight, Handedness, and picture link

# Load Packages
library(XML)
library(rvest)
library(stringr)
require(xlsx)

# Load player lists
bat <- readRDS("data/Batters/Batters(Bef1900).rds")
pitch <- readRDS("data/Pitchers/Pitchers(Bef1900).rds")

# Combine hitters and pitchers
birth <- rbind(bat, pitch)

# Find how many unique players
birthids <- unique(birth$bref_id_mlb)
birthids <- birthids[order(birthids)]
# Progress bar
pb <- txtProgressBar(min = 1, max = length(birthids), style = 3)

# For each unique player, go to players baseball reference page and retrieve information.
for(i in 1:length(birthids)){# 
  # Set progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  # Define URL
  urlmlb <- paste("http://www.baseball-reference.com/players/", 
                  substr(birthids[i],1,1),sep="")
  urlmlb <- paste(urlmlb, "/",sep="")
  urlmlb <- paste(urlmlb, birthids[i],sep="")
  urlmlb <- paste(urlmlb, ".shtml", sep="")
  
  # Name Info
  htmlpage <- read_html(urlmlb)
  namehtml <- html_nodes(htmlpage, "#player_name")
  name <- html_text(namehtml)
  
  # Birth Info
  birthinfohtml <- html_nodes(htmlpage, "#necro-birth")
  birthinfo <- html_text(birthinfohtml)
  
  # Death Info
  deathinfohtml <- html_nodes(htmlpage, "#necro-death")
  deathinfo <- html_text(deathinfohtml)
  if(length(deathinfo)==0) deathinfo <- NA
  
  # Picture link as string
  pichtml <- html_nodes(htmlpage, ".border")
  if(length(pichtml)!=0){
    pic <- as.character(pichtml)
    pic <- strsplit(pic, 'src=\"')[[1]][2]
    pic <- strsplit( pic, '\"')[[1]][1]
  }else {
    pic <- NA
  }
  
  # Handedness, height and weight info
  info <- html_nodes(htmlpage, "p")
  keep <- 0
  for(j in 1:length(info)){
    t <- as.character(info[j])
    if((grepl("Position", t) & grepl("Bats", t)) |
       (grepl("Throws", t) & grepl("Bats", t))){
      keep <- j
    }
  }
  info <- html_text(info[keep])
  
  # Compile output table for all players
  if(i == 1 ){
    birthtable <- cbind(birthids[i], name, birthinfo, deathinfo,info, pic)
  }else{
    birthtable <- rbind(birthtable, cbind(birthids[i], name, birthinfo,deathinfo, info, pic))
    birthtable <- as.data.frame(birthtable, stringsAsFactors=F)
  }
  
}
# Close progress bar
close(pb)

# Save to file
saveRDS(birthtable, "data/BirthInfo/MLBPlayerBirthInfo(Bef1900).rds")

