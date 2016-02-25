# Scrape information from baseball reference and save to file.
# Slightly different for 2016 to account for players with missing MLB ID's
#
# Retrieves : Name, Birth Info, Death info, Ht, Weight, Handedness, and picture link

# Load Packages
library(XML)
library(rvest)
library(stringr)
require(xlsx)

# Load player lists
birth <- readRDS("data/2016/Players2016FangraphsBrefIDs.rds")

# Take only needed columns
birth <- birth[ ,c("Year","bref_id","bref_id_mlb","WAR","teamID")]

# Change data frame (why?)
birthtable <- birth

# Create progress bar
pb <- txtProgressBar(min = 1, max = nrow(birthtable), style = 3)

# For each player look up biographic info
for(i in 1:nrow(birthtable)){# nrow(birthtable)
  
  # Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  # If MLB ID info available use players MLB page
  if(!is.na(birthtable$bref_id_mlb[i])){
    
    # URL
    urlmlb <- paste("http://www.baseball-reference.com/players/", 
                    substr(birthtable$bref_id_mlb[i],1,1),sep="")
    urlmlb <- paste(urlmlb, "/",sep="")
    urlmlb <- paste(urlmlb, birthtable$bref_id_mlb[i],sep="")
    urlmlb <- paste(urlmlb, ".shtml", sep="")
    
    # Name
    htmlpage <- read_html(urlmlb)
    namehtml <- html_nodes(htmlpage, "#player_name")
    name <- html_text(namehtml)
    
    # Birth info
    birthinfohtml <- html_nodes(htmlpage, "#necro-birth")
    birthinfo <- html_text(birthinfohtml)
    
    # Death info
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
    
    # Assorted info
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
    
    # If no MLB ID info use Bref non-MLB ID
  }else {
    # URL
    url <- paste("http://www.baseball-reference.com/register/player.cgi?id=", 
                    birthtable$bref_id[i],sep="")
    # Name
    htmlpage <- read_html(url)
    namehtml <- html_nodes(htmlpage, "#player_name")
    name <- html_text(namehtml)
    
    # Birth info
    birthinfohtml <- html_nodes(htmlpage, "#necro-birth")
    birthinfo <- html_text(birthinfohtml)
    
    # Death info
    deathinfohtml <- html_nodes(htmlpage, "#necro-death")
    deathinfo <- html_text(deathinfohtml)
    if(length(deathinfo)==0) deathinfo <- NA
    
    # Pitcture link as string
    pichtml <- html_nodes(htmlpage, ".border")
    if(length(pichtml)!=0){
      pic <- as.character(pichtml)
      pic <- strsplit(pic, 'src=\"')[[1]][2]
      pic <- strsplit( pic, '\"')[[1]][1]
    }else {
      pic <- NA
    }
    
    # Assorted info
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
    
  }
  
  
  
    
    # Enter scraped info in table
  birthtable$name[i] <- name
  birthtable$birthinfo[i] <- birthinfo
  birthtable$deathinfo[i] <- deathinfo
  birthtable$info[i] <- info
  birthtable$pic[i] <- pic
  
}
# Close progress bar
close(pb)

# Save to file
saveRDS(birthtable, "data/2016/2016MLBPlayerBirthInfo.rds")



