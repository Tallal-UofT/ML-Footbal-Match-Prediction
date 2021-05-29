## start
# Load:
options(scipen=999)
rm(list=ls())
Start = Sys.time()
# 0a - Install and load required packages
pkgs <- c("tidyverse","openxlsx","stringr","lubridate","magrittr", "haven","rstudioapi","readxl","data.table","scales","ggrepel","plyr", "dplyr", "mosaic", "stringi", "DescTools")
# Install: - should not need to re-install if already installed
# install.packages(pkgs)
# Load:
lapply(pkgs, require, character.only = TRUE)
###############################################
###             Read-ins and Paths          ###
###############################################
Crosswalk <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/3 - Crosswalk/'
###############################################
###           Create Matches Club Crosswalk###
###############################################
File_Iterate <- Sys.glob(file.path("C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/Data/Matches", "*.csv"))
Appended <- data.frame(Club_Name=(character()) ,stringsAsFactors=FALSE)


for (file in File_Iterate){
  Matches_DF <- read.csv(file)
  print(file)
  
  Home <- Matches_DF %>%
    mutate( Club_Name  = HomeTeam) %>%
    select(Club_Name) %>%
    unique()

  Appended <- rbind.fill(Home, Appended)
  
  
  Away <- Matches_DF %>%
    mutate( Club_Name  = AwayTeam) %>%
    select(Club_Name) %>%
    unique()
  Appended <- rbind.fill(Away, Appended)
  
  Appended <- Appended %>%
    unique()
}

write.xlsx(Appended,paste0(Crosswalk, "Match Clubs.xlsx"))
