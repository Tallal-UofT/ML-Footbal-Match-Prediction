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
Output <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/2 - Transform/OUTPUT/'
Crosswalk <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/2 - Transform/'
###############################################
###           Wrangle  Matches Data         ###
###############################################
File_Iterate <- Sys.glob(file.path("C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/Data/Matches", "*.csv"))
Appended <- data.frame(HomeTeam=(character()) ,stringsAsFactors=FALSE)


for (file in File_Iterate){
  Matches_DF <- read.csv(file)
  print(file)
  
  Matches_DF <- Matches_DF %>%
    select(c(HomeTeam, AwayTeam, FTHG, FTAG, Date)) %>%
  mutate(Result = derivedFactor(
    "Win" = (FTHG > FTAG),
    "Draw" = (FTHG == FTAG),
    "Lose" = (FTHG < FTAG),
    .method = "first",
    .default = 0,
    .asFactor = T))
  
  Appended <- rbind.fill(Matches_DF, Appended)
  
}

write.xlsx(Appended,paste0(Output, "Match Data.xlsx"))
