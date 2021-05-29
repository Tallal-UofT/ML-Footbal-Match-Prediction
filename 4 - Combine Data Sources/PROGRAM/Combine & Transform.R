## start
# Load:
options(scipen=999)
rm(list=ls())
Start = Sys.time()
# 0a - Install and load required packages
pkgs <- c("tidyverse","openxlsx","stringr","lubridate","magrittr", "haven","rstudioapi","readxl","data.table","scales","ggrepel","plyr", "dplyr", "mosaic", "stringi", "DescTools", "Hmisc")
# Install: - should not need to re-install if already installed
# install.packages(pkgs)
# Load:
lapply(pkgs, require, character.only = TRUE)
###############################################
###             Read-ins and Paths          ###
###############################################
Input <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/2 - Transform/OUTPUT/'
Crosswalk <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/3 - Crosswalk/'
Output <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/4 - Combine Data Sources/OUTPUT/'
###############################################
###             Combine Players Data       ###
###############################################
Player_DF <- read.xlsx(paste0(Input, "Player Data.xlsx"))
Match_DF <- read.xlsx(paste0(Input, "Match Data.xlsx"))

Crosswalk_Df <- read.xlsx(paste0(Crosswalk, "Crosswalk.xlsx"), sheet = 3)

Combined_Df <- inner_join(Match_DF, Crosswalk_Df, by = c("HomeTeam" = "Match_Club.Name")) %>%
  mutate(HomeTeam_Player = Player_Name) %>%
  select(-c(Player_Name)) %>%
  inner_join(Crosswalk_Df, by = c("AwayTeam" = "Match_Club.Name")) %>%
  mutate(AwayTeam_Player = Player_Name) %>%
  select(-c(Player_Name, AwayTeam, HomeTeam)) %>%
  mutate(Date_var1 = as.Date(Date, format =  "%d/%m/%Y")) %>%
  mutate(Date_var2 = as.Date(Date, format =  "%d/%m/%y")) %>%
  mutate(Character_C = nchar(Date)) %>%
  mutate(Date_Proper = as.Date("10/10/2020", format = "%m/%d/%Y"))
  
Combined_Df$Date_Proper <- ifelse(Combined_Df$Character_C > 9, as.character(Combined_Df$Date_var1), as.character(Combined_Df$Date_var2))

Combined_Df <- Combined_Df %>% 
  mutate(Date_Proper = as.Date(Date_Proper, format = "%Y-%m-%d")) %>%
  select(-c(Date_var1, Date_var2, Character_C,Date))

Combined_Df <- mutate(Combined_Df, Game_Date = derivedFactor(
  "14-15" = Date_Proper > as.Date("07/01/2014", format = "%m/%d/%Y") & Date_Proper < as.Date("07/01/2015", format = "%m/%d/%Y"),
  "15-16" = Date_Proper > as.Date("07/01/2015", format = "%m/%d/%Y") & Date_Proper < as.Date("07/01/2016", format = "%m/%d/%Y"),
  "16-17" = Date_Proper >  as.Date("07/01/2016", format = "%m/%d/%Y") & Date_Proper < as.Date("07/01/2017", format = "%m/%d/%Y"),
  "17-18" = Date_Proper >  as.Date("07/01/2017", format = "%m/%d/%Y") & Date_Proper < as.Date("07/01/2018", format = "%m/%d/%Y"),
  "18-19" = Date_Proper >  as.Date("07/01/2018", format = "%m/%d/%Y")  & Date_Proper < as.Date("07/01/2019", format = "%m/%d/%Y"),
  "19-20" = Date_Proper >  as.Date("07/01/2019", format = "%m/%d/%Y")  & Date_Proper < as.Date("07/01/2020", format = "%m/%d/%Y"),
  "20-21" = Date_Proper >  as.Date("07/01/2020", format = "%m/%d/%Y"),
  .method = "first",
  .default = "20-21",
  .asFactor = F))

Player_DF <- Player_DF %>%
  select(-c(club_nameutf))

Columns_C <- Player_DF %>% 
  select(age_Att:Def_Work_Rate_Ind_Mid) %>%
  names()

Player_DF_H <- cbind(Player_DF)
Player_DF_A <- cbind(Player_DF)


for (col in Columns_C){
  names(Player_DF_H)[names(Player_DF_H) == col] <- paste0(col, "_h")
  names(Player_DF_A)[names(Player_DF_A) == col] <- paste0(col, "_a")
}

names(Player_DF_H)[names(Player_DF_H) == "league_name"] <- paste0("league_name", "_h")
names(Player_DF_H)[names(Player_DF_H) == "club_name"] <- paste0("club_name", "_h")

names(Player_DF_A)[names(Player_DF_A) == "league_name"] <- paste0("league_name", "_a")
names(Player_DF_A)[names(Player_DF_A) == "club_name"] <- paste0("club_name", "_a")

Final_df <- inner_join(Combined_Df, Player_DF_H, by = c("HomeTeam_Player" = "club_name_h", "Game_Date"))
Final_df <- inner_join(Final_df,Player_DF_A, by = c("AwayTeam_Player" = "club_name_a", "Game_Date"))
  

for (col in Columns_C){
  Final_df[col] <- Final_df[paste0(col, "_h")] - Final_df[paste0(col, "_a")]
}

Final_df <- Final_df %>%
  select(c(FTHG, FTAG, Result, HomeTeam_Player, AwayTeam_Player, Date_Proper, Game_Date, league_name_h, league_name_a, Columns_C))

Final_df[is.na(Final_df)] <- 0


Final_df$Match_Combo <- paste0(Final_df$HomeTeam_Player, ".", Final_df$AwayTeam_Player)


saveRDS(Final_df, file = paste0(Output,"Final.rds"))
