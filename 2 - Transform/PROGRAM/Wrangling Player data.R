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
Crosswalk <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/3 - Crosswalk/'
###############################################
###             Wrangle Players Data       ###
###############################################

#Clean Data and Make Dummies
Clean_Player_Data <- function(file){
k <- file
position <- gregexpr(pattern = "players_", k)
m <- substr(k, position[[1]][1] + nchar("players_"), position[[1]][1] + nchar("players_")+1)
Fifa_Game <- paste0( as.character(as.integer(m)-1), "-", m)

  
Players_DF <- read.csv(file)

i <- sapply(Players_DF, is.factor)
Players_DF[i] <- lapply(Players_DF[i], as.character)

Leagues_Keep <- c('Belgian Jupiler Pro League','English League Championship','English Premier League','French Ligue 1','French Ligue 2','German 1. Bundesliga','German 2. Bundesliga','Holland Eredivisie','Italian Serie A','Italian Serie B','Scottish Premiership','Spain Primera Division','Spanish Segunda DivisiÃ³n')
Best_Players_DF <- Players_DF %>%
  filter(club_name != "") %>%
  select(-(ls:rb)) %>%
  select(-c(player_tags, preferred_foot, sofifa_id, player_url, body_type, real_face, release_clause_eur, team_jersey_number, loaned_from, joined, contract_valid_until, nation_jersey_number, short_name, long_name, dob, nationality, nation_position, league_rank)) %>%
  group_by(club_name) %>%
  filter(row_number(desc(overall)) < 20) %>%
  ungroup() %>%
  filter(league_name %in% Leagues_Keep)



Best_Players_DF <- mutate(Best_Players_DF, Att_Work_Rate_Ind = derivedFactor(
  "1" = (grepl("High/", work_rate)),
  "0.5" = (grepl("Medium/", work_rate)),
  .method = "first",
  .default = 0,
  .asFactor = F))

Best_Players_DF <- mutate(Best_Players_DF, Def_Work_Rate_Ind = derivedFactor(
  "1" = (grepl("/High", work_rate)),
  "0.5" = (grepl("/Medium", work_rate)),
  .method = "first",
  .default = 0,
  .asFactor = F))

Variables_Convert <- c('international_reputation', 'weak_foot', 'skill_moves')
for (convert in Variables_Convert){
  Best_Players_DF[convert] <- Best_Players_DF[convert]*20
}


# Variables_Names_GK <- Best_Players_DF %>%
#   select(goalkeeping_diving:goalkeeping_reflexes) %>%
#   names()
# for (Change_v in Variables_Names_GK){
#   Best_Players_DF[Best_Players_DF$player_positions != "GK", Change_v] <- NA
# }
# 
# Variables_Names_nonGK <- Best_Players_DF %>%
#   select(attacking_crossing:defending_sliding_tackle)  %>%
#   names()
# for (Change_v in Variables_Names_nonGK){
#   Best_Players_DF[Best_Players_DF$player_positions == "GK", Change_v] <- NA
# }


Best_Players_DF <- Best_Players_DF %>%
  mutate(player_traits = replace(player_traits, player_positions == 'GK', ''))
List_Traits <- unique(c(as.character(Best_Players_DF$player_traits)))
List_Traits <- trimws(unique(unlist(strsplit(List_Traits, ","))))
for (trait in List_Traits){
  print(paste0('Trait_', gsub(' ', '_', trait)))
  Best_Players_DF[[paste0('Trait_', gsub(" ", "_", trait))]] <- ifelse(grepl(trait, Best_Players_DF$player_traits), 100, 0)
}

Best_Players_DF <- Best_Players_DF %>%
  mutate(Game_Date = Fifa_Game)

Appended <- Best_Players_DF %>% 
  select(-c(work_rate, player_traits)) %>%
  mutate(Def_Work_Rate_Ind = as.double(Def_Work_Rate_Ind)) %>%
  mutate(Att_Work_Rate_Ind = as.double(Att_Work_Rate_Ind))

return(Appended)

}




#Applying Functions
File_Iterate <- Sys.glob(file.path("C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/Data/FIFA Player/FIFA Data Sets", "players*"))
Appended <- data.frame(Game_Date=(character()) ,stringsAsFactors=FALSE)


for (URL in File_Iterate){
Clean_DF <- Clean_Player_Data(URL)
Appended <- rbind.fill(Clean_DF, Appended)
}



#Appended_Drop <- Appended[ , colSums(is.na(Appended)) == 0]

#Creating Shrinked Version
Appended_Shrinked <- Appended %>%
  select(-c(player_positions, team_position)) %>%
  group_by(club_name, league_name, Game_Date) %>%
  summarise_all(mean, na.rm = TRUE)  %>%
  ungroup()

#Creating Expanded Version
Appended_Expanded <- Appended %>%
  mutate(player_positions_p_t = str_detect(player_positions, ",")) %>%
  mutate(player_positions_p_l = StrPos(x = player_positions, pattern = ",") - 1) %>%
  mutate(team_position_p_l = StrPos(x = team_position, pattern = ",")) %>%
  mutate(team_position1 = team_position)

Appended_Expanded$team_position1 <- ifelse((Appended_Expanded$player_positions_p_t == FALSE & (Appended_Expanded$team_position1 == "SUB" | Appended_Expanded$team_position1 == "RES")), Appended_Expanded$player_positions, Appended_Expanded$team_position1)
Appended_Expanded$team_position1 <- ifelse((Appended_Expanded$player_positions_p_t == TRUE & (Appended_Expanded$team_position1 == "SUB" | Appended_Expanded$team_position1 == "RES")), str_sub(Appended_Expanded$player_positions, end = Appended_Expanded$player_positions_p_l), Appended_Expanded$team_position1)

Appended_Expanded <- Appended_Expanded %>%
  select(-c(team_position, player_positions, team_position_p_l, player_positions_p_l,player_positions_p_t)) %>%
  relocate(Game_Date) %>%
  select(-c(Trait_Finesse_Shot:Trait_Swerve_Pass))

Defense <- c('CB','LB','LCB','LDM','RB','RCB','RDM','RWB', 'LWB')
Attack <- c('CAM','CF','LAM','LF','LS','LW','RAM','RF','RS','RW', "ST")
Mid <- c('CM','LCM','RCM','RM','LM', 'CDM')
Goal_Keep <- c('GK')

Appended_Expanded <- mutate(Appended_Expanded, Placement = derivedFactor(
  "Def" = (team_position1 %in% Defense),
  "Att" = (team_position1 %in% Attack),
  "Mid" = (team_position1 %in% Mid),
  "GK" = (team_position1 %in% Goal_Keep),
  .method = "first",
  .default = 0,
  .asFactor = F))

Cols1 <- Appended_Expanded %>%
  select(-c(team_position1, club_name, league_name, Game_Date, Placement)) %>%
  names()

Appended_Expanded <- Appended_Expanded %>%
  select(-c(team_position1)) %>%
  group_by(club_name, league_name, Game_Date, Placement) %>%
  summarise_all(mean, na.rm = TRUE)  %>%
  ungroup() %>%
  pivot_wider(names_from = Placement, values_from = all_of(Cols1)) %>%
  select(-c(pace_GK,shooting_GK, passing_GK, dribbling_GK, defending_GK, physic_GK, gk_diving_Att, gk_diving_Def, gk_diving_Mid, gk_handling_Att, gk_handling_Mid,gk_handling_Def, gk_kicking_Att, gk_kicking_Mid, gk_kicking_Def, gk_reflexes_Att, gk_reflexes_Mid, gk_reflexes_Def, gk_speed_Att, gk_speed_Mid, gk_speed_Def, gk_positioning_Att, gk_positioning_Def, gk_positioning_Mid, skill_moves_GK, Def_Work_Rate_Ind_GK, Att_Work_Rate_Ind_GK))


Appended_Expanded <- Appended_Expanded %>%
  mutate(club_nameutf = club_name) %>%
  mutate(club_name =  iconv(club_name, 'utf-8', 'ascii', sub='')) %>%
  mutate(club_name = replace(club_name, club_name == '1. FC Kln', '1. FC Koln')) %>%
  mutate(club_name = replace(club_name, club_name == 'vian Thonon Gaillard FC', 'Thonon Evian Grand Geneve F.C.')) %>%
  mutate(club_name = replace(club_name, club_name == 'AD Alcorcn', 'AD Alcorcocn')) %>%
  mutate(club_name = replace(club_name, club_name == 'AS Bziers', 'AS Beziers')) %>%
  mutate(club_name = replace(club_name, club_name == 'AS Saint-tienne', 'AS Saint-Etienne')) %>%
  mutate(club_name = replace(club_name, club_name == 'Atltico Madrid', 'Atletico Madrid')) %>%
  mutate(club_name = replace(club_name, club_name == 'AS Saint-tienne', 'AS Saint-Etienne')) %>%
  mutate(club_name = replace(club_name, club_name == 'Borussia Mönchengladbach', 'Borussia Monchengladbach')) %>%
  mutate(club_name = replace(club_name, club_name == 'C.D. Castelln', 'CD Castellon')) %>%
  mutate(club_name = replace(club_name, club_name == 'Cdiz CF', 'Cadiz CF')) %>%
  mutate(club_name = replace(club_name, club_name == 'Crdoba CF', 'Cordoba CF')) %>%
  mutate(club_name = replace(club_name, club_name == 'CD Legans', 'CD Leganes')) %>%
  mutate(club_name = replace(club_name, club_name == 'CD Mirands', 'CD Mirandes')) %>%
  mutate(club_name = replace(club_name, club_name == 'Deportivo Alavs', 'Deportivo Alaves')) %>%
  mutate(club_name = replace(club_name, club_name == 'Deportivo de La Corua', 'Deportivo de La Coruna')) %>%
  mutate(club_name = replace(club_name, club_name == 'FC Bayern Mnchen', 'FC Bayern Munchen')) %>%
  mutate(club_name = replace(club_name, club_name == 'FC Sochaux-Montbliard', 'FC Sochaux-Montbeliard')) %>%
  mutate(club_name = replace(club_name, club_name == 'FC Wrzburger Kickers', 'Wurzburger Kickers')) %>%
  mutate(club_name = replace(club_name, club_name == 'Fortuna Dsseldorf', 'Fortuna Dusseldorf')) %>%
  mutate(club_name = replace(club_name, club_name == 'La Berrichonne de Chteauroux', 'La Berrichonne de Chateauroux')) %>%
  mutate(club_name = replace(club_name, club_name == 'Mlaga CF', 'Malaga CF')) %>%
  mutate(club_name = replace(club_name, club_name == 'Nmes Olympique', 'Nimes Olympique')) %>%
  mutate(club_name = replace(club_name, club_name == 'Real Sporting de Gijn', 'Real Sporting de Gijon')) %>%
  mutate(club_name = replace(club_name, club_name == 'Sevilla Atltico', 'Sevilla Atletico')) %>%
  mutate(club_name = replace(club_name, club_name ==  'SpVgg Greuther Frth', 'SpVgg Greuther Furth')) %>%
  mutate(club_name = replace(club_name, club_name == 'Standard de Lige', 'Standard de Liege')) %>%
  mutate(club_name = replace(club_name, club_name == 'TSV 1860 Mnchen', 'TSV 1860 Munchen')) %>%
  mutate(club_name = replace(club_name, club_name == 'UD Almera', 'UD Almeria')) %>%
  mutate(club_name = replace(club_name, club_name == 'UD Logros', 'UD Logrones')) %>%
  mutate(club_name = replace(club_name, club_name == 'CD Legans', 'CD Leganes')) %>%
  mutate(club_name = replace(club_name, club_name == 'US Crteil-Lusitanos', 'US Creteil-Lusitanos')) %>%
  mutate(club_name = replace(club_name, club_name == 'US Orlans Loiret Football', 'US Orleans Loiret Football')) %>%
  mutate(club_name = replace(club_name, club_name == 'US Quevilly Rouen Mtropole', 'US Quevilly Rouen Metropole')) %>%
  mutate(club_name = replace(club_name, club_name == 'VfL Osnabrck', 'VfL Osnabruck'))
  
write.xlsx(Appended_Expanded,paste0(Output, "Player Data.xlsx"))


Unique_Clubs <- Appended_Expanded %>%
  select(c(club_name)) %>%
  unique()
write.xlsx(Unique_Clubs,paste0(Crosswalk, "Player Clubs.xlsx"))

Tab <-  Appended_Expanded %>%
  select( club_name, club_nameutf) %>%
  group_by(club_name, club_nameutf) %>%
  mutate(total = 1) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  filter(club_nameutf != club_name)
