library(tidyverse)
library(dplyr)
library(worldfootballR)
library(ggplot2)
library(ggrepel)

#### NOT IMPORTANT ####
# general data for later
mapped_players <- player_dictionary_mapping()
stat_types <- c("shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time",
                "misc", "keepers", "keepers_adv")
stats_percentiles <- c("Non-Penalty Goals", "Non-Penalty xG", "Shots on target", "Goals/Shot",
                       "xAG", "Passes Completed", "Pass Completion % (Short)", "Pass Completion % (Medium)",
                       "Pass Completion % (Long)", "Shot-Creating Actions", "Tackles", "Blocks", "Interceptions",
                       "Successful Take-Ons", "Progressive Carries", "Progressive Passes Rec", "Progressive Passes")
current_season_end_year <- 2023

# big5 player's URLs
all_players_big5 <- c()
all_leagues_URLs <- fb_league_urls(country = c("ENG", "ESP", "GER", "ITA", "FRA"),
                          gender = "M", season_end_year = current_season_end_year, tier = '1st')
for (league in all_leagues_URLs) {
  all_teams_URLs <- fb_teams_urls(league)
  for (team in all_teams_URLs) {
    all_players_URLs <- fb_player_urls(team)
    team <- sub("-Stats", "", team)
    team <- substr(team, 38, nchar(team))
    
    players_df <- data.frame("Team" = rep(team, length(all_players_URLs)),
                     "Player" = all_players_URLs)
  }
  all_players_big5 <- rbind(all_players_big5, players_df)
}




#### CODE - PLAYER SCOUTING REPORT ####

# wait for user to insert the link of the player
playerlink <- readline(prompt = "Insert player's FBref URL: ")


# get player's scouting report
player_scouting_report <- fb_player_scouting_report(player_url = playerlink, pos_versus = "primary", league_comp_name = "Last 365 Days Men's Big 5 Leagues, UCL, UEL")

# filter to show the report data of the statistics we want, and remove duplicated stats
player_percentiles_data <- player_scouting_report %>%
  filter(Statistic %in% stats_percentiles) %>%
  drop_na()
player_percentiles_data <- player_percentiles_data %>% filter(StatGroup != "Standard")
player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]

# change statistics names (to make the plot more readable)
for (row in seq(1:nrow(player_percentiles_data))) {
  if (player_percentiles_data$Statistic[row] == "Non-Penalty Goals") {
    player_percentiles_data$Statistic[row] <- "np:Gls"
  } else if (player_percentiles_data$Statistic[row] == "Non-Penalty xG") {
    player_percentiles_data$Statistic[row] <- "np:xG"
  } else if (player_percentiles_data$Statistic[row] == "Shots on target") {
    player_percentiles_data$Statistic[row] <- "SoT"
  } else if (player_percentiles_data$Statistic[row] == "Goals/Shot") {
    player_percentiles_data$Statistic[row] <- "Gls/Sh"
  } else if (player_percentiles_data$Statistic[row] == "xAG") {
    player_percentiles_data$Statistic[row] <- "xAG"
  } else if (player_percentiles_data$Statistic[row] == "Passes Completed") {
    player_percentiles_data$Statistic[row] <- "Cmp"
  } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Short)") {
    player_percentiles_data$Statistic[row] <- "Short %"
  } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Medium)") {
    player_percentiles_data$Statistic[row] <- "Med. %"
  } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Long)") {
    player_percentiles_data$Statistic[row] <- "Long %"
  } else if (player_percentiles_data$Statistic[row] == "Shot-Creating Actions") {
    player_percentiles_data$Statistic[row] <- "SCA"
  } else if (player_percentiles_data$Statistic[row] == "Tackles") {
    player_percentiles_data$Statistic[row] <- "Tkl"
  } else if (player_percentiles_data$Statistic[row] == "Blocks") {
    player_percentiles_data$Statistic[row] <- "Blocks"
  } else if (player_percentiles_data$Statistic[row] == "Interceptions") {
    player_percentiles_data$Statistic[row] <- "Int"
  } else if (player_percentiles_data$Statistic[row] == "Successful Take-Ons") {
    player_percentiles_data$Statistic[row] <- "Drib Cmp"
  } else if (player_percentiles_data$Statistic[row] == "Progressive Carries") {
    player_percentiles_data$Statistic[row] <- "Prog Carries"
  } else if (player_percentiles_data$Statistic[row] == "Progressive Passes") {
    player_percentiles_data$Statistic[row] <- "Prog Passes"
  } else if (player_percentiles_data$Statistic[row] == "Progressive Passes Rec") {
    player_percentiles_data$Statistic[row] <- "Prog Rec"
  }
  
  if (player_percentiles_data$StatGroup[row] == "Goal and Shot Creation") {
    player_percentiles_data$StatGroup[row] <- "Shooting"
  }
}

# order by stat group
player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]

# add id for each stat group
player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
  StatGroup == "Defense" ~ 1,
  StatGroup == "Passing" ~ 2,
  StatGroup == "Possession" ~ 3,
  StatGroup == "Shooting" ~ 4
),
season = paste(current_season_end_year-1, "/", current_season_end_year, sep=""))

# save player percentiles
save(player_percentiles_data, file="rda/player_percentiles_data.rda")
