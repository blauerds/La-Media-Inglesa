plot_creator_LMI <- function(playerlink){
  library(shiny)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(cowplot)
  library(magick)
  library(worldfootballR)
  library(shinyBS)
  library(googledrive)
  
  
  # data for later
  stats_percentiles <- c("Non-Penalty Goals", "Non-Penalty xG", "Shots on target", "Goals/Shot",
                         "xAG", "Passes Completed", "Pass Completion % (Short)", "Pass Completion % (Medium)",
                         "Pass Completion % (Long)", "Shot-Creating Actions", "Tackles", "Blocks", "Interceptions",
                         "Successful Take-Ons", "Progressive Carries", "Progressive Passes Rec", "Progressive Passes")
  current_season_end_year <- 2023
  
  # get player's scouting report
  player_scouting_report <- fb_player_scouting_report(player_url = as.character(playerlink), pos_versus = "primary", league_comp_name = "Last 365 Days Men's Big 5 Leagues, UCL, UEL")
  
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
  
  # add id for each stat group and current season
  player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
    StatGroup == "Defense" ~ 1,
    StatGroup == "Passing" ~ 2,
    StatGroup == "Possession" ~ 3,
    StatGroup == "Shooting" ~ 4
  ),
  season = paste(current_season_end_year-1, "/", current_season_end_year, sep=""))
  
  
  
  ###### PLOT #####
  # get player's name and versus for the plot data
  playername <- player_percentiles_data$Player[1]
  playerversus <- player_percentiles_data$Versus[1]
  
  # group colors for the plot
  group.colors <- c("Defense" = "#C86742", "Passing" = "#84B86F",
                    "Possession" = "#E3AE2E", "Shooting" = "#6F78B3")
  
  # create the plot
  plt <- ggplot(player_percentiles_data) +
    # make custom panel grid
    geom_hline(
      aes(yintercept = y),
      data.frame(y = c(25, 50, 75, 100)),
      color = "lightgrey", linetype = "dotted"
    ) +
    
    # add bars to represent the percentiles
    geom_col(
      aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, fill = StatGroup
      ),
      position = "dodge2", show.legend = TRUE, alpha = 1
    ) +
    
    # lollipop shaf for guidance
    geom_segment(
      aes(x = reorder(str_wrap(Statistic, 5), id), y = 0,
          xend = reorder(str_wrap(Statistic, 5), id), yend = 100),
      color = "black",
      linetype = "dotted"
    ) +
    
    # make it circular
    coord_polar() +
    
    # scale y axis so bars don't start at the center
    scale_y_continuous(
      limits = c(-50, 100)
    ) +
    
    theme(
      # remove axis ticks and text
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      
      # use gray text for the labels
      axis.text.x = element_text(color = "gray12", size = 35, lineheight = 0.3,
                                 hjust = 20),
      
      # move the legend to the bottom and make it horizontal
      legend.direction = "horizontal",
      legend.position = "bottom"
    ) +
    
    # add labels
    labs(
      title = (paste("\n", playername, paste("(",player_percentiles_data$season[1], ")", sep = ""))),
      subtitle = str_wrap(paste("\n", "Statistics compared to other ", playerversus,
                                " in Men's Big 5 Leagues, UCL, UEL over the last 365 days", sep = ""), 70),
      caption = "\n Data Visualization by Lucas Varela\n@blauerds\n https://github.com/lucassvarelaa\nSource: FBref"
    ) +
    
    # add labels with the percentile values
    geom_label(aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, color = StatGroup),
               label = player_percentiles_data$Percentile, size = 10,
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    
    # customize general theme
    theme(
      # customize the text in the title, subtitle, and caption
      text = element_text(color = "grey12", size = 35),
      plot.title = element_text(face = "bold", size = 50, hjust = 0.5, lineheight = 0.3),
      plot.subtitle = element_text(size = 35, hjust = 0.5, lineheight = 0.4),
      plot.caption = element_text(size = 29, hjust = 0.5, lineheight = 0.4),
      
      # make the background white and remove extra grid lines
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank(),
      
      legend.box.just = "center"
    ) +
    
    # specify group colors manually
    scale_fill_manual(values = group.colors, name = NULL) +
    scale_colour_manual(values = group.colors, name = NULL)
  
  # save the plot
  ggsave("figs\\plot_NO_LOGO.png", plt)
  
  
  
  # get the IMAGE of the plot and the logo
  final_plot <- image_read("figs\\plot_NO_LOGO.png")
  LMI_logo <- image_read("figs\\la_media_inglesa.jpg") %>% image_resize("300x300")
  
  # calculate the offset (the position of the logo inside the plot)
  # the 0.01 and 0.99 are to put a 1% and 99% padding (respectively)
  height_off <- image_info(final_plot)$height - image_info(LMI_logo)$height -
    image_info(final_plot)$height * 0.01
  width_off <- (image_info(final_plot)$width - image_info(LMI_logo)$width) * 0.99
  offset_image <- paste("+", width_off,"+", height_off, sep = "")
  
  # plot + logo
  plot_logoed <- final_plot %>% image_composite(LMI_logo, offset = offset_image)
  
  # save the plot after the logo was added
  image_write(plot_logoed, path = "figs\\plot_ready.png", format = "png")
}


plot_creator_LMI("https://fbref.com/en/players/7aa8adfe/Alejandro-Garnacho")


# Authenticate to Google Drive API
drive_auth()

# Get the folder ID of the "LMI" folder
folder_id <- drive_find("LMI", type = "folder")$id

# Get a list of all the files in the "figs" folder
files <- list.files(path = "figs", full.names = TRUE)

# Loop through each file in the list
for (file in files) {
  # Upload the file to the "LMI" folder in Google Drive, replacing any existing file with the same name
  drive_upload(file, destfolder = folder_id, overwrite = TRUE)
}

drive_upload("figs/plot_ready.png", path = "LMI/", overwrite = T, name = "sacachispas.png")
