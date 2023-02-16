library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(magick)
library(worldfootballR)
library(stringr)
library(googledrive)
library(showtext)
library(roxygen2)
library(lubridate)
library(raster)
library(grid)
library(png)
library(ggimage)
library(lubridate)
font_add_google(name = "Barlow", family = "barlow")
showtext_auto()

# function to generate a shot locations plot, with LMI logo added
shot_locations <- function(player_id = NULL, match_date = NULL, label_type = "All_NO"){
  # corroborate that the label_type argument is correct
  allowed_label_type <- c("All", "All_NO", "None")
  if (!label_type %in% allowed_label_type) {
    stop(sprintf("The argument 'label_type' must be one of the following values: %s",
                 paste(allowed_label_type, collapse = ", ")))
  }
  
  # generate player's understat url based on the player id
  player_link <- paste("https://understat.com/player/", player_id, sep = "")
  
  # extract player's shots
  player_shots <- understat_player_shots(player_url = player_link)
  
  # remove the time in the date
  player_shots$date <- substr(player_shots$date, 1, 10)
  
  # filter by date
  mapped_shots <- player_shots %>% filter(date == match_date)
  
  # create a column to determine the desired size of the point, based on the quality (xG)
  mapped_shots <- mapped_shots %>% mutate(size_ball = case_when(
    (round(as.numeric(xG), 1) <= 0.1) ~  1,
    (round(as.numeric(xG), 1) > 0.1) & (round(as.numeric(xG), 1) <= 0.2) ~ 2,
    (round(as.numeric(xG), 1) > 0.2) & (round(as.numeric(xG), 1) <= 0.5) ~ 3,
    (round(as.numeric(xG), 1) > 0.5) ~ 4
  ))
  
  # change the names of the values in the column "result" to make them more readable
  mapped_shots <- mapped_shots %>% mutate(result = case_when(
    result == "SavedShot" ~ "Saved",
    result == "Goal" ~ "Goal",
    result == "MissedShots" ~ "Missed",
    result == "BlockedShot" ~ "Blocked",
    result == "ShotOnPost" ~ "On Post"
  ))
  
  # hex codes for every outcome of the goal
  group.colors <- c("Saved" = "#497EBF",
                    "Goal" = "#329536",
                    "Missed" = "#D01F3D",
                    "Blocked" = "#9B57CB",
                    "On Post" = "#FCDF29")
  
  # read in the image of the football field
  pitch_img <- rasterGrob(readPNG("figs/view-of-football-field.png"), interpolate = T)
  
  # define the dimensions of the image
  img_height <- 2978
  img_width <- 2234
  
  # round the xG column before labeling
  mapped_shots$xG <- round(mapped_shots$xG, 2)
  
  # make plot with pitch image as background
  plt <- ggplot(mapped_shots, aes(x = Y * img_width, y = X * img_height)) +
    annotation_custom(pitch_img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point(aes(size = as.numeric(size_ball), color = result, fill = result), alpha = 0.4) +
    scale_size_identity() +
    xlim(img_width, 0) + ylim(0, img_height) +
    coord_equal() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none") +
    scale_fill_manual(values = group.colors, name = NULL) +
    scale_color_manual(values = group.colors, name = NULL)
  
  # make xG numeric
  mapped_shots$xG <- as.numeric(mapped_shots$xG)
  
  # based on the label_type argument, add a label shwing the xG values
  if (label_type == "All") {
    plt <- plt + geom_text(
      aes(label = xG),
      nudge_y = ifelse(round(mapped_shots$xG, 1) <= 0.2, -40,
                       ifelse(round(mapped_shots$xG, 1) > 0.2 &
                                round(mapped_shots$xG, 1) <= 0.5, -50,
                              ifelse(round(mapped_shots$xG, 1) > 0.5, -60, -40))),
      size = 7,
      color = "#FFFFFF", family = "barlow"
    )
  } else if (label_type == "All_NO") {
    plt <- plt + geom_text(
      aes(label = xG),
      nudge_y = ifelse(round(mapped_shots$xG, 1) <= 0.2, -40,
                       ifelse(round(mapped_shots$xG, 1) > 0.2 &
                                round(mapped_shots$xG, 1) <= 0.5, -50,
                              ifelse(round(mapped_shots$xG, 1) > 0.5, -60, -40))),
      check_overlap = T,
      size = 7,
      color = "#FFFFFF", family = "barlow"
    )
  }
  
  # save the image, cropping the white border and the defensive half of the pitch
  ggsave("figs\\shots_plot.png", plt, width = 2234, height = 2978, units = "px")
  cropped_plot <- image_crop(image_read("figs/shots_plot.png"), "2160x1469+40+54")
  image_write(cropped_plot, "figs/shots_plot.png")
  img <- image_read("figs/shots_plot.png")
  
  # determine the name of the rival and own team based on the home_away column
  rival_team <- ifelse(mapped_shots$home_away[1] == "h",mapped_shots$away_team[1],
         ifelse(mapped_shots$home_away[1] == "a",mapped_shots$home_team[1]))
  own_team <- ifelse(mapped_shots$home_away[1] == "h",mapped_shots$home_team[1],
                       ifelse(mapped_shots$home_away[1] == "a",mapped_shots$away_team[1]))
  
  # set title to add to the image
  title <- paste(mapped_shots$player[1],
        paste("(", toupper(substr(own_team, 1, 3)), ")", sep = ""),
        "vs.",
        rival_team,
        " - ",
        format(ymd(mapped_shots$date[1]), "%d de %B de %Y"))
  
  # add title to the image + the legend with credits and source
  img_with_title <- image_annotate(img, title, size = 45, font = "Helvetica",
                                   gravity = "north", color = "white") %>%
    image_annotate("@blauerds" , size = 30, font = "Helvetica",
                   gravity = "north", color = "white", location = "-940+1300") %>%
    image_annotate("https://github.com/blauerds", size = 30, font = "Helvetica",
                   gravity = "north", color = "white", location = "-820+1340") %>%
    image_annotate("Source: Understat", size = 30, font = "Helvetica",
                   gravity = "north", color = "white", location = "-892+1380")
  
  
  # save image with text
  image_write(img_with_title, "figs/shots_plot.png")
}





  