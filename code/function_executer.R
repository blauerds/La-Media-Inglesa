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
font_add_google(name = "Barlow", family = "barlow")
showtext_auto()

# get the time before runnng code
before_time <- Sys.time()

# executing functions to put players percentile plots on the drive
get_players_urls_big5(curr_szn = 2023, leagues = "ENG")
drive_plot_saver(language_def = "ESP", logo_in_plot = TRUE)

# executing functions to create shot location plots
shot_locations(player_id = 618, match_date = "2014-12-29", label_type = "All")

# calculate the running time of the code
end_time <- Sys.time()
lasting_time <- before_time - end_time