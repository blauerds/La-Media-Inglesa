library(ggplot2)
library(tidyverse)
library(showtext)
library(dplyr)
library(cowplot)
library(magick)
load("rda/player_percentiles_data.rda")

# install fonts
showtext_auto()

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
    title = (paste("\n", playername, paste("(",player_percentiles_data$season[1], ")", sep = ""),
                  "-", "Percentiles vs.", playerversus)),
    subtitle = "La Media Inglesa",
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
    text = element_text(color = "grey12", family = "barlow", size = 35),
    plot.title = element_text(face = "bold", size = 50, hjust = 0.5, lineheight = 0.4),
    plot.subtitle = element_text(size = 40, hjust = 0.5),
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

