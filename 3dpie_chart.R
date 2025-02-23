

library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)

# Calculate the proportion of each sector over time
sector_data <- startup %>%
  group_by(Year, Sector) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))  # Calculate the proportion for each sector

# Calculate the position for the text labels (middle of each pie slice)
sector_data <- sector_data %>%
  arrange(Year, desc(proportion)) %>%
  group_by(Year) %>%
  mutate(cumsum = cumsum(proportion) - 0.5 * proportion,  # Get the cumulative sum for label positions
         label_position = cumsum)  # Position of the labels

# Create the animated pie chart with sector labels
p <- ggplot(sector_data, aes(x = "", y = proportion, fill = Sector, text = Sector)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to pie chart
  transition_time(Year) +  # Animate over years
  labs(title = "Sector Distribution Over Time: {frame_time}") +
  geom_text(aes(y = label_position, label = Sector), color = "white", size = 3, fontface = "bold") +  # Reduced label size
  theme_void() +  # Clean theme
  theme(legend.position = "none")  # Hide legend for cleaner look

# Render the animation
animate(p, renderer = gifski_renderer(), width = 800, height = 800)
