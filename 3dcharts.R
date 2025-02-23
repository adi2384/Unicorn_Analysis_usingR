# Load necessary libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)

# Get the top 10 unicorn startups based on their valuation over all years
top_10_unicorns <- startup %>%
  filter(!is.na(`Valuation ($B)`)) %>%
  group_by(Year) %>%
  top_n(10, `Valuation ($B)`)

# Create animated bar chart showing valuation changes over time with simultaneous bar movement
p <- ggplot(top_10_unicorns, aes(x = reorder(Company, `Valuation ($B)`), y = `Valuation ($B)`, fill = Company)) +
  geom_col() +
  coord_flip() +
  transition_time(Year) +  # Animate over years
  labs(title = "Top Unicorn Startups: {frame_time}", x = "Company", y = "Valuation ($B)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend to avoid clutter

# Render the animation with simultaneous bars
animate(p, renderer = gifski_renderer(), width = 1500, height = 1000)
