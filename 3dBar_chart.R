# Install necessary packages (if not already installed)
install.packages("ggplot2")
install.packages("gganimate")
install.packages("gifski")
install.packages("readr")

# Load libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(readr)

# Load the dataset
startup <- read_csv("startup.csv")

# Convert Entry column to proper date format (Extract year)
startup$Year <- as.numeric(sub(".*/", "", startup$Entry))

# Filter non-missing years
startup <- subset(startup, !is.na(Year))

# Create animated bar chart
p <- ggplot(startup, aes(x = reorder(Company, `Valuation ($B)`), y = `Valuation ($B)`, fill = Sector)) +
  geom_col() +
  coord_flip() +
  transition_time(Year) +
  labs(title = "Startup Valuation Over Time: {frame_time}", x = "Company", y = "Valuation ($B)") +
  theme_minimal()

# Render animation
animate(p, renderer = gifski_renderer())
