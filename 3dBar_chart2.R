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

# Create animated bar chart without the Sector fill
p <- ggplot(startup, aes(x = reorder(Company, `Valuation ($B)`), y = `Valuation ($B)`)) +
  geom_col() +
  coord_flip() +
  transition_time(Year) +
  labs(title = "Startup Valuation Over Time: {frame_time}", x = "Company", y = "Valuation ($B)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Render animation
# Render animation with a larger image size
animate(p, renderer = gifski_renderer(), width = 1600, height = 2000)

# Install and load plotly
install.packages("plotly")
library(plotly)

# Create interactive 3D scatter plot
# Create interactive 3D scatter plot
fig <- plot_ly(startup, 
               x = ~`Entry Valuation^^ ($B)`,  # Corrected column name with backticks
               y = ~`Valuation ($B)`,          # Corrected column name with backticks
               z = ~Year, 
               text = ~Company, 
               type = "scatter3d", 
               mode = "markers",
               marker = list(size = 6, color = ~`Valuation ($B)`, colorscale = "Viridis"))

# Show plot
fig


