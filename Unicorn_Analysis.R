library(readr)
library(ggplot2)
library(dplyr)

# Load the dataset
startup <- read_csv("startup.csv")
View(startup)

# Remove missing values (using appropriate column names)
startup_clean <- startup %>%
  filter(!is.na(`Valuation ($B)`) & !is.na(Sector))

# Remove duplicate rows
startup_clean <- startup_clean %>%
  distinct()

# Convert 'Valuation ($B)' column to numeric (removing non-numeric characters)
startup_clean$`Valuation ($B)` <- as.numeric(gsub("[^0-9.]", "", startup_clean$`Valuation ($B)`))

# Remove outliers in 'Valuation ($B)' using the IQR method
Q1 <- quantile(startup_clean$`Valuation ($B)`, 0.25, na.rm = TRUE)
Q3 <- quantile(startup_clean$`Valuation ($B)`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

startup_clean <- startup_clean %>%
  filter(`Valuation ($B)` >= (Q1 - 1.5 * IQR) & `Valuation ($B)` <= (Q3 + 1.5 * IQR))

# Normalize or scale the 'Valuation ($B)' column
startup_clean$`Valuation ($B)` <- (startup_clean$`Valuation ($B)` - min(startup_clean$`Valuation ($B)`, na.rm = TRUE)) / 
  (max(startup_clean$`Valuation ($B)`, na.rm = TRUE) - min(startup_clean$`Valuation ($B)`, na.rm = TRUE))

# Check the cleaned data
head(startup_clean)

# 1. Bar Plot: Distribution of unicorns by sector
sector_count <- startup %>%
  group_by(Sector) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Bar Plot
ggplot(sector_count, aes(x = reorder(Sector, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Distribution of Unicorns by Sector",
       x = "Sector",
       y = "Number of Unicorns") +
  theme_minimal()

# 2. Pie Chart: Proportion of startups based on location
location_count <- startup %>%
  group_by(Location) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Pie Chart
ggplot(location_count, aes(x = "", y = Count, fill = Location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proportion of Unicorn Startups by Location") +
  theme_void()

# 3. Line Plot: Growth of unicorn startups over time
# Check if 'Entry' column contains date-like information
str(startup)

# If the 'Entry' column has year information, extract it
startup$Year <- as.numeric(sub(".*(\\d{4}).*", "\\1", startup$Entry))

# Check the extracted 'Year' column
head(startup$Year)

# Create the 'yearly_growth' dataframe
yearly_growth <- startup %>%
  group_by(Year) %>%
  summarise(Count = n())

# Line Plot to visualize the growth of unicorns over time
ggplot(yearly_growth, aes(x = Year, y = Count)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Growth of Unicorn Startups Over Time",
       x = "Year",
       y = "Number of Unicorns") +
  theme_minimal()
#4 
# Top 10 companies by valuation
top_companies <- startup %>%
  arrange(desc(`Valuation ($B)`)) %>%
  slice_head(n = 10)

# Bar Plot
ggplot(top_companies, aes(x = reorder(Company, `Valuation ($B)`), y = `Valuation ($B)`, fill = Sector)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Companies by Valuation",
       x = "Company",
       y = "Valuation ($B)") +
  theme_minimal()

#5 
# Count of unicorns by country
country_count <- startup %>%
  group_by(Location) %>%
  summarise(Unicorn_Count = n()) %>%
  arrange(desc(Unicorn_Count))

# Bar Plot
ggplot(country_count, aes(x = reorder(Location, -Unicorn_Count), y = Unicorn_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Number of Unicorns by Country",
       x = "Country",
       y = "Number of Unicorns") +
  theme_minimal()

#6
# Box Plot
ggplot(startup, aes(x = Sector, y = `Valuation ($B)`, fill = Sector)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Valuation Distribution by Sector",
       x = "Sector",
       y = "Valuation ($B)") +
  theme_minimal()
#  
# Count of unicorns by country and sector
heatmap_data <- startup %>%
  group_by(Location, Sector) %>%
  summarise(Unicorn_Count = n()) %>%
  ungroup()

#7 Heatmap
ggplot(heatmap_data, aes(x = Sector, y = Location, fill = Unicorn_Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Unicorn Count by Country and Sector",
       x = "Sector",
       y = "Country",
       fill = "Unicorn Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#8
# Scatter Plot: Valuation vs. Entry Valuation
ggplot(startup, aes(x = `Entry Valuation^^ ($B)`, y = `Valuation ($B)`, color = Sector)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Valuation vs. Entry Valuation",
       x = "Entry Valuation ($B)",
       y = "Valuation ($B)",
       color = "Sector") +
  theme_minimal()

#9
# Histogram
ggplot(startup, aes(x = `Valuation ($B)`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Unicorn Valuations",
       x = "Valuation ($B)",
       y = "Frequency") +
  theme_minimal()

#10
ggplot(startup, aes(x = `Valuation ($B)`, fill = Sector)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot of Valuations by Sector",
       x = "Valuation ($B)",
       y = "Density",
       fill = "Sector") +
  theme_minimal()






