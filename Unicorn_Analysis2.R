library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(caret)
library(lmtest)

# Load the dataset
startup <- read_csv("startup.csv")

# Data Cleaning
startup_clean <- startup %>%
  filter(!is.na(`Valuation ($B)`) & !is.na(Sector)) %>%
  distinct()

startup_clean$`Valuation ($B)` <- as.numeric(gsub("[^0-9.]", "", startup_clean$`Valuation ($B)`))
startup_clean$`Entry Valuation^^ ($B)` <- as.numeric(gsub("[^0-9.]", "", startup_clean$`Entry Valuation^^ ($B)`))

# Correlation Analysis
correlation <- cor(startup_clean %>% 
  select(`Valuation ($B)`, `Entry Valuation^^ ($B)`), use = "complete.obs")
print("Correlation Matrix:")
print(correlation)

# Hypothesis Testing: ANOVA to compare valuations across sectors
anova_results <- aov(`Valuation ($B)` ~ Sector, data = startup_clean)
summary(anova_results)

# Clustering Analysis
# Select numeric columns for clustering
clustering_data <- startup_clean %>% 
  select(`Valuation ($B)`, `Entry Valuation^^ ($B)`) %>% 
  na.omit()

# Scale the data
clustering_data_scaled <- scale(clustering_data)

# Determine the optimal number of clusters
fviz_nbclust(clustering_data_scaled, kmeans, method = "wss")

# Apply k-means clustering
set.seed(123)
kmeans_result <- kmeans(clustering_data_scaled, centers = 3, nstart = 25)
startup_clean$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
fviz_cluster(kmeans_result, data = clustering_data_scaled, geom = "point", ellipse.type = "convex") +
  labs(title = "Clustering of Startups", x = "Feature 1", y = "Feature 2")

# Time-Series Analysis: Growth of Unicorns over Time
startup_clean$Year <- as.numeric(sub(".*(\\d{4}).*", "\\1", startup_clean$Entry))
yearly_growth <- startup_clean %>%
  group_by(Year) %>%
  summarise(Unicorn_Count = n())

# Line Plot for Time-Series
ggplot(yearly_growth, aes(x = Year, y = Unicorn_Count)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Growth of Unicorn Startups Over Time",
       x = "Year",
       y = "Number of Unicorns") +
  theme_minimal()

# Predictive Modeling: Regression Analysis
# Train-test split
set.seed(123)
data_split <- createDataPartition(startup_clean$`Valuation ($B)`, p = 0.8, list = FALSE)
train_data <- startup_clean[data_split, ]
test_data <- startup_clean[-data_split, ]

# Build the model
lm_model <- lm(`Valuation ($B)` ~ `Entry Valuation^^ ($B)` + Sector, data = train_data)
summary(lm_model)

# Predict on test data
predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model
mse <- mean((predictions - test_data$`Valuation ($B)`)^2)
cat("Mean Squared Error:", mse, "\n")

# Check residuals
bptest(lm_model)