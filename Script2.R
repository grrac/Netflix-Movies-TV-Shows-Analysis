library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

netflix_titles <- read.csv("netflix_titles.csv")

# Check structure
glimpse(netflix_titles)
summary(netflix_titles)

# Handle missing values like NA or ""
netflix_titles <- netflix_titles %>%
  mutate(
    cast = ifelse(cast == "" | is.na(cast), "Unknown", cast),
    country = ifelse(country == "" | is.na(country), "Unknown", country),
    director = ifelse(director == "" | is.na(director), "Unknown", director), 
  )

# Split country column and extract the first element 
netflix_titles <- netflix_titles %>%
  mutate(
    country = str_split(country, ",") %>% map_chr(1) %>% str_trim()
  )

# Check structure
glimpse(netflix_titles)
summary(netflix_titles)

# Movies vs. TV Shows
type_count <- netflix_titles %>%
  count(type) %>%
  mutate(percentage = n / sum(n) * 100)

# Top 10 Countries
top_countries <- netflix_titles %>%
  count(country, sort = TRUE) %>%  # Count titles by country, sort descending
  filter(country != "Unknown") %>%  # Remove rows with unknown country
  head(10)                         # Keep top 10 countries

pie_chart <- ggplot(type_count, aes(x = "", y = percentage, fill = type)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(
    aes(label = paste0(round(percentage), "%")),  # Add percentage labels
    position = position_stack(vjust = 0.5),       # Center labels
    color = "white",                              # White text for visibility
    size = 5                                      # Adjust font size
  ) +
  labs(title = "Movies vs. TV Shows") +
  theme_void()

bar_plot <- ggplot(top_countries, aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries",
    x = "Country",         
    y = "Number of Titles"  
  ) +
  theme_minimal()

pie_chart + plot_spacer() + bar_plot + 
  plot_layout(
    widths = c(1, 0.2, 2),  # Second plot is wider
  )