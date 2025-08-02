# Load packages 
library(dplyr)
library(ggplot2)

# Read script
glasto <- read.csv("/Users/hannah/Desktop/glasto/glasto_gender.csv")

# Calculate gender proportions per year
gender_proportions <- glasto %>%
  group_by(Year, Gender) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Year) %>%
  mutate(Proportion = round(Count / sum(Count) * 100)) %>%  
  select(Year, Gender, Proportion) %>%
  arrange(Year, Gender)

# Generate plot
gender_proportions <- gender_proportions %>%
  mutate(Gender = factor(Gender, levels = c("M", "F", "NB")))

year_labels <- sort(unique(gender_proportions$Year))

ggplot(gender_proportions, aes(x = Year, y = Proportion, fill = Gender)) +
  geom_area(position = "identity", alpha = 1, color = NA) +
  scale_fill_manual(
    values = c("M" = "#c85002", "F" = "#00aeef", "NB" = "#284d46")
  ) +
  scale_x_continuous(breaks = year_labels) +
  labs(
    title = "Gender representation across the 5 main stages at Glastonbury, 2015-2025",
    x = "Year",
    y = "Proportion (%)",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),           
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),  
    panel.grid.minor.y = element_blank()
  )
