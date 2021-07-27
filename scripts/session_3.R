# Load Packages
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)

# Load Data
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# Wrangle Data
plot_data <- olympics %>% 
  select(c(noc, year, season, medal)) %>% 
  mutate(
    bronze = case_when(
      medal == "Bronze" ~ 1, 
      TRUE ~ 0
    ), 
    silver = case_when(
      medal == "Silver" ~ 1, 
      TRUE ~ 0
    ),
    gold = case_when(
      medal == "Gold" ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  select(-medal) %>% 
  group_by(noc, year, season) %>% 
  summarize(
    bronze = sum(bronze),
    silver = sum(silver), 
    gold = sum(gold)
  ) %>% 
  pivot_longer(
    cols = c("bronze", "silver", "gold"), 
    names_to = "medal", 
    values_to = "count"
  ) %>% 
  mutate(medal = ordered(
    as.factor(medal),
    levels = c("bronze", "silver", "gold")))

# Plot of US Medal Counts by Year

ggplot(plot_data %>% filter(noc == 'USA'), aes(year, count, fill = season))+
  geom_col(position = "dodge")+
  facet_wrap(~medal)+
  labs(
    title = "US Olympic Medal Counts",
    x = "Year", 
    y = "Number of Medals",
    fill = "Season", 
    caption = "Source: Kaggle"
  )+
  theme_wsj()


# Scatterplot of US medals by season

ggplot(plot_data %>% filter(noc == "USA"), aes(x = year, y = season, color = medal, size = count))+
  geom_jitter()+
  scale_color_manual(values = c("#A77044", "#D7D7D7", "#D6AF36"))+
  labs(
    title = "Olympic Medal Count by Year and Season",
    subtitle = "US Medals",
    color = "Medal",
    size = "Number of Medals",
    caption = "Source: Kaggle",
    x = "Year",
    y = "Season"
  )+
  theme(
    legend.position = "bottom", 
    legend.background = element_rect(fill = "lightblue"),
    legend.box.background = element_rect(fill = "lightblue"),
    legend.key = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "lightblue"),
    panel.background = element_rect(fill = "white",
                                    color = "#C42032"),
    text = element_text(color = "#C42032")
  )




