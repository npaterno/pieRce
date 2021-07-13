# Load Packages
library(dplyr)
library(openintro)
library(ggplot2)

# View dataset
View(gpa_study_hours)

# View help file
?gpa_study_hours

# Chapter 3: Data Wrangling
# <- "assignment", %>% "pipe"

# Task 1: Arrange descending gpa
# arrange goes least to greatest by default
df_1 <- gpa_study_hours %>%
  arrange(-gpa) 

# Task 2: Arrange descending study_hours for gpa > 3.5
# filter() "filters to a subset of data"
df_2 <- gpa_study_hours %>% 
  filter(gpa > 3.5) %>% 
  arrange(-study_hours)

# Task 3: Calculate the average study_hours for ranges of gpa
# mutate() "mutates the dataset by creating a new variable"
# case_when() "defines a variabled based on another"
# group_by() "condenses dataset by given variable"
# summarize() "calculate summary statistics for the group_by variable
df_3 <- gpa_study_hours %>% 
  mutate(gpa_range = case_when(
    gpa < 3.0 ~ "2.500 - 2.999",
    gpa < 3.5 ~ "3.000 - 3.499",
    gpa < 4.0 ~ "3.500 - 3.999",
    gpa == 4.0 ~ "4.000"
    )) %>% 
  group_by(gpa_range) %>% 
  summarize(mean_study = mean(study_hours),
            mean_gpa = mean(gpa))


# Chapter 2: Data Visualization
# ggplot(data = name_of_dataset, mapping = aes(x = variable_1, y = variable_2)) +
# geom_*() where * is a type of graph

# Scatterplot

## Base plot
ggplot(data = gpa_study_hours, 
       mapping = aes(x = study_hours, y = gpa))+
  geom_point()

## Detailed plot
ggplot(data = gpa_study_hours, 
       mapping = aes(x = study_hours, y = gpa))+
  geom_point(alpha = 0.25, # alpha = transparency
             size = 2, # size changes the size of the points 
             color = "deeppink", # changes color of the points
             shape = 10 # changes shape of the points
  )

# Histograms

## Base plot
ggplot(data = gpa_study_hours, 
       mapping = aes(x = gpa))+
  geom_histogram()

## Detailed plot
ggplot(data = gpa_study_hours, 
       mapping = aes(x = gpa))+
  geom_histogram(binwidth = 0.2, # binwidth is the interval length for the classes
                 color = "white", # change border color of the bars
                 fill = "goldenrod", # changes the bar color,
                 size = 1.5 # changes thickness of border
                 )

# Boxplots

## Base plot
ggplot(gpa_study_hours,
       aes(study_hours))+
  geom_boxplot()

## Detailed plot
# color, fill, size behave like with a histogram
# can change color, size, shape of outliers
ggplot(gpa_study_hours,
       aes(study_hours))+
  geom_boxplot(color = "deeppink",
    fill = "springgreen",
    size = 1.2,
    outlier.color = "firebrick1", 
    outlier.shape = 8, 
    outlier.size = 2
  )
  # coord_flip() # flips x-y axes


# Bar plot: data = diamonds

## Base plot
ggplot(diamonds, 
       aes(clarity))+
  geom_bar()

## Detailed plot
ggplot(diamonds, 
       aes(clarity))+
  geom_bar( aes(fill = cut), 
           position = "dodge")+ # side-by-side not stacked
  facet_wrap(~cut) # different graph for each level of the facet variable