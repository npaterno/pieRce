# Load Packages
library(tidyverse)
library(moderndive)
library(skimr)
library(palmerpenguins)

# View Data
glimpse(penguins)

# One Numeric 

## Wrangle Data for plot
plot_data <- penguins %>% 
  select(bill_length_mm, body_mass_g, species) %>% 
  na.omit()

summary_plot_data <- plot_data %>% 
  skim()

## Plot
ggplot(plot_data, aes(bill_length_mm, body_mass_g))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

## Check correlation
get_correlation(plot_data, body_mass_g ~ bill_length_mm)

## Fit Model
bill_mass_model <- lm(body_mass_g ~ bill_length_mm, data = plot_data)

get_regression_table(bill_mass_model)
get_regression_points(bill_mass_model)

# One Categorical

## Wrangle data
cat_data <- penguins %>% 
  select(sex, species, body_mass_g) %>% 
  na.omit()

## Plot 
ggplot(cat_data, aes(sex, body_mass_g))+
  geom_boxplot()

ggplot(cat_data, aes(body_mass_g))+
  geom_histogram()+
  facet_wrap(~sex)

ggplot(cat_data, aes(species, body_mass_g))+
  geom_boxplot()

ggplot(cat_data, aes(body_mass_g))+
  geom_histogram()+
  facet_wrap(~species)

## Fit Model
species_mass_model <- lm(body_mass_g ~ species, data = cat_data)

get_regression_table(species_mass_model)
get_regression_points(species_mass_model)

# Multiple Regression: interaction model
bill_species_mass_model <- lm(body_mass_g ~ bill_length_mm * species, data = penguins %>% na.omit())
get_regression_table(bill_species_mass_model)

# \hat{y} = 66.5 + 93.7*bill_length_mm + 780*1_is.chinstrap-94.3*1_is.gentoo-34.6*1_is.chinstrap*bill_length_mm+13.9*1_is.gentoo*bill_length_mm

# Plot
ggplot(penguins %>% na.omit(), aes(bill_length_mm, body_mass_g, color = species))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


## Iterating to get individual models (equiv of interaction model)
species_models <- penguins %>%
  na.omit() %>% 
  select(species, body_mass_g, bill_length_mm) %>% 
  group_by(species) %>% 
  nest(data = c(body_mass_g, bill_length_mm)) %>% 
  mutate(fit = map(data, ~lm(.$body_mass_g ~ .$bill_length_mm, data = .))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  group_by(species, fit) %>% 
  summarize(count = n())

species_models$fit %>% 
  map(summary) %>% 
  map_df(broom::tidy) %>% 
  mutate(species = c(rep("Adelie", 2), rep("Chinstrap",2), rep("Gentoo", 2)), .before = term)

get_correlation(data = mult_data %>% group_by(species), 
                formula = body_mass_g ~ bill_length_mm)