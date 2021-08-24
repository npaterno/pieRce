# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(moderndive)
library(infer)

# Sampling

## View the dataset and histogram of the price variable
head(diamonds)

ggplot(diamonds, aes(price))+
  geom_histogram()

## Generate a sampling distributuion with 1000 samples of size 40 and view
## the histogram of the average price for each sample
diamonds_samples <- rep_sample_n(diamonds, size = 40, reps = 1000)

diamonds_samples_summary <- diamonds_samples %>% 
  group_by(replicate) %>% 
  summarize(mean_price = mean(price))

ggplot(diamonds_samples_summary, aes(mean_price))+
  geom_histogram()+
  geom_vline(xintercept = mean(diamonds$price))

## Bootstrap samples (sampling with replacement)
## This repeats the above process with bootstrap samples
diamonds_bootstrap <- rep_sample_n(diamonds, size = 40, reps = 1000, replace = TRUE)

bootstrap_summary <- diamonds_bootstrap %>% 
  group_by(replicate) %>% 
  summarize(mean_price = mean(price))

ggplot(bootstrap_summary, aes(mean_price))+
  geom_histogram()+
  geom_vline(xintercept = mean(diamonds$price))

## Infer workflow
## This is an alternate way to calculate the bootstrap samples/summary
diamonds_bootstrap_infer <- diamonds %>% 
  specify(response = price) %>% 
  generate(size = 40, reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")

# Confidence intervals

## Using the infer package
get_confidence_interval(x = diamonds_bootstrap_infer)
get_confidence_interval(x = diamonds_bootstrap_infer, level = 0.90)

## Hard coding from raw data
## This example shows why we need to make sure we satisfy conditions 
## (i.e. have a normal dist) before calcuting a CI. Using the original
## skewed data, we have an unusable interval.
conf_data <- diamonds %>% 
  summarize(mean_price = mean(price), 
            sd_price = sd(price))

lower_ci <- conf_data$mean_price-1.96*conf_data$sd_price
upper_ci <- conf_data$mean_price+1.96*conf_data$sd_price

# Hypothesis testing
## Prep data to compare money received based on party.
## To install the Stat231 package run the line below in 
## you console: 
## remotes::install_github("npaterno/stat231")
test_data <- Stat231::piracy %>% 
  mutate(money_pro = replace_na(money_pro, replace = 0),
         money_con = replace_na(money_con, replace = 0),
         total = money_pro + money_con) %>% 
  filter(money_pro >= 0 & money_con >= 0) %>% 
  filter(party != "I") 

## Calculate the t-test
t_test(test_data, 
       formula = total ~ party, 
       order = c("D", "R"),
       mu = 0, 
       alternative = "two.sided",
       conf_int = TRUE,
       conf_level = 0.95)