# Load packages
library(dplyr)
library(knitr)
library(nycflights13)

View(flights) # Excel type sheet

head(flights) # First 6 rows

glimpse(flights) # from dplyr

knitr::kable(mtcars) # better for html, pdf, word output

# preview of next session
# What is the average arrival delay on flights to SEA

seatac <- flights %>% # %>% is a pipe operator: "and then", <- is the assignment operator for objects (variables, vectors, data frames, etc)
  filter(dest == "SEA") # selects only flights that are going to Seattle

# base-r
mean(seatac$arr_delay, na.rm = TRUE) # na.rm removes missing values from the calculation

# tidyverse
seatac %>% summarize(mean(arr_delay, na.rm = TRUE))

#One advantage of tidyverse syntax is that we can string together multiple summaries in one code block instead of having to make individual calculations. We can also easily calculate based on a different variable.
seatac %>% group_by(origin) %>% # "splits" the seatac data set into sub data sets for each origin airport
  summarize(mean(arr_delay, na.rm = TRUE),
                     sd(arr_delay, na.rm = TRUE)) # calculates the mean and standard deviation 

# loading external data
pierce_faculty <- readxl::read_xlsx("pierce_faculty.xlsx")
