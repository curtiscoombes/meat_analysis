# Read in the libraries
library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(here)
library(usethis)

# Read in the data
meat <- read_csv(here("Data", "Raw", "meat_consumption.csv"))

# Initialise git repository
use_git()
use_github()

# View the data
head(meat)
str(meat)

# Clean the data's names using Janitor
meat <- meat|>
  clean_names()

# Understanding the individual countries, as some may overlap
meat|>
  distinct(location)|>
  print(n = Inf)

# The previous step showed overlap (the world total, OECD, EU27, BRICS)
# We need to filter those out
meat|>
  filter(!location %in% c("WLD", "OECD", "EU27", "BRICS"))|>
  group_by(location)|>
  summarise(mean = mean(value))|>
  arrange(desc(mean))

# Let's filter the data to only Great Britain, and given the multiple recorded
# and given that there's multiple, kinda conflicting entries per year, let's
# find the average for them
meat_filtered_gbr <- meat_filtered|>
  group_by(subject, time)|>
  filter(location == "GBR")|>
  summarise(year_avg = mean(value))
  

## Create a line plot which shows the average consumption per year, per animal
ggplot(meat_filtered_gbr, aes(x = time, y = year_avg, color = subject))+
  geom_line()+
  labs(x = "Year", y = "Average Amount Per Year")