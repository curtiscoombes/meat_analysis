# Read in the libraries
library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(here)
library(usethis)

# -----------------------------------------------------------------------------
# Read in the data
meat <- read_csv(here("Data", "Raw", "meat_consumption.csv"))

# Initialise git repository
use_git()
use_github()

# View the data
head(meat)
str(meat)

# ------------------------------------------------------------------------------
# Clean the data's names using Janitor
meat <- meat|>
  clean_names()|>
  filter(time < 2026)|>
  filter(value > 1)

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

# ------------------------------------------------------------------------------

# Let's filter the data to only Great Britain, and only the ruminants as the unit
# is different for ruminents and for poultry. Also, given the multiple recorded
# and given that there's multiple, kinda conflicting entries per year, let's
# find the average for them
ruminant_filt_gbr <- meat|>
  group_by(subject, time)|>
  filter(location == "GBR")|>
  filter(subject %in% c("BEEF", "SHEEP", "PIG"))|>
  summarise(year_avg = mean(value))|>
  ungroup()

## Create a line plot which shows the average consumption per year, per ruminant
ggplot(ruminant_filt_gbr, aes(x = time, y = year_avg, color = subject))+
  geom_line(size = 1)+
  labs(x = "Year", y = "Measured in thousand of tonnes of carcass weight (average)", color = "Subject", title = "Amount of Chicken Consumed in the UK between 1990 and 2026")+
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5), 
    minor_breaks = seq(1990, 2025, by = 1)) +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(r = 10, l = 10), size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, b = 10), size = 14), 
        legend.title = element_text(size = 14),
        plot.title = element_text(margin = margin(t = 20, b = 20), hjust = 0.5, size = 20))

# Again, filter for GBR but now for poultry, finding the average
poultry_filt_gbr <- meat|>
  group_by(subject, time)|>
  filter(location == "GBR")|>
  filter(subject == "POULTRY")|>
  summarise(year_avg = mean(value))|>
  ungroup()

## Create a line plot which shows the average consumption per year
ggplot(poultry_filt_gbr, aes(x = time, y = year_avg, color = subject))+
  geom_line(size = 1)+
  labs(x = "Year", y = "Ready-to-cook weight (average)", color = "Subject", title = "Amount of Chicken Consumed in the UK between 1990 and 2026")+
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 1), 
    minor_breaks = seq(1990, 2025, by = 1)) +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(r = 10, l = 10), size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, b = 10), size = 14), 
        legend.title = element_text(size = 14),
        plot.title = element_text(margin = margin(t = 20, b = 20), hjust = 0.5, size = 20))

