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
