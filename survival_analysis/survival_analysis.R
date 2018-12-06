library(tidyverse)
library(ggthemes)
library(janitor)
library(survival)

# Set plot theme
old <- theme_set(theme_tufte() + theme(text = element_text(family = "Menlo")))

# Get Data ----

# Source: Data Camp
file_url <- "https://assets.datacamp.com/production/repositories/1861/datasets/284a58cc6f9c5873937a97eca0420b980b5e5b23/survivalDataExercise.csv"
dat <- read_csv(file_url)
