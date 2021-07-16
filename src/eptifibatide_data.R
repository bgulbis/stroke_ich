library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)

demog <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "baseline") %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    mutate(across(c(weight, hgb, platelets, scr, egfr, nihss), as.numeric))

home_meds <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "home_meds") %>%
    rename_all(str_to_lower) %>%
    distinct()

locations <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "locations") %>%
    rename_all(str_to_lower) %>%
    distinct()

meds <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "meds") %>%
    rename_all(str_to_lower) %>%
    distinct()

labs_vitals <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "labs_vitals") %>%
    rename_all(str_to_lower) %>%
    distinct()

transfusions <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "transfusions") %>%
    rename_all(str_to_lower) %>%
    distinct()

