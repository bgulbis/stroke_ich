library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)

pts <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "baseline") %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    mutate(across(fin, as.character))

