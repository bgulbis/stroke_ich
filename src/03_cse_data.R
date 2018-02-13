library(tidyverse)
library(lubridate)
library(readxl)
library(edwr)

dir_raw <- "data/raw/cse"

pts <- read_excel("data/raw/cse/patient_list.xlsx") %>%
    rename(fin = `FIN#`) %>%
    mutate_at("fin", str_trim)

mbo_fin <- concat_encounters(pts$fin)

# run MBO query
#   * Identifiers - by FIN
