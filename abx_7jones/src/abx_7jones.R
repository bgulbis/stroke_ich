library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "abx_7jones")

raw_icu_admissions <- read_excel(paste0(f, "raw/icu_admissions.xlsx"), skip = 9) |>
    rename_all(str_to_lower)

raw_antibiotics <- read_excel(paste0(f, "raw/antibiotics.xlsx"), skip = 9) |>
    rename_all(str_to_lower)
