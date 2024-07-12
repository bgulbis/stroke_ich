library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "tbi_pe_sophie")

raw_pts <- read_excel(paste0(f, "raw/ultrasound_cta_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    distinct(fin, .keep_all = TRUE)

mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)
