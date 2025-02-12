library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "stat_epi_sophie")

raw_pts <- read_excel(paste0(f, "raw/patients.xlsx")) |> 
    rename_all(str_to_lower) 

mbo_id <- concat_encounters(raw_pts$encntr_id)
print(mbo_id)
