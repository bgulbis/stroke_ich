library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)
# library(themebg)
# library(broom)

f <- set_data_path("stroke_ich", "sah_roc")

raw_screen <- read_excel(paste0(f, "raw/sah_patients.xlsx")) |> 
    rename_all(str_to_lower) 

df_pts <- raw_screen |> 
    select(fin, vent = `mechanical ventilation`) |> 
    mutate(across(fin, str_replace_all, pattern = "-", replacement = "")) |> 
    filter(
        !is.na(fin),
        vent == "Yes"
    )

mbo_fin <- concat_encounters(df_pts$fin)
print(mbo_fin)
