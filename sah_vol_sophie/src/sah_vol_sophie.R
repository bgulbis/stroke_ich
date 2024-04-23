library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "sah_vol_sophie")

raw_pts <- read_excel(paste0(f, "raw/sah_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, \(x) str_remove_all(x, "-")))

mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)

raw_labs_vitals <- read_csv(paste0(f, "raw/labs_vitals.csv")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

df_labs <- raw_labs_vitals |> 
    mutate(event_date = floor_date(event_datetime, unit = "day")) |> 
    arrange(encntr_id, fin, event_datetime, event) |> 
    distinct(fin, event_date, event, .keep_all = TRUE) |> 
    select(fin, event_date, event, result_val) |> 
    pivot_wider(names_from = event, values_from = result_val)
