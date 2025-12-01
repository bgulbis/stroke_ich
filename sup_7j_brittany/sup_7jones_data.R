library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "sup_7j_brittany")

raw_demog <- read_excel(
    paste0(f, "raw/demographics.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "age", "sex", "race", "ethnicity", 
                  "admit_date", "admit_time", "disch_date", "disch_time", "los", "diag_primary", "diag_poa")
) |> 
    rename_all(str_to_lower) |> 
    mutate(
        admit_datetime = ymd_hms(paste(admit_date, admit_time)),
        disch_datetime = ymd_hms(paste(disch_date, disch_time)),
        length_stay = difftime(disch_datetime, admit_datetime, units = "days"),
        across(length_stay, as.numeric)
    )|> 
    select(-range_start, -range_end, -admit_date, -admit_time, -disch_date, -disch_time)

raw_meds <- read_excel(
    paste0(f, "raw/meds.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "order_id", "med_datetime", "medication", 
                  "order_name", "dose", "dose_unit", "route", "freq", "action", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

raw_labs <- read_excel(
    paste0(f, "raw/labs.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "lab_datetime", "lab", "value", "value_unit", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

raw_icu_los <- read_excel(
    paste0(f, "raw/icu_los.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "nurse_unit", "icu_start_datetime", "icu_stop_datetime", "icu_los")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

raw_flow_bp <- read_excel(
    paste0(f, "raw/flowsheet_bp.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "bp", "sbp", "dbp")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_fio2 <- read_excel(
    paste0(f, "raw/flowsheet_fio2.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "fio2")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_gcs <- read_excel(
    paste0(f, "raw/flowsheet_gcs.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "gcs")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_hr <- read_excel(
    paste0(f, "raw/flowsheet_hr.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "hr")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_map <- read_excel(
    paste0(f, "raw/flowsheet_map.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "map")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_rr <- read_excel(
    paste0(f, "raw/flowsheet_rr.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "rr")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_temp <- read_excel(
    paste0(f, "raw/flowsheet_temp.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "temp_c", "temp_f")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_vent <- read_excel(
    paste0(f, "raw/flowsheet_vent.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "vent")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_flow_weight <- read_excel(
    paste0(f, "raw/flowsheet_weight.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "weight_kg")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

