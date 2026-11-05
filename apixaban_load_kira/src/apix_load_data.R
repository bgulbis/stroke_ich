library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "apixaban_load_kira")

raw_demog <- read_excel(
    paste0(f, "raw/demographics.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "age", "sex", "race", "ethnicity", "admit_date", "admit_time",
                  "disch_date", "disch_time", "los_days", "icu_los", "disch_dispo", "disch_facility", "disch_nurse_unit",
                  "readmitted", "diag_primary", "diag_poa")
) |> 
    rename_all(str_to_lower) |> 
    mutate(
        admit_datetime = ymd_hms(paste(admit_date, admit_time)),
        disch_datetime = ymd_hms(paste(disch_date, disch_time)),
        length_stay = difftime(disch_datetime, admit_datetime, units = "days"),
        across(length_stay, as.numeric)
    ) |> 
    select(-range_start, -range_end, -admit_date, -admit_time, -disch_date, -disch_time)

raw_meds <- read_excel(
    paste0(f, "raw/meds.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "med_datetime", "order_id", "medication", "order_name", 
                  "dose", "dose_unit", "route", "freq", "action", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    # mutate(across(medication, str_to_lower)) |> 
    select(-range_start, -range_end) |> 
    arrange(mrn, encounter_csn, med_datetime)

raw_labs <- read_excel(
    paste0(f, "raw/labs.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "lab_datetime", "case_id", "lab", "result", "result_unit", "nurse_unit")
) |>
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end) |> 
    mutate(across(lab, str_to_lower))
