library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "mrsa_pcr_andrew")

raw_pcr <- read_excel(
    paste0(f, "raw/mrsa_pcr_results.xlsx"), 
    skip = 11,
    col_names = c("range_start", "range_end", "mrn", "lab_datetime", "lab", "result")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

raw_demog <- read_excel(
    paste0(f, "raw/demographics.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "age", "sex", "race", "ethnicity", "los", 
                  "admit_date", "admit_time", "disch_date", "disch_time", "disch_dispo", "diag_primary", "diag_poa")
) |> 
    rename_all(str_to_lower) |> 
    mutate(
        admit_datetime = ymd_hms(paste(admit_date, admit_time)),
        disch_datetime = ymd_hms(paste(disch_date, disch_time)),
        length_stay = difftime(disch_datetime, admit_datetime, units = "days"),
        across(length_stay, as.numeric)
    )|> 
    select(-range_start, -range_end, -admit_date, -admit_time, -disch_date, -disch_time)

raw_abx <- read_excel(
    paste0(f, "raw/antibiotics.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "order_id", "med_datetime", "medication", "route")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

df_pcr <- raw_pcr |> 
    inner_join(raw_demog[c("mrn", "encounter_csn", "admit_datetime", "disch_datetime")], by = "mrn", relationship = "many-to-many") |> 
    filter(lab_datetime >= admit_datetime, lab_datetime <= disch_datetime) |> 
    select(mrn, encounter_csn, everything()) |> 
    arrange(mrn, admit_datetime, lab_datetime) |> 
    distinct(mrn, .keep_all = TRUE)

df_abx_before <- raw_abx |> 
    inner_join(df_pcr, by = c("mrn", "encounter_csn"), relationship = "many-to-many") |> 
    filter(med_datetime < lab_datetime)

df_pts <- anti_join(df_pcr, df_abx_before, by = c("mrn", "encounter_csn")) 

data_patients <- df_pts |> 
    select(-admit_datetime, -disch_datetime) |> 
    inner_join(raw_demog, by = c("mrn", "encounter_csn")) |> 
    select(-los)

write.xlsx(data_patients, paste0(f, "final/mrsa_pcr_7jones_patients.xlsx"), overwrite = TRUE)
