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
    mutate(across(c(medication, order_name), str_to_lower)) |>
    select(-range_start, -range_end) |> 
    arrange(mrn, encounter_csn, med_datetime)

zzz_meds <- distinct(raw_meds, medication) |> arrange(medication)
zzz_action <- distinct(raw_meds, action) |> arrange(action)

raw_labs <- read_excel(
    paste0(f, "raw/labs.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "lab_datetime", "case_id", "lab", "result", "result_unit", "nurse_unit")
) |>
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end) |> 
    mutate(across(lab, str_to_lower))

df_apix_dur <- raw_meds |> 
    filter(
        medication == "apixaban" | str_detect(order_name, "apixaban"),
        action == "Given"
    ) |> 
    summarize(
        first_dose = first(med_datetime),
        last_dose = last(med_datetime),
        .by = c(mrn, encounter_csn)
    ) |> 
    mutate(
        apix_dur = difftime(last_dose, first_dose, units = "days"),
        across(apix_dur, as.numeric)
    ) |> 
    filter(apix_dur >= 2) |> 
    distinct(mrn, .keep_all = TRUE)

write.xlsx(df_apix_dur[c("mrn", "encounter_csn")], paste0(f, "final/prelim_apix_pts.xlsx"), overwrite = TRUE)

df_demog <- raw_demog |> 
    inner_join(df_apix_dur, by = c("mrn", "encounter_csn")) |> 
    mutate(
        apix_start_day = difftime(first_dose, admit_datetime, units = "days"),
        across(apix_start_day, as.numeric)
    )

df_meds <- raw_meds |> 
    # semi_join(df_apix_dur, by = c("mrn", "encounter_csn")) |> 
    inner_join(df_demog[c("mrn", "encounter_csn", "admit_datetime", "apix_start_day")], by = c("mrn", "encounter_csn")) |> 
    mutate(
        med_hosp_day = difftime(med_datetime, admit_datetime, units = "days"),
        across(med_hosp_day, as.numeric)
    ) 

df_apixaban <- df_meds |> 
    filter(
        (medication == "apixaban" | str_detect(order_name, "apixaban")),
        action == "Given"
    ) |> 
    mutate(across(medication, \(x) coalesce(x, "apixaban"))) |> 
    med_runtime(.id = encounter_csn)

df_hep_drip <- df_meds |> 
    filter(
        (str_detect(medication, "heparin") | str_detect(order_name, "heparin")),
        route == "Intravenous",
        freq == "Continuous",
        action %in% c("New Bag", "Rate Change", "Rate Verify", "Stopped", "Held by provider", "Handoff")
    ) |> 
    mutate(
        across(dose, \(x) if_else(action == "Held by provider", 0, x)),
        across(dose_unit, \(x) coalesce(x, "Units/kg/hr"))
    ) |> 
    drip_runtime(.id = encounter_csn, .rate = dose, .rate_unit = dose_unit) |> 
    summarize_drips(.id = encounter_csn, .rate = dose, .rate_unit = dose_unit)

df_antiplt <- df_meds |> 
    filter(
        (medication %in% c("aspirin", "clopidogrel bisulfate", "ticagrelor") | 
             str_detect(order_name, "aspirin|clopidogrel|ticagrelor")),
        action == "Given"
    ) |> 
    # mutate(across(medication, \(x) coalesce(x, "apixaban"))) |> 
    med_runtime(.id = encounter_csn)
