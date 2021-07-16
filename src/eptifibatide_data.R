library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

demog <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "baseline") |>
    rename_all(str_to_lower) |>
    distinct() |>
    mutate(across(c(weight, hgb, platelets, scr, egfr, nihss), as.numeric))

home_meds <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "home_meds") |>
    rename_all(str_to_lower) |>
    distinct()

locations <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "locations") |>
    rename_all(str_to_lower) |>
    distinct()

meds <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "meds") |>
    rename_all(str_to_lower) |>
    distinct()

labs_vitals <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "labs_vitals") |>
    rename_all(str_to_lower) |>
    distinct()

transfusions <- read_excel("U:/Data/stroke_ich/eptifibatide/raw/eptifibatide_data.xlsx", sheet = "transfusions") |>
    rename_all(str_to_lower) |>
    distinct()

icu_los <- locations |>
    filter(nurse_unit == "HH 7J")

# how to handle multiple ICU admissions

icu_num <- count(icu_los, fin)

target_home_meds <- c(
    "apixaban",
    "aspirin",
    "atorvastatin",
    "clopidogrel",
    "lovastatin",
    "pravastatin",
    "rivaroxaban",
    "rosuvastatin",
    "simvastatin",
    "ticagrelor"
)

data_home_meds <- home_meds |>
    filter(medication %in% target_home_meds) |>
    distinct(fin, medication) |>
    mutate(dose = TRUE) |>
    pivot_wider(names_from = medication, values_from = dose)

weights <- select(demog, fin, weight)

eptif_drip <- meds |>
    filter(
        medication == "eptifibatide",
        !is.na(iv_event)
    ) |>
    rename(med_datetime = dose_datetime, rate_unit = infusion_unit) |>
    inner_join(weights, by = "fin") |>
    mutate(
        rate = case_when(
            rate_unit == "mg/kg/hr" ~ infusion_rate * 1000 / 60,
            rate_unit == "mg/hr" ~ infusion_rate * 1000 / 60 / weight,
            rate_unit == "mg/min" ~ infusion_rate * 1000 / weight,
            rate_unit == "microgram/kg/hr" ~ infusion_rate / 60,
            TRUE ~ infusion_rate
        )
        # rate_unit = "microgram/kg/min"
    ) |>
    drip_runtime(.grp_var = vars(fin), id = fin, drip_off = 24) |>
    summarize_drips(.grp_var = vars(fin), id = fin)

eptif_bolus <- meds |>
    filter(
        medication == "eptifibatide",
        admin_dosage > 0,
        is.na(iv_event),
        dosage_unit != "mL"
    ) |>
    inner_join(weights, by = "fin") |>
    mutate(
        bolus = case_when(
            dosage_unit == "mg" ~ admin_dosage * 1000 / weight,
            dosage_unit == "microgram" ~ admin_dosage / weight
        ),
        bolus_units = "microgram/kg"
    )

eptif_bolus_first <- eptif_bolus |>
    select(fin, start_datetime = dose_datetime) |>
    arrange(fin, start_datetime) |>
    distinct(fin, .keep_all = TRUE)

eptif_drip_first <- eptif_drip |>
    select(fin, start_datetime) |>
    distinct(fin, .keep_all = TRUE)

eptif_start <- eptif_bolus_first |>
    bind_rows(eptif_drip_first) |>
    arrange(fin, start_datetime) |>
    distinct(fin, .keep_all = TRUE)

# x <- anti_join(meds, eptif_start, by = "fin")

sbp_eptif <- labs_vitals |> 
    inner_join(eptif_start, by = "fin") |> 
    filter(
        event %in% c("Systolic Blood Pressure", "Arterial Systolic BP 1"),
        event_datetime <= start_datetime
    ) |> 
    arrange(fin, desc(event_datetime)) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_before_eptif = result_val)

oral_antiplt <- meds |> 
    filter(medication %in% c("aspirin", "clopidogrel", "ticagrelor")) |> 
    arrange(fin, dose_datetime) |> 
    distinct(fin, medication, .keep_all = TRUE)

# multiple courses of eptif
# hgb drop only after eptif?
# transfusion only after eptif?
