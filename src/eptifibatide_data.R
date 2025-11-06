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
    filter(nurse_unit == "HH 7J") |> 
    distinct(fin, .keep_all = TRUE)  |> 
    select(fin, icu_los = unit_los)

# icu_num <- count(icu_los, fin)

target_home_meds <- c(
    "apixaban",
    "aspirin",
    "atorvastatin",
    "clopidogrel",
    "dabigatran",
    "edoxaban",
    "lovastatin",
    "prasugrel",
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
    pivot_wider(names_from = medication, values_from = dose, names_prefix = "home_")

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
    select(fin, bolus_datetime = dose_datetime, bolus, bolus_units) |>
    arrange(fin, bolus_datetime) |>
    distinct(fin, .keep_all = TRUE)

eptif_drip_first <- eptif_drip |>
    select(fin, start_datetime, stop_datetime, duration, first_rate, last_rate, max_rate, time_wt_avg_rate) |>
    distinct(fin, .keep_all = TRUE)

eptif_start <- eptif_drip_first |>
    full_join(eptif_bolus_first, by = "fin") |> 
    group_by(fin) |> 
    mutate(begin_datetime = min(bolus_datetime, start_datetime, na.rm = TRUE)) |> 
    select(fin, begin_datetime, starts_with("bolus"), everything())

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

oral_agents <- c(
    "aspirin", 
    "clopidogrel", 
    "ticagrelor", 
    "prasugrel",
    "warfarin",
    "apixaban",
    "dabigatran",
    "edoxaban",
    "rivaroxaban"
)

oral_antithrmb <- meds |> 
    filter(medication %in% oral_agents) |> 
    arrange(fin, dose_datetime) |> 
    distinct(fin, medication, .keep_all = TRUE) |> 
    select(fin, dose_datetime, medication) |> 
    pivot_wider(names_from = medication, values_from = dose_datetime)

tx_anticoag <- meds |> 
    filter(
        (medication == "heparin" & !is.na(iv_event)) | 
            (medication == "enoxaparin" & admin_dosage > 40)
    ) |> 
    distinct(fin, medication, .keep_all = TRUE) |> 
    select(fin, dose_datetime, medication) |> 
    pivot_wider(names_from = medication, values_from = dose_datetime)

hgb_drop <- labs_vitals |> 
    inner_join(eptif_start[c("fin", "begin_datetime")], by = "fin") |> 
    filter(
        event == "Hgb",
        event_datetime >= begin_datetime,
        event_datetime <= begin_datetime + days(3)
    ) |> 
    mutate(across(result_val, as.numeric)) |> 
    lab_change(id = fin, lab_datetime = event_datetime, change.by = -2, FUN = max, back = 2) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, hgb_drop = change)

blood <- transfusions |> 
    inner_join(eptif_start[c("fin", "begin_datetime")], by = "fin") |> 
    mutate(
        prod = case_when(
            str_detect(product, "RBC") ~ "prbc",
            str_detect(product, "Plasma") ~ "ffp",
            str_detect(product, "PHP") ~ "platelet"
        )
    ) |> 
    filter(
        prod %in% c("prbc", "ffp"),
        event_datetime >= begin_datetime,
        event_datetime <= begin_datetime + days(3)
    ) |> 
    add_count(fin, prod, name = "num_units") |> 
    distinct(fin, prod, .keep_all = TRUE) |> 
    select(fin, transfuse_datetime = event_datetime, blood_product = prod, num_units)

data_eptif <- demog |> 
    left_join(icu_los, by = "fin") |> 
    left_join(data_home_meds, by = "fin") |> 
    inner_join(eptif_start, by = "fin") |> 
    left_join(sbp_eptif, by = "fin") |> 
    left_join(oral_antithrmb, by = "fin") |> 
    left_join(tx_anticoag, by = "fin") |> 
    left_join(hgb_drop, by = "fin") |> 
    left_join(blood, by = "fin")

write.xlsx(data_eptif, "U:/Data/stroke_ich/eptifibatide/final/eptifibatide_abstract_data.xlsx")    

