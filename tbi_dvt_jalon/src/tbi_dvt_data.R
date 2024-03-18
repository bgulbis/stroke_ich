library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "tbi_dvt_jalon")

raw_pts <- read_excel(paste0(f, "raw/tbi_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    select(fin) |> 
    mutate(across(fin, as.character))

mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)

raw_missing <- read_excel(paste0(f, "raw/missing_data_fins.xlsx")) |> 
    rename_all(str_to_lower)

mbo_fin_miss <- concat_encounters(raw_missing$fin)
print(mbo_fin_miss)


raw_demographics <- read_excel(paste0(f, "raw/demographics.xlsx")) |>
    rename_all(str_to_lower)

raw_diagnosis <- read_excel(paste0(f, "raw/diagnosis.xlsx")) |>
    rename_all(str_to_lower)

raw_home_meds <- read_excel(paste0(f, "raw/home_meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_ins_blood <- read_excel(paste0(f, "raw/ins_blood.xlsx")) |>
    rename_all(str_to_lower) |>
    mutate(across(c(event, child_event, event_details), str_to_lower))

raw_labs <- read_excel(paste0(f, "raw/labs.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

raw_meds <- read_excel(paste0(f, "raw/meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

zz_meds <- distinct(raw_meds, medication) |> arrange(medication)

raw_transfusions <- read_excel(paste0(f, "raw/transfusions.xlsx")) |>
    rename_all(str_to_lower)

df_admit <- select(raw_demographics, encntr_id, admit_datetime)

df_home_meds <- raw_home_meds |> 
    filter(medication %in% c("warfarin", "apixaban", "rivaroxaban", "dabigatran", "enoxaparin", "heparin")) |> 
    mutate(value = TRUE) |> 
    distinct(encntr_id, medication, value) |> 
    pivot_wider(names_from = medication, values_from = value, names_prefix = "home_")

df_reversal <- raw_meds |> 
    inner_join(df_admit, by = "encntr_id") |> 
    mutate(arrive_med_time = as.numeric(difftime(med_datetime, admit_datetime, units = "hours"))) |> 
    filter(
        medication == "prothrombin complex",
        arrive_med_time <= 24
    ) |> 
    mutate(pcc_24h = TRUE) |> 
    distinct(encntr_id, pcc_24h) 

df_anticoag <- raw_meds |> 
    inner_join(df_admit, by = "encntr_id") |> 
    filter(
        medication != "prothrombin complex",
        !(medication == "heparin" & admin_route == "SUB-Q"),
        !(medication == "enoxaparin" & dose <= 40),
        (is.na(iv_event) | iv_event == "Begin Bag")
    ) |> 
    arrange(encntr_id, med_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, first_anticoag = medication, anticoag_start_datetime = med_datetime)
    
df_diagnosis <- raw_diagnosis |> 
    filter(str_detect(icd_10_code, "I82.4|I26|I80|K92.2|K68.3|I10|E78.5|I51.9|I50.2|I63.9")) |> 
    mutate(
        diagnosis = case_when(
            str_detect(icd_10_code, "I82.4") ~ "dvt",
            str_detect(icd_10_code, "I26") ~ "pe",
            str_detect(icd_10_code, "I80") ~ "vte",
            str_detect(icd_10_code, "K92.2") ~ "gi_bleed",
            str_detect(icd_10_code, "K68.3") ~ "rp_bleed",
            str_detect(icd_10_code, "I10") ~ "htn",
            str_detect(icd_10_code, "E78.5") ~ "hld",
            str_detect(icd_10_code, "I51.9") ~ "cvd",
            str_detect(icd_10_code, "I50.2") ~ "chf",
            str_detect(icd_10_code, "I63.9") ~ "cva"
        ),
        value = TRUE
    ) |> 
    distinct(encntr_id, diagnosis, value) |> 
    pivot_wider(names_from = diagnosis, values_from = value)

df_coags <- raw_labs |> 
    inner_join(df_admit, by = "encntr_id") |> 
    filter(
        event %in% c("hgb", "hct", "platelet"),
        event_datetime <= admit_datetime + hours(24),
        !str_detect(result_val, "[A-Za-z]")
    ) |> 
    mutate(across(result_val, as.numeric)) |> 
    summarize(
        across(result_val, min),
        .by = c(encntr_id, event)
    ) |> 
    pivot_wider(names_from = event, values_from = result_val, names_prefix = "min_")

df_etoh <- raw_labs |> 
    inner_join(df_admit, by = "encntr_id") |> 
    filter(
        event == "ethanol lvl",
        event_datetime <= admit_datetime + hours(24),
        !str_detect(result_val, "[A-Za-z]")
    ) |> 
    mutate(
        censor_low = str_detect(result_val, "<"),
        censor_high = str_detect(result_val, ">"),
        across(result_val, \(x) str_remove_all(x, ">|<")),
        across(result_val, as.numeric)
    ) |> 
    summarize(
        across(result_val, \(x) max(x, na.rm = TRUE)),
        .by = c(encntr_id, censor_low)
    ) |> 
    mutate(
        across(result_val, as.character),
        across(result_val, \(x) if_else(censor_low, "<3", result_val))
    ) |> 
    select(encntr_id, ethanol_lvl = result_val)

df_gcs_admit <- raw_labs |> 
    filter(event == "glasgow coma score") |> 
    arrange(encntr_id, event_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(across(result_val, as.numeric)) |> 
    select(encntr_id, admit_gcs = result_val)

df_gcs_disch <- raw_labs |> 
    filter(event == "glasgow coma score") |> 
    inner_join(raw_demographics[c("encntr_id", "disch_datetime")], by = "encntr_id") |> 
    mutate(
        across(result_val, as.numeric),
        across(c(disch_datetime, event_datetime), \(x) floor_date(x, unit = "day"))
    ) |> 
    filter(event_datetime == disch_datetime) |> 
    summarize(
        dc_gcs_low = min(result_val, na.rm = TRUE),
        dc_gcs_high = max(result_val, na.rm = TRUE),
        .by = encntr_id
    )

df_sbp <- raw_labs |> 
    inner_join(df_admit, by = "encntr_id") |> 
    filter(
        str_detect(event, "systolic"),
        event_datetime <= admit_datetime + hours(4)
    ) |> 
    mutate(across(result_val, as.numeric)) |> 
    summarize(
        min_sbp = min(result_val, na.rm = TRUE),
        .by = encntr_id
    )

df_transfuse <- raw_transfusions |> 
    inner_join(df_admit, by = "encntr_id") |> 
    filter(event_datetime <= admit_datetime + hours(4)) |> 
    mutate(
        prod_type = case_when(
            str_detect(product, "WB") ~ "whole",
            str_detect(product, "RBC") ~ "prbc",
            str_detect(product, "Plasma|FFP") ~ "ffp",
            str_detect(product, "PHP|Plts") ~ "platelets",
            str_detect(product, "Cryo") ~ "cryo"
        ),
        across(volume, as.numeric)
    ) |> 
    summarize(
        across(volume, sum),
        .by = c(encntr_id, prod_type)
    ) |> 
    pivot_wider(names_from = prod_type, values_from = volume, names_prefix = "prod_")

df_ins_blood <- raw_ins_blood |> 
    inner_join(df_admit, by = "encntr_id") |> 
    filter(event_datetime <= admit_datetime + hours(4)) |> 
    mutate(across(event, \(x) str_replace_all(x, " ", "_"))) |> 
    summarize(
        across(io_volume, sum),
        .by = c(encntr_id, event)
    ) |> 
    pivot_wider(names_from = event, values_from = io_volume)

df_blood <- df_transfuse |> 
    full_join(df_ins_blood, by = "encntr_id") |> 
    mutate(
        blood_whole = !is.na(prod_whole),
        blood_prbc = !is.na(prod_prbc) | prbc_volume > 0,
        blood_ffp = !is.na(prod_ffp) | ffp_volume > 0,
        blood_platelet = !is.na(prod_platelets) | platelets_volume > 0,
        blood_cryo = !is.na(prod_cryo) | cryo_volume > 0
    ) |> 
    select(encntr_id, starts_with("blood_"))

data_patients <- raw_demographics |> 
    left_join(df_home_meds, by = "encntr_id") |> 
    left_join(df_reversal, by = "encntr_id") |> 
    left_join(df_anticoag, by = "encntr_id") |> 
    left_join(df_diagnosis, by = "encntr_id") |> 
    left_join(df_coags, by = "encntr_id") |> 
    left_join(df_etoh, by = "encntr_id") |> 
    left_join(df_gcs_admit, by = "encntr_id") |> 
    left_join(df_gcs_disch, by = "encntr_id") |> 
    left_join(df_sbp, by = "encntr_id") |> 
    left_join(df_blood, by = "encntr_id") |> 
    left_join(df_transfuse, by = "encntr_id") |> 
    left_join(df_ins_blood, by = "encntr_id") |> 
    mutate(across(where(is.logical), \(x) coalesce(x, FALSE))) |> 
    select(-encntr_id, -arrive_datetime, -admit_datetime, -disch_datetime)

write.xlsx(data_patients, paste0(f, "final/tbi_dvt_data.xlsx"), overwrite = TRUE)


# missing data patients ---------------------------------------------------------------------------------------------------------------

raw_miss_demog <- read_excel(paste0(f, "raw/missing_demog.xlsx")) |>
    rename_all(str_to_lower)

raw_locations <- read_excel(paste0(f, "raw/locations.xlsx")) |>
    rename_all(str_to_lower)

raw_vent_extubation <- read_excel(paste0(f, "raw/vent_extubation.xlsx")) |>
    rename_all(str_to_lower)

raw_vent_times <- read_excel(paste0(f, "raw/vent_times.xlsx")) |>
    rename_all(str_to_lower)

raw_vte_proph <- read_excel(paste0(f, "raw/vte_prophylaxis.xlsx")) |>
    rename_all(str_to_lower)


tmp_vent_last <- raw_vent_times |>
    filter(event == "Vent Stop Time") |>
    arrange(encntr_id, result_datetime) |>
    group_by(encntr_id) |>
    summarize(across(result_datetime, \(x) max(x, na.rm = TRUE))) |>
    rename(last_vent_datetime = result_datetime)

df_vent <- raw_vent_extubation |>
    bind_rows(raw_vent_times) |>
    filter(event %in% c("Vent Start Time", "Extubation Event")) |>
    mutate(
        across(result_datetime, \(x) coalesce(x, event_datetime)),
        across(event, \(x) str_replace_all(x, pattern = c("Vent Start Time" = "intubation", "Extubation Event" = "extubation")))
    ) |>
    arrange(encntr_id, result_datetime) |>
    group_by(encntr_id) |>
    mutate(new_event = event != lag(event) | is.na(lag(event))) |>
    mutate(across(new_event, cumsum)) |>
    distinct(encntr_id, new_event, .keep_all = TRUE) |>
    filter(!(new_event == 1 & event == "extubation")) |>
    group_by(encntr_id, event) |>
    mutate(
        vent = TRUE,
        vent_n = cumsum(vent)
    ) |>
    select(encntr_id, vent_n, event, result_datetime) |>
    spread(event, result_datetime) |>
    select(
        encntr_id,
        vent_n,
        intubate_datetime = intubation,
        extubate_datetime = extubation
    ) |>
    left_join(tmp_vent_last, by = "encntr_id") |>
    left_join(raw_miss_demog[c("encntr_id", "disch_datetime")], by = "encntr_id") |>
    mutate(
        across(extubate_datetime, \(x) coalesce(x, last_vent_datetime)),
        across(extubate_datetime, \(x) coalesce(x, disch_datetime)),
        vent_duration_days = difftime(extubate_datetime, intubate_datetime, units = "days"),
        across(vent_duration_days, as.numeric)
    ) |>
    select(-last_vent_datetime, -disch_datetime) |> 
    filter(vent_duration_days > 0)

icu <- c(
    "HH 7J",
    "HH CCU",
    "HH CVICU",
    "HH HFIC",
    "HH MICU",
    "HH NVIC",
    "HH S MICU",
    "HH S SHIC",
    "HH S STIC",
    "HH S TSCU",
    "HH S TSIC",
    "HH STIC",
    "HH TICU",
    "HH TSCU",
    "HH TSIC"
)

df_icu_los <- raw_locations |>
    filter(nurse_unit %in% icu) |>
    group_by(encntr_id) |>
    summarize(
        across(unit_los, sum),
        icu_start = min(unit_in_datetime),
        icu_stop = max(unit_out_datetime)
    )

df_vte <- raw_vte_proph |> 
    filter(admin_route == "SUB-Q") |> 
    distinct(encntr_id) |> 
    mutate(vte_proph = TRUE)

df_miss_data <- raw_miss_demog |> 
    select(encntr_id, fin, age, sex, race, weight, los) |> 
    left_join(df_icu_los, by = "encntr_id") |> 
    left_join(df_vent, by = "encntr_id") |> 
    left_join(df_vte, by = "encntr_id") |> 
    select(-encntr_id, -vent_n, -contains("datetime"), -icu_start, -icu_stop, icu_los = unit_los)

write.xlsx(df_miss_data, paste0(f, "final/missing_data_pts.xlsx"), overwrite = TRUE)
