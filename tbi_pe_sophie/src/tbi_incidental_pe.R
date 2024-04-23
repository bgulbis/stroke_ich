library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "tbi_pe_sophie")

# raw_pts <- get_xlsx_data(paste0(f, "raw"), "tbi_pe_imaging")
# 
# raw_pts_trauma <- read_excel(paste0(f, "raw/tbi_pe_trauma.xlsx")) |> 
#     rename_all(str_to_lower)
# 
# df_trauma_pts <- anti_join(raw_pts_trauma, raw_pts, by = "fin")


raw_pts <- read_excel(paste0(f, "raw/tbi_pe_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    distinct(fin, .keep_all = TRUE)
                          
mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)

raw_demographics <- read_excel(paste0(f, "raw/demographics.xlsx")) |>
    rename_all(str_to_lower)

raw_diagnosis <- read_excel(paste0(f, "raw/diagnosis.xlsx")) |>
    rename_all(str_to_lower)

raw_io_blood <- read_excel(paste0(f, "raw/io_blood.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

raw_labs_vitals <- read_excel(paste0(f, "raw/labs_vitals.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

raw_locations <- read_excel(paste0(f, "raw/locations.xlsx")) |>
    rename_all(str_to_lower)

raw_meds <- read_excel(paste0(f, "raw/meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_scans <- read_excel(paste0(f, "raw/scans.xlsx")) |>
    rename_all(str_to_lower)

raw_transfusions <- read_excel(paste0(f, "raw/transfusions.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

raw_vent_extubations <- read_excel(paste0(f, "raw/vent_extubations.xlsx")) |>
    rename_all(str_to_lower)

raw_vent_times <- read_excel(paste0(f, "raw/vent_times.xlsx")) |>
    rename_all(str_to_lower)

df_demog <- raw_demographics |> 
    select(fin, weight, height, admit_datetime, disch_datetime, los, disch_disposition)

df_vte_proph <- raw_meds |> 
    filter(
        (medication == "heparin" & admin_route == "SUB-Q") |
            (medication == "enoxaparin" & dose <= 40)
    ) |> 
    arrange(fin, med_datetime) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, start_vte_proph = med_datetime, vte_proph = medication)

df_anticoag <- raw_meds |> 
    filter(
        !medication %in% c("enoxaparin", "heparin") |
            (medication == "enoxaparin" & dose > 40) |
            (medication == "heparin" & iv_event == "Begin Bag")
    ) |> 
    arrange(fin, medication, med_datetime) |> 
    distinct(fin, medication, .keep_all = TRUE) |> 
    select(fin, medication, med_datetime) |> 
    pivot_wider(names_from = medication, values_from = med_datetime, names_prefix = "start_")

tmp_vent_last <- raw_vent_times |>
    filter(event == "Vent Stop Time") |>
    arrange(fin, result_datetime) |>
    summarize(across(result_datetime, \(x) max(x, na.rm = TRUE)), .by = fin) |>
    rename(last_vent_datetime = result_datetime)

df_vent <- raw_vent_extubations |>
    bind_rows(raw_vent_times) |>
    filter(event %in% c("Vent Start Time", "Extubation Event")) |>
    mutate(
        across(result_datetime, \(x) coalesce(x, event_datetime)),
        across(event, \(x) str_replace_all(x, pattern = c("Vent Start Time" = "intubation", "Extubation Event" = "extubation"))),
        across(result_datetime, \(x) if_else(fin == 495737639367 & event == "intubation", x - days(1), x))
    ) |>
    arrange(fin, result_datetime) |>
    mutate(new_event = event != lag(event) | is.na(lag(event)), .by = fin) |>
    mutate(across(new_event, cumsum)) |>
    distinct(fin, new_event, .keep_all = TRUE) |>
    filter(!(new_event == 1 & event == "extubation")) |>
    mutate(
        vent = TRUE,
        vent_n = cumsum(vent),
        .by = c(fin, event)
    ) |>
    select(fin, vent_n, event, result_datetime) |>
    spread(event, result_datetime) |>
    select(
        fin,
        vent_n,
        intubate_datetime = intubation,
        extubate_datetime = extubation
    ) |>
    left_join(tmp_vent_last, by = "fin") |>
    left_join(raw_demographics[c("fin", "disch_datetime")], by = "fin") |>
    mutate(
        across(extubate_datetime, \(x) coalesce(x, last_vent_datetime)),
        across(extubate_datetime, \(x) coalesce(x, disch_datetime)),
        across(extubate_datetime, \(x) if_else(x < intubate_datetime, disch_datetime, x)),
        vent_days = difftime(extubate_datetime, intubate_datetime, units = "days"),
        across(vent_days, as.numeric)
    ) |>
    summarize(
        across(vent_days, sum),
        .by = fin
    )

df_gcs_admit <- raw_labs_vitals |> 
    filter(event == "glasgow coma score") |> 
    arrange(fin, event_datetime) |> 
    distinct(fin, .keep_all = TRUE) |> 
    mutate(across(result_val, as.numeric)) |> 
    select(fin, event_id, admit_gcs = result_val)

df_gcs_disch <- raw_labs_vitals |> 
    filter(event == "glasgow coma score") |> 
    anti_join(df_gcs_admit, by = "event_id") |> 
    inner_join(raw_demographics[c("fin", "disch_datetime")], by = "fin") |> 
    mutate(
        across(result_val, as.numeric),
        across(c(disch_datetime, event_datetime), \(x) floor_date(x, unit = "day"))
    ) |> 
    filter(event_datetime == disch_datetime) |> 
    arrange(fin, desc(event_datetime)) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, disch_gcs = result_val)

df_sbp_lt90 <- raw_labs_vitals |> 
    inner_join(raw_demographics[c("fin", "admit_datetime")], by = "fin") |> 
    filter(
        str_detect(event, "systolic"),
        event_datetime <= admit_datetime + hours(4)
    ) |> 
    mutate(across(result_val, as.numeric)) |> 
    filter(result_val < 90) |> 
    distinct(fin) |> 
    mutate(admit_sbp_lt90 = TRUE)

df_transfuse <- raw_transfusions |> 
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
        .by = c(fin, prod_type)
    ) |> 
    pivot_wider(names_from = prod_type, values_from = volume, names_prefix = "prod_")

df_ins_blood <- raw_io_blood |> 
    mutate(across(event, \(x) str_replace_all(x, " ", "_"))) |> 
    summarize(
        across(io_volume, sum),
        .by = c(fin, event)
    ) |> 
    pivot_wider(names_from = event, values_from = io_volume)

df_blood <- df_transfuse |> 
    full_join(df_ins_blood, by = "fin") |> 
    mutate(
        blood_whole = !is.na(prod_whole),
        blood_prbc = !is.na(prod_prbc) | prbc_volume > 0,
        blood_ffp = !is.na(prod_ffp) | ffp_volume > 0,
        blood_platelet = !is.na(prod_platelets) | platelets_volume > 0,
        blood_cryo = !is.na(prod_cryo) | cryo_volume > 0
    ) |> 
    select(fin, starts_with("blood_"))

df_scans <- raw_scans |> 
    distinct(fin, event) |> 
    mutate(values = TRUE) |> 
    pivot_wider(names_from = event, values_from = values)

df_icu_los <- raw_locations |> 
    filter(nurse_unit %in% c("HH 7J", "HH CVICU", "HH NVIC", "HH S MICU", "HH S STIC", "HH STIC")) |> 
    mutate(
        across(nurse_unit, \(x) str_remove_all(x, "HH S |HH ")),
        across(nurse_unit, \(x) str_replace_all(x, "7J", "7Jones")),
        across(nurse_unit, str_to_lower)
    ) |> 
    summarize(
        across(unit_los, sum),
        .by = c(fin, nurse_unit)
    ) |> 
    pivot_wider(names_from = "nurse_unit", values_from = unit_los, names_prefix = "los_")

data_patients <- raw_pts |> 
    select(-admit_datetime, -disch_datetime) |> 
    mutate(across(pe_icd10:dabigatran, as.logical)) |> 
    left_join(df_demog, by = "fin") |> 
    left_join(df_icu_los, by = "fin") |> 
    left_join(df_anticoag, by = "fin") |> 
    left_join(df_gcs_admit, by = "fin") |> 
    select(-event_id) |> 
    left_join(df_gcs_disch, by = "fin") |> 
    left_join(df_sbp_lt90, by = "fin") |> 
    left_join(df_blood, by = "fin") |> 
    left_join(df_vent, by = "fin") |> 
    left_join(df_vte_proph, by = "fin") |> 
    left_join(df_scans, by = "fin")

data_diagnosis <- raw_diagnosis |> 
    select(-encntr_id) |> 
    arrange(fin, diag_priority)

l <- list(
    "data" = data_patients,
    "diagnosis" = data_diagnosis
)

write.xlsx(l, paste0(f, "final/incidental_pe_data.xlsx"), overwrite = TRUE)
