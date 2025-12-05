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

raw_op_meds <- read_excel(
    paste0(f, "raw/op_meds.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "op_meds")
) |> 
    rename_all(str_to_lower)

raw_meds <- read_excel(
    paste0(f, "raw/meds.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "order_id", "med_datetime", "medication", 
                  "order_name", "dose", "dose_unit", "route", "freq", "action", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower)) |> 
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

df_patients <- raw_demog |> 
    anti_join(raw_op_meds, by = "mrn") |> 
    arrange(mrn, admit_datetime) |> 
    distinct(mrn, .keep_all = TRUE)

df_dates <- select(df_patients, mrn, encounter_csn, admit_datetime, disch_datetime)

zzz_meds <- distinct(raw_meds, medication) |> arrange(medication)
zzz_routes <- distinct(raw_meds, route) |> arrange(route)

df_meds <- raw_meds |> 
    semi_join(df_dates, by = c("mrn", "encounter_csn")) |> 
    arrange(mrn, encounter_csn, med_datetime)

df_sup <- df_meds |> 
    filter(str_detect(medication, "pantoprazole|lansoprazole|esomeprazole|omeprazole|rabeprazole|famotidine|cimetidine")) |> 
    med_runtime(route, .id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        first_dose = first(dose),
        max_dose = max(dose),
        dose_start = first(dose_start),
        dose_stop = last(dose_stop),
        .by = c(encounter_csn, medication, route, course_count)
    )

incl_routes <- c("Intravenous", "Nasogastric", "Oral", "Per G Tube", "Per J Tube")

df_meds_steroids <- df_meds |> 
    filter(
        str_detect(medication, "dexamethasone|hydrocortisone|methylprednisolone|prednisone"),
        route %in% incl_routes
    ) |> 
    med_runtime(route, .id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        first_dose = first(dose),
        max_dose = max(dose),
        dose_start = first(dose_start),
        dose_stop = last(dose_stop),
        .by = c(encounter_csn, medication, route, course_count)
    )

df_meds_nsaids <- df_meds |> 
    filter(
        str_detect(medication, "ibuprofen|diclofenac|ketorolac|meloxicam|naproxen"),
        route %in% incl_routes
    ) |> 
    med_runtime(route, .id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        first_dose = first(dose),
        max_dose = max(dose),
        dose_start = first(dose_start),
        dose_stop = last(dose_stop),
        .by = c(encounter_csn, medication, route, course_count)
    )

df_meds_antiplt <- df_meds |> 
    filter(
        str_detect(medication, "aspirin|clopidogrel|ticagrelor"),
        route %in% incl_routes
    ) |> 
    med_runtime(route, .id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        first_dose = first(dose),
        max_dose = max(dose),
        dose_start = first(dose_start),
        dose_stop = last(dose_stop),
        .by = c(encounter_csn, medication, route, course_count)
    )

df_meds_anticoag <- df_meds |> 
    filter(
        (str_detect(medication, "apixaban|dabigatran|rivaroxaban|warfarin") & route %in% incl_routes) |
            (str_detect(medication, "enoxaparin") & dose > 40) 
    ) |> 
    med_runtime(route, .id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        first_dose = first(dose),
        max_dose = max(dose),
        dose_start = first(dose_start),
        dose_stop = last(dose_stop),
        .by = c(encounter_csn, medication, route, course_count)
    )

df_drips <- df_meds |> 
    filter(
        str_detect(medication, "argatroban|bivalirudin|cangrelor|heparin"),
        route == "Intravenous",
        freq == "Continuous"
    ) 

df_pressors <- df_meds |> 
    filter(
        str_detect(medication, "dobutamine|dopamine|epinephrine|norepinephrine"),
        route == "Intravenous",
        freq == "Continuous"
    ) 

df_labs <- raw_labs |> 
    inner_join(df_dates, by = "mrn") |> 
    filter(
        lab_datetime >= admit_datetime,
        lab_datetime <= disch_datetime
    ) |> 
    arrange(mrn, encounter_csn, lab_datetime, lab) |> 
    mutate(across(lab, str_to_lower))

zzz_labs <- distinct(df_labs, lab) |> arrange(lab)

df_labs_admit <- df_labs |> 
    filter(
        lab_datetime <= admit_datetime + hours(24),
        lab != "c difficile dna"
    ) |> 
    distinct(mrn, encounter_csn, lab, .keep_all = TRUE) |> 
    select(mrn, encounter_csn, lab, value) |> 
    pivot_wider(names_from = lab, values_from = value, names_sort = TRUE, names_prefix = "admit_")

df_labs_daily <- df_labs |> 
    filter(lab != "c difficile dna") |> 
    mutate(
        # lab_date = floor_date(lab_datetime, unit = "days"),
        hosp_day = difftime(lab_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(value, list(min = min, max = max, first = first), .names = "{.fn}"),
        .by = c(mrn, encounter_csn, lab, hosp_day)
    ) |> 
    pivot_wider(names_from = lab, values_from = c(first, min, max), names_glue = "{lab}_{.value}", names_sort = TRUE)

df_bp_daily <- raw_flow_bp |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |> 
    mutate(
        hosp_day = difftime(taken_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(c(sbp, dbp), list(min = min, max = max)),
        .by = c(mrn, encounter_csn, hosp_day)
    ) |> 
    arrange(mrn, encounter_csn, hosp_day)

df_map_admit <- raw_flow_map |> 
    arrange(mrn, encounter_csn, taken_datetime) |> 
    distinct(mrn, encounter_csn, .keep_all = TRUE) |> 
    select(mrn, encounter_csn, map_admit = map)

df_map_daily <- raw_flow_map |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |>
    mutate(
        hosp_day = difftime(taken_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(map, list(min = min, max = max)),
        .by = c(mrn, encounter_csn, hosp_day)
    ) |> 
    arrange(mrn, encounter_csn, hosp_day)

df_hr_daily <- raw_flow_hr |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |>
    mutate(
        hosp_day = difftime(taken_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(hr, list(min = min, max = max)),
        .by = c(mrn, encounter_csn, hosp_day)
    ) |> 
    arrange(mrn, encounter_csn, hosp_day)

df_rr_daily <- raw_flow_rr |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |>
    mutate(
        hosp_day = difftime(taken_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(rr, list(min = min, max = max)),
        .by = c(mrn, encounter_csn, hosp_day)
    ) |> 
    arrange(mrn, encounter_csn, hosp_day)

df_temp_daily <- raw_flow_temp |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |>
    mutate(
        hosp_day = difftime(taken_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(temp_c, list(min = min, max = max)),
        .by = c(mrn, encounter_csn, hosp_day)
    ) |> 
    arrange(mrn, encounter_csn, hosp_day)

df_gcs_daily <- raw_flow_gcs |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |>
    arrange(mrn, encounter_csn, taken_datetime) |> 
    mutate(
        hosp_day = difftime(taken_datetime, admit_datetime, units = "day"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    summarize(
        across(gcs, first),
        .by = c(mrn, encounter_csn, hosp_day)
    ) |> 
    arrange(mrn, encounter_csn, hosp_day)

df_labs_cdiff <- df_labs |> 
    filter(lab == "c difficile dna") |> 
    arrange(mrn, encounter_csn, desc(value)) |> 
    distinct(mrn, encounter_csn, .keep_all = TRUE) |> 
    select(mrn, encounter_csn, cdiff = value)

df_weight_admit <- raw_flow_weight |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn")) |>
    arrange(mrn, encounter_csn, taken_datetime) |> 
    distinct(mrn, encounter_csn, .keep_all = TRUE) |> 
    select(mrn, encounter_csn, weight_kg)
