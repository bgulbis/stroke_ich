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
        across(apix_start_day, as.numeric),
        across(apix_start_day, ceiling)
        # across(icu_los, \(x) str_split(x, "\r\n"))
    ) |> 
    separate_wider_delim(icu_los, "\r\n", names_sep = "_", too_few = "align_start") |> 
    mutate(
        across(starts_with("icu_los"), as.numeric),
        icu_los = sum(icu_los_1, icu_los_2, icu_los_3, icu_los_4, icu_los_5, icu_los_6, icu_los_7, na.rm = TRUE),
        .by = c(mrn, encounter_csn)
    ) |> 
    select(mrn:length_stay, icu_los, everything(), -los_days, -starts_with("icu_los_"))
    

df_meds <- raw_meds |> 
    # semi_join(df_apix_dur, by = c("mrn", "encounter_csn")) |> 
    inner_join(df_demog[c("mrn", "encounter_csn", "admit_datetime", "apix_start_day")], by = c("mrn", "encounter_csn")) |> 
    mutate(
        med_hosp_day = difftime(med_datetime, admit_datetime, units = "days"),
        across(med_hosp_day, as.numeric),
        across(med_hosp_day, ceiling),
        across(medication, \(x)
               coalesce(
                   x, case_when(
                       str_detect(order_name, "bivalirudin") ~ "bivalirudin",
                       str_detect(order_name, "argatroban") ~ "argatroban",
                       str_detect(order_name, "enoxaparin") ~ "enoxaparin sodium"
                   )
               )
        ),
        across(medication, \(x) str_remove_all(x, "\\(porcine\\) in nacl|sodium \\(porcine\\)|sodium|bisulfate")),
        across(medication, str_trim)
    ) 

# x <- filter(df_meds, is.na(medication))

df_anticoag_load <- df_meds |> 
    filter(
        !(medication == "heparin" & route == "Subcutaneous"),
        !(medication == "enoxaparin" & dose <= 40),
        !(medication == "fondaparainux" & dose <= 2.5),
        !(medication == "rivaroxaban" & dose <= 2.5),
        !(medication %in% c("aspirin", "clopidogrel", "ticagrelor")),
        dose > 0,
        action %in% c("Given", "New Bag", "Rate Change", "Rate Verify", "Handoff"),
        med_hosp_day >= apix_start_day - 7,
        med_hosp_day <= apix_start_day,
        medication != "apixaban"
    ) |> 
    distinct(mrn, encounter_csn, med_hosp_day) |> 
    summarize(
        load_days = n(),
        .by = c(mrn, encounter_csn)
    ) 
    # pivot_wider(names_from = medication, values_from = load_days)

df_apixaban <- df_meds |> 
    filter(
        (medication == "apixaban" | str_detect(order_name, "apixaban")),
        action == "Given"
    ) |> 
    mutate(across(medication, \(x) coalesce(x, "apixaban"))) |> 
    med_runtime(.id = encounter_csn)

df_apix_load <- df_apixaban |> 
    filter(dose == 10) |> 
    select(encounter_csn, apix_10mg_doses = num_doses)

data_patients <- df_demog |> 
    left_join(df_apix_load, by = "encounter_csn") |> 
    left_join(df_anticoag_load, by = c("mrn", "encounter_csn")) |> 
    mutate(no_load = is.na(apix_10mg_doses) & is.na(load_days))

write.xlsx(data_patients, paste0(f, "final/prelim_apxi_data.xlsx"), overwrite = TRUE)


df_rivaroxaban <- df_meds |> 
    filter(
        (medication == "rivaroxaban" | str_detect(order_name, "rivaroxaban")),
        action == "Given"
    ) |> 
    mutate(across(medication, \(x) coalesce(x, "rivaroxaban"))) |> 
    med_runtime(.id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        across(dose_start, min),
        across(dose_stop, max),
        across(dose, list(mean = mean, max = max, first = first, last = last)),
        .by = c(encounter_csn, course_count)
    )

df_warfarin <- df_meds |> 
    filter(
        (medication == "warfarin" | str_detect(order_name, "warfarin")),
        action == "Given"
    ) |> 
    mutate(across(medication, \(x) coalesce(x, "warfarin"))) |> 
    med_runtime(.id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        across(dose_start, min),
        across(dose_stop, max),
        across(dose, list(mean = mean, max = max, first = first, last = last)),
        .by = c(encounter_csn, course_count)
    )

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

df_enox <- df_meds |> 
    filter(
        (str_detect(medication, "enoxaparin") | str_detect(order_name, "enoxaparin")),
        dose > 40,
        action == "Given"
    ) |> 
    mutate(across(medication, \(x) coalesce(x, "enoxaparin"))) |> 
    med_runtime(.id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        across(dose_start, min),
        across(dose_stop, max),
        across(dose, list(mean = mean, max = max, first = first, last = last)),
        .by = c(encounter_csn, course_count)
    )

df_fondap <- df_meds |> 
    filter(
        (str_detect(medication, "fondaparinux") | str_detect(order_name, "fondaparinux")),
        dose > 2.5,
        action == "Given"
    ) |> 
    mutate(across(medication, \(x) coalesce(x, "fondaparinux"))) |> 
    med_runtime(.id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        across(dose_start, min),
        across(dose_stop, max),
        across(dose, list(mean = mean, max = max, first = first, last = last)),
        .by = c(encounter_csn, course_count)
    )

df_argatrob <- df_meds |> 
    filter(
        (str_detect(medication, "argatroban") | str_detect(order_name, "argatroban")),
        route == "Intravenous",
        freq == "Continuous",
        action %in% c("New Bag", "Rate Change", "Rate Verify", "Stopped", "Held by provider", "Handoff")
    ) |> 
    mutate(
        across(dose, \(x) if_else(action == "Held by provider", 0, x))
        # across(dose_unit, \(x) coalesce(x, "Units/kg/hr"))
    ) |> 
    drip_runtime(.id = encounter_csn, .rate = dose, .rate_unit = dose_unit) |> 
    summarize_drips(.id = encounter_csn, .rate = dose, .rate_unit = dose_unit) |> 
    mutate(across(medication, \(x) coalesce(x, "argatroban")))

df_antiplt <- df_meds |> 
    filter(
        (medication %in% c("aspirin", "clopidogrel bisulfate", "ticagrelor") | 
             str_detect(order_name, "aspirin|clopidogrel|ticagrelor")),
        action == "Given"
    ) |> 
    # mutate(across(medication, \(x) coalesce(x, "apixaban"))) |> 
    med_runtime(.id = encounter_csn) |> 
    summarize(
        across(c(num_doses, duration), sum),
        across(dose_start, min),
        across(dose_stop, max),
        across(dose, list(mean = mean, max = max, first = first, last = last)),
        .by = c(encounter_csn, medication, course_count)
    )
