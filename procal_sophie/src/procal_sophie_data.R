library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "procal_sophie")

raw_demog <- read_excel(
    paste0(f, "raw/patients.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "age", "sex", "race", "ethnicity", 
                  "admit_date", "admit_time", "disch_date", "disch_time", "los")
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
    mutate(across(medication, str_to_lower)) |> 
    select(-range_start, -range_end)

raw_labs <- read_excel(
    paste0(f, "raw/labs.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "order_datetime", "lab_datetime", "lab", "value", "value_unit", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end) |> 
    arrange(mrn, lab_datetime, lab)

raw_cultures <- read_excel(
    paste0(f, "raw/cultures.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "specimen_id", "lab_datetime", "lab", "value", "value_unit", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

raw_icu_los <- read_excel(
    paste0(f, "raw/icu_stay.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "nurse_unit", "icu_stop_datetime", "icu_start_datetime", "icu_los", "vent_days")
) |> 
    rename_all(str_to_lower) |> 
    select(mrn, nurse_unit, icu_start_datetime, everything(), -range_start, -range_end) |> 
    arrange(mrn, icu_start_datetime)

raw_gcs <- read_excel(
    paste0(f, "raw/gcs.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "value", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_temps <- read_excel(
    paste0(f, "raw/temps.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "temp_c", "temp_f", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_vent <- read_excel(
    paste0(f, "raw/vent.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "value", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time)

raw_lines <- read_excel(
    paste0(f, "raw/central_lines.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "entry_date", "entry_time", "event", "value", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hms(paste(entry_date, entry_time)))|> 
    select(-range_start, -range_end, -entry_date, -entry_time)

raw_csf <- read_excel(
    paste0(f, "raw/csf_drain.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "entry_date", "entry_time", "event", "value", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hms(paste(entry_date, entry_time)))|> 
    select(-range_start, -range_end, -entry_date, -entry_time)

raw_dialysis <- read_excel(
    paste0(f, "raw/dialysis.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "taken_date", "taken_time", "event", "value", "date_value", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    mutate(taken_datetime = ymd_hm(paste(taken_date, taken_time)))|> 
    select(-range_start, -range_end, -taken_date, -taken_time, -date_value)

raw_procedures <- read_excel(
    paste0(f, "raw/procedures.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "anesth_start_date_time", "procedure")
) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

df_dates <- raw_demog |> 
    select(mrn, encounter_csn, admit_datetime, disch_datetime) |> 
    mutate(
        admit_date = floor_date(admit_datetime, unit = "day"),
        disch_date = floor_date(disch_datetime, unit = "day")
    )

df_7jones <- raw_icu_los |>
    separate_longer_delim(vent_days, "\r\n") |> 
    mutate(across(vent_days, as.numeric)) |> 
    summarize(
        across(vent_days, sum),
        .by = c(mrn, nurse_unit, icu_start_datetime, icu_stop_datetime, icu_los)
    ) |> 
    add_count(mrn, name = "num_icu")

df_icu_los <- df_7jones |> 
    summarize(
        across(icu_los, sum),
        .by = mrn
    )

zzz_labs <- distinct(raw_labs, lab) |> arrange(lab)

df_labs <- raw_labs |> 
    inner_join(df_dates, by = "mrn", relationship = "many-to-many") |> 
    mutate(
        lab_date = floor_date(lab_datetime, unit = "day"),
        hosp_day = difftime(lab_date, admit_date, units = "days"),
        across(hosp_day, as.numeric)
    ) |> 
    filter(
        lab_date >= admit_date,
        lab_date <= disch_date
    )

df_procal <- df_labs |> 
    filter(
        lab == "Procalcitonin",
        hosp_day <= 14
    ) |> 
    select(mrn, encounter_csn, hosp_day, order_datetime, collect_datetime = lab_datetime, procal = value, nurse_unit)

df_wbc <- df_labs |> 
    filter(lab == "WBC") |> 
    mutate(across(value, as.numeric)) |> 
    summarize(
        wbc_max = max(value, na.rm = TRUE),
        .by = c(mrn, encounter_csn, hosp_day)
    )

df_scr_baseline <- df_labs |> 
    filter(lab == "Creatinine Lvl") |> 
    mutate(
        across(value, \(x) str_remove_all(x, "<|>")),
        across(value, as.numeric)
    ) |> 
    summarize(
        scr_baseline = first(value),
        .by = c(mrn, encounter_csn)
    )

df_temps <- raw_temps |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn"), relationship = "many-to-many") |> 
    mutate(
        taken_date = floor_date(taken_datetime, unit = "day"),
        hosp_day = difftime(taken_date, admit_date, units = "days"),
        across(hosp_day, as.numeric)
    ) |> 
    filter(!is.na(temp_c), !is.na(temp_f)) |> 
    summarize(
        # temp_c_max = max(temp_c, na.rm = TRUE),
        temp_f_max = max(temp_f, na.rm = TRUE),
        .by = c(mrn, encounter_csn, hosp_day)
    )

df_meds <- raw_meds |> 
    inner_join(df_dates, by = c("mrn", "encounter_csn"), relationship = "many-to-many") |> 
    mutate(
        med_date = floor_date(med_datetime, unit = "day"),
        hosp_day = difftime(med_date, admit_date, units = "days"),
        across(hosp_day, as.numeric)
    ) |> 
    separate_longer_delim(medication, "\r\n") |> 
    filter(!medication %in% c("dextrose", "sodium chloride", "sterile water", "water for injection sterile")) |> 
    arrange(mrn, encounter_csn, med_datetime, medication)

zzz_meds <- distinct(df_meds, medication) |> arrange(medication)

zzz_pressors <- c("angiotensin ii acetate", "dopamine in d5w", "epinephrine", "epinephrine-nacl", "norepinephrine bitartrate", 
                  "norepinephrine-sodium chloride", "phenylephrine hcl", "phenylephrine hcl (pressors)", 
                  "phenylephrine hcl-nacl", "vasopressin-sodium chloride")

zzz_steroids <- c("dexamethasone", "dexamethasone sodium phosphate", "fludrocortisone acetate", "hydrocortisone",
                  "hydrocortisone sod succinate", "methylprednisolone sodium succ", "prednisone")

zzz_exclude <- c("acyclovir", "acyclovir sodium", "amphotericin b liposome", "atovaquone", "bictegravir-emtricitab-tenofov",
                 "cytarabine", "entecavir", "erythromycin lactobionate", "ethambutol hcl", "fidaxomicin", "fluconazole",
                 "fluconazole in sodium chloride", "hydroxychloroquine sulfate", "isavuconazonium sulfate", "isoniazid",
                 "ivermectin", "methotrexate sodium", "micafungin sodium", "midodrine hcl", "nitrofurantoin monohyd macro",
                 "nystatin", "oseltamivir phosphate", "pyrazinamide", "rifaximin", "valacyclovir hcl", "voriconazole")

df_pressors <- df_meds |> 
    filter(
        medication %in% zzz_pressors,
        freq == "Continuous"
        # nurse_unit == "TMC JONES 7 ELECTIVE NEURO ICU"
    ) |> 
    distinct(mrn, encounter_csn) |> 
    mutate(pressors = TRUE)

df_steroids <- df_meds |> 
    filter(medication %in% zzz_steroids) |> 
    distinct(mrn, encounter_csn) |> 
    mutate(steroids = TRUE)

df_abx <- df_meds |> 
    filter(!medication %in% c(zzz_exclude, zzz_pressors, zzz_steroids)) |> 
    med_runtime(.id = encounter_csn) |> 
    summarize(
        across(dose_start, first),
        across(dose_stop, last),
        across(duration, sum),
        .by = c(encounter_csn, medication, course_count)
    ) |> 
    inner_join(df_dates, by = "encounter_csn") |> 
    mutate(
        across(duration, \(x) x / 24),
        start_date = floor_date(dose_start, unit = "day"),
        stop_date = floor_date(dose_stop, unit = "day"),
        start_hosp_day = difftime(start_date, admit_date, units = "days"),
        stop_hosp_day = difftime(stop_date, admit_date, units = "days"),
        across(c(start_hosp_day, stop_hosp_day), as.numeric)
    ) |> 
    select(mrn, encounter_csn, medication, start_hosp_day, stop_hosp_day, dose_start, dose_stop, duration)

df_cultures <- raw_cultures |> 
    inner_join(df_dates, by = "mrn", relationship = "many-to-many") |> 
    mutate(
        lab_date = floor_date(lab_datetime, unit = "day"),
        hosp_day = difftime(lab_date, admit_date, units = "days"),
        across(hosp_day, as.numeric)
    ) |> 
    filter(
        lab_date >= admit_date,
        lab_date <= disch_date
    ) |> 
    arrange(mrn, encounter_csn, lab_datetime)

df_culture_pos <- df_cultures |> 
    filter(
        !str_detect(value, regex("no growth|flora", ignore_case = TRUE)),
        !(lab == "Respiratory Culture" & str_detect(value, regex("yeast", ignore_case = TRUE)))
    ) |> 
    select(mrn, encounter_csn, specimen_id, hosp_day, lab_datetime, lab, culture = value)

df_culture_pos_pt <- df_culture_pos |> 
    distinct(mrn, encounter_csn, lab) |> 
    mutate(
        value = TRUE,
        across(lab, str_to_lower),
        across(lab, \(x) str_replace_all(x, " ", "_"))
    ) |> 
    pivot_wider(names_from = lab, values_from = value, names_prefix = "positive_")

df_gcs <- raw_gcs |> 
    arrange(mrn, encounter_csn, taken_datetime) |> 
    summarize(
        gcs_first = first(value),
        gcs_last = last(value),
        .by = c(mrn, encounter_csn)
    )

df_cvc <- raw_lines |> 
    distinct(mrn, encounter_csn) |> 
    mutate(central_line = TRUE)

df_csf <- raw_csf |> 
    distinct(mrn, encounter_csn) |> 
    mutate(csf_drain = TRUE)

df_vent <- raw_vent |> 
    filter(str_detect(event, "R VENTILATOR ON")) |> 
    distinct(mrn, encounter_csn) |> 
    mutate(vent = TRUE)

df_dialysis <- raw_dialysis |> 
    mutate(
        dialysis = case_when(
            str_detect(event, "HD") ~ "hd",
            str_detect(event, "CRRT") ~ "crrt"
        )
    ) |> 
    distinct(mrn, encounter_csn, dialysis) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = dialysis, values_from = value)

joinby <- join_by("mrn", "encounter_csn")

data_patients <- raw_demog |> 
    select(-length_stay) |> 
    left_join(df_icu_los, by = "mrn") |> 
    left_join(df_vent, by = joinby) |> 
    left_join(df_pressors, by = joinby) |> 
    left_join(df_gcs, by = joinby) |> 
    left_join(df_cvc, by = joinby) |> 
    left_join(df_csf, by = joinby) |> 
    left_join(df_scr_baseline, by = joinby) |> 
    left_join(df_dialysis, by = joinby) |> 
    left_join(df_culture_pos_pt, by = joinby)

data_daily <- df_wbc |>
    full_join(df_temps, by = c("mrn", "encounter_csn", "hosp_day")) |> 
    arrange(mrn, encounter_csn, hosp_day)
    
l <- list(
    "patients" = data_patients,
    "procal" = df_procal,
    "abx" = df_abx,
    "daily_vals" = data_daily
)

write.xlsx(l, paste0(f, c("final/procal_data.xlsx")), overwrite = TRUE)
