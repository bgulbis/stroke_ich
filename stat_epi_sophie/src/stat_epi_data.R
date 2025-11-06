library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "stat_epi_sophie")

raw_pts <- read_excel(paste0(f, "raw/patients.xlsx")) |> 
    rename_all(str_to_lower) 

mbo_id <- concat_encounters(raw_pts$encntr_id)
print(mbo_id)

raw_demographics <- get_xlsx_data(paste0(f, "raw"), "demographics") 

raw_diagnosis <- get_xlsx_data(paste0(f, "raw"), "diagnosis") 
    
raw_dialysis <- get_xlsx_data(paste0(f, "raw"), "dialysis") 

raw_labs_vitals <- get_xlsx_data(paste0(f, "raw"), "labs_vitals") |> 
    mutate(across(event, str_to_lower))

raw_locations <- get_xlsx_data(paste0(f, "raw"), "locations")

raw_meds <- get_xlsx_data(paste0(f, "raw"), "meds") |>  
    mutate(across(medication, str_to_lower))

raw_vent_extubations <- get_xlsx_data(paste0(f, "raw"), "vent_extubation")

raw_vent_times <- get_xlsx_data(paste0(f, "raw"), "vent_times")

tmp_vent_last <- raw_vent_times |>
    filter(event == "Vent Stop Time") |>
    arrange(encntr_id, result_datetime) |>
    summarize(across(result_datetime, \(x) max(x, na.rm = TRUE)), .by = encntr_id) |>
    rename(last_vent_datetime = result_datetime)

df_vent <- raw_vent_extubations |>
    bind_rows(raw_vent_times) |>
    filter(event %in% c("Vent Start Time", "Extubation Event")) |>
    mutate(
        across(result_datetime, \(x) coalesce(x, event_datetime)),
        across(event, \(x) str_replace_all(x, pattern = c("Vent Start Time" = "intubation", "Extubation Event" = "extubation")))
    ) |>
    arrange(encntr_id, result_datetime) |>
    mutate(new_event = event != lag(event) | is.na(lag(event)), .by = encntr_id) |>
    mutate(across(new_event, cumsum)) |>
    distinct(encntr_id, new_event, .keep_all = TRUE) |>
    filter(!(new_event == 1 & event == "extubation")) |>
    mutate(
        vent = TRUE,
        vent_n = cumsum(vent),
        .by = c(encntr_id, event)
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
    left_join(raw_demographics[c("encntr_id", "disch_datetime")], by = "encntr_id") |>
    mutate(
        across(extubate_datetime, \(x) coalesce(x, last_vent_datetime)),
        across(extubate_datetime, \(x) coalesce(x, disch_datetime)),
        across(extubate_datetime, \(x) if_else(x < intubate_datetime, disch_datetime, x)),
        vent_days = difftime(extubate_datetime, intubate_datetime, units = "days"),
        across(vent_days, as.numeric)
    ) |>
    summarize(
        across(vent_days, sum),
        .by = encntr_id
    )

df_times <- select(raw_demographics, encntr_id, arrive_datetime)

zz_labs <- distinct(raw_labs_vitals, event) |> arrange(event)

df_labs <- raw_labs_vitals |> 
    select(-event_datetime_csv) |> 
    inner_join(df_times, by = "encntr_id") |> 
    filter(!str_detect(result_val, "[A-Za-z]")) |> 
    mutate(
        event_day = difftime(event_datetime, arrive_datetime, units = "days"),
        across(event_day, as.numeric),
        across(event_day, ceiling),
        # across(event_day, \(x) x + 1),
        censor_high = str_detect(result_val, ">"),
        censor_low = str_detect(result_val, "<"),
        across(result_val, \(x) str_replace_all(x, ">|<|,", "")),
        across(result_val, as.numeric)
    )

df_sbp <- df_labs |> 
    filter(
        str_detect(event, "systolic"),
        event_day > 0,
        event_day <= 7,
        result_val >= 20
    ) |> 
    mutate(
        sbp_gt_160 = result_val >= 160,
        sbp_lt_100 = result_val <= 100,
        sbp_lt_90 = result_val <= 90
    ) |> 
    summarize(
        mean_sbp = mean(result_val),
        across(c(sbp_gt_160, sbp_lt_100, sbp_lt_90), sum),
        .by = c(encntr_id, event_day)
    ) |> 
    arrange(encntr_id, event_day) |> 
    pivot_wider(names_from = event_day, values_from = c(mean_sbp, starts_with("sbp_")))

df_map <- df_labs |> 
    filter(
        str_detect(event, "mean arterial"),
        event_day > 0,
        event_day <= 7,
        result_val >= 20
    ) |> 
    mutate(map_lt_65 = result_val <= 65) |> 
    summarize(
        mean_map = mean(result_val),
        across(map_lt_65, sum),
        .by = c(encntr_id, event_day)
    ) |> 
    arrange(encntr_id, event_day) |> 
    pivot_wider(names_from = event_day, values_from = c(mean_map, map_lt_65))

df_labs_mean <- df_labs |> 
    filter(
        event %in% c("creatinine lvl", "lactic acid lvl", "poc a la", "troponin-i"),
        event_day > 0,
        event_day <= 7,
        result_val >= 0
    ) |> 
    mutate(
        event_grp = case_when(
            event == "creatinine lvl" ~ "scr",
            event == "troponin-i" ~ "troponin",
            event %in% c("lactic acid lvl", "poc a la") ~ "lactate"
        )
    ) |> 
    summarize(
        across(result_val, mean),
        .by = c(encntr_id, event_grp, event_day)
    ) |> 
    arrange(encntr_id, event_grp, event_day) |> 
    pivot_wider(names_from = c(event_grp, event_day), values_from = result_val, names_sort = TRUE)

df_drips <- raw_meds |> 
    filter(!is.na(iv_event)) |> 
    drip_runtime() |> 
    filter(!is.na(rate)) |> 
    summarize_drips() |> 
    summarize(
        across(duration, sum),
        .by = c(encntr_id, medication)
    ) |> 
    pivot_wider(names_from = medication, values_from = duration, names_sort = TRUE, names_glue = "{medication}_duration_hrs")

df_meds <- raw_meds |> 
    filter(
        is.na(iv_event),
        is.na(prn)
    ) |> 
    med_runtime() |> 
    summarize(
        across(num_doses, sum),
        .by = c(encntr_id, medication)
    ) |> 
    pivot_wider(names_from = medication, values_from = num_doses, names_sort = TRUE, names_glue = "{medication}_doses")

zz_meds <- distinct(raw_meds, medication) |> arrange(medication)
zz_routes <- distinct(raw_meds, admin_route) |> arrange(admin_route)

df_prn <- raw_meds |> 
    filter(
        medication %in% c("hydralazine", "labetalol", "enalapril"),
        is.na(iv_event),
        !is.na(prn),
        str_detect(admin_route, "IV")
    ) |> 
    med_runtime() |> 
    summarize(
        across(num_doses, sum),
        .by = c(encntr_id, medication)
    ) |> 
    pivot_wider(names_from = medication, values_from = num_doses, names_sort = TRUE, names_glue = "{medication}_prn_doses")

df_dialysis <- raw_dialysis |> 
    mutate(
        event_grp = case_when(
            str_detect(event, "CRRT") ~ "crrt",
            str_detect(event, "Hemodialysis") ~ "hemodialysis",
            str_detect(event, "Peritoneal") ~ "peritoneal_dialysis"
        )
    ) |> 
    distinct(encntr_id, event_grp) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = event_grp)

df_diagnosis_mi <- raw_diagnosis |> 
    filter(str_detect(icd_10_code, "I21.[0-9]|I25.10")) |> 
    mutate(
        icd_grp = case_when(
            str_detect(icd_10_code, "I21") ~ "icd_I21",
            str_detect(icd_10_code, "I25") ~ "icd_I25"
        ),
        value = TRUE
    ) |> 
    distinct(encntr_id, icd_grp, value) |> 
    pivot_wider(names_from = icd_grp)

zz_units <- distinct(raw_locations, nurse_unit) |> arrange(nurse_unit)

data_patients <- raw_demographics |> 
    left_join(df_vent, by = "encntr_id") |> 
    left_join(df_sbp, by = "encntr_id") |> 
    left_join(df_map, by = "encntr_id") |> 
    left_join(df_labs_mean, by = "encntr_id") |> 
    left_join(df_meds, by = "encntr_id") |> 
    left_join(df_drips, by = "encntr_id") |> 
    left_join(df_prn, by = "encntr_id") |> 
    left_join(df_dialysis, by = "encntr_id") |> 
    left_join(df_diagnosis_mi, by = "encntr_id") |> 
    select(-encntr_id, -ends_with("datetime"))

data_diagnosis <- raw_diagnosis |> 
    inner_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, everything(), -encntr_id)
    
data_locations <- raw_locations |> 
    inner_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, everything(), -encntr_id, -ends_with("datetime"))

l <- list(
    "patients" = data_patients,
    "diagnosis" = data_diagnosis,
    "nurse_unit_los" = data_locations
)

write.xlsx(l, paste0(f, "final/stat_epi_data.xlsx"), overwrite = TRUE)
