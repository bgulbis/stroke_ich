library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)
# library(themebg)
# library(broom)

f <- set_data_path("stroke_ich", "cva_acei_kyndol")

raw_screen <- read_excel(paste0(f, "raw/patient_screening.xlsx")) |> 
    rename_all(str_to_lower)

# x <- count(raw_screen, med_group)

mbo_id <- concat_encounters(raw_screen$encntr_id)
print(mbo_id)

raw_contrast <- get_xlsx_data(paste0(f, "raw/"), "contrast_orders")

raw_demographics <- get_xlsx_data(paste0(f, "raw/"), "demographics")

raw_diagnosis <- get_xlsx_data(paste0(f, "raw/"), "diagnosis")

raw_dialysis <- get_xlsx_data(paste0(f, "raw/"), "dialysis")

raw_ins_outs <- get_xlsx_data(paste0(f, "raw/"), "ins_outs")

raw_labs_vitals <- get_xlsx_data(paste0(f, "raw/"), "labs_vitals") |> 
    mutate(
        across(event, str_to_lower),
        censor_low = str_detect(result_val, "<"),
        censor_high = str_detect(result_val, ">"),
        across(result_val, str_replace_all, pattern = "<|>", replacement = ""),
        across(result_val, as.numeric)
    )

raw_meds <- get_xlsx_data(paste0(f, "raw/"), "meds") |> 
    mutate(across(medication, str_to_lower))

df_cva_type <- raw_diagnosis |> 
    filter(
        str_detect(icd_10_code, "^I63|^I61"),
        diag_priority == 1
    ) |> 
    mutate(cva_type = if_else(str_detect(icd_10_code, "^I63"), "ais", "ich")) |> 
    distinct(encntr_id, cva_type) 

excl_mult_cva <- df_cva_type |> 
    count(encntr_id) |> 
    filter(n > 1)

df_diag_concom <- raw_diagnosis |> 
    filter(icd_10_code %in% c("I21.4", "I21.3", "A41.9", "N39.0", "E11.10", "E10.10")) |> 
    mutate(
        name = case_when(
            icd_10_code == "I21.4" ~ "nstemi",
            icd_10_code == "^I21.3" ~ "stemi",
            icd_10_code == "A41.9" ~ "sepsis",
            icd_10_code == "N39.0" ~ "uti",
            icd_10_code %in% c("E11.10", "E10.10") ~ "dka"
        ),
        value = TRUE
    ) |> 
    select(encntr_id, name, value) |> 
    pivot_wider()

df_drips <- raw_meds |> 
    filter(!is.na(iv_event)) |> 
    drip_runtime() |> 
    filter(!is.na(rate)) |>
    summarize_drips()

df_meds <- raw_meds |> 
    filter(is.na(iv_event)) |> 
    med_runtime() |> 
    group_by(encntr_id, medication, course_count) |> 
    summarize(
        across(dose_start, first),
        across(dose_stop, last),
        across(c(num_doses, duration), sum, na.rm = TRUE),
        across(dose, list(first = first, last = last, max = max, min = min), .names = "{.fn}_{.col}"),
        .groups = "drop"
    )

df_tpa <- raw_meds |> 
    filter(medication %in% c("alteplase", "reteplase")) |> 
    distinct(encntr_id) |> 
    mutate(tpa = TRUE)

df_gcs <- raw_labs_vitals |> 
    filter(event == "glasgow coma score") |> 
    arrange(encntr_id, event_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, gcs_baseline = result_val)

df_labs <- raw_labs_vitals |> 
    filter(event %in% c("creatinine lvl", "bun", "potassium lvl")) |> 
    mutate(across(event, str_replace_all, pattern = " lvl", replacement = "")) |> 
    select(-event_id, -nurse_unit, -censor_low, -censor_high) |> 
    distinct(encntr_id, event, .keep_all = TRUE) |> 
    pivot_wider(names_from = event, values_from = result_val)
    
df_bp <- raw_labs_vitals |> 
    filter(str_detect(event, "systolic|diastolic"))

df_bp_first <- df_bp |> 
    mutate(
        across(
            event,
            ~case_when(
                str_detect(., "systolic") ~ "sbp",
                str_detect(., "diastolic") ~ "dbp"
            )
        )
    ) |> 
    arrange(encntr_id, event_datetime, event) |> 
    distinct(encntr_id, event, .keep_all = TRUE) |> 
    select(encntr_id, event, result_val) |> 
    pivot_wider(names_from = event, values_from = result_val)

df_bp_low <- df_bp |> 
    filter(
        (str_detect(event, "systolic") & result_val < 90) |
            (str_detect(event, "diastolic") & result_val < 60)
    )
    
df_dialysis <- raw_dialysis |> 
    distinct(encntr_id, event) |> 
    mutate(
        across(
            event, 
            ~case_when(
                . == "CRRT Actual Pt Fluid Removed Vol" ~ "crrt",
                . == "Hemodialysis Output Volume" ~ "hemodialysis"
            )
        ),
        value = TRUE
    ) |> 
    pivot_wider(names_from = event)

data_patients <- raw_demographics |> 
    inner_join(df_cva_type, by = "encntr_id") |> 
    anti_join(excl_mult_cva, by = "encntr_id") |> 
    left_join(df_gcs, by = "encntr_id") |> 
    left_join(df_diag_concom, by = "encntr_id") |> 
    left_join(df_bp_first, by = "encntr_id") |> 
    left_join(df_tpa, by = "encntr_id") |> 
    left_join(df_dialysis, by = "encntr_id")

data_meds <- df_meds |> 
    inner_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    anti_join(excl_mult_cva, by = "encntr_id") |> 
    select(fin, everything(), -encntr_id)

data_drips <- df_drips |> 
    inner_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    anti_join(excl_mult_cva, by = "encntr_id") |> 
    select(fin, everything(), -encntr_id)

data_labs <- df_labs |> 
    inner_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    anti_join(excl_mult_cva, by = "encntr_id") |> 
    select(fin, everything(), -encntr_id)

data_contrast <- raw_contrast |> 
    inner_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    anti_join(excl_mult_cva, by = "encntr_id") |> 
    select(fin, order_datetime, product, dose_quantity)

x <- list(
    "patients" = data_patients,
    "meds" = data_meds,
    "drips" = data_drips,
    "labs" = data_labs,
    "contrast" = data_contrast
)

write.xlsx(x, paste0(f, "final/cva_acei_data.xlsx"), overwrite = TRUE)
