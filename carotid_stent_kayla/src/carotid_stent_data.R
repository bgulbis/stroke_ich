library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "carotid_stent_kayla")

raw_screen <- get_xlsx_data(paste0(f, "raw"), pattern = "screening")

mbo_fin <- concat_encounters(raw_screen$fin)
print(mbo_fin)

raw_demographics <- read_excel(paste0(f, "raw/demographics.xlsx")) |>
    rename_all(str_to_lower)

raw_diagnosis <- read_excel(paste0(f, "raw/diagnosis.xlsx")) |>
    rename_all(str_to_lower)

raw_home_meds <- read_excel(paste0(f, "raw/home_meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_labs_vitals <- read_excel(paste0(f, "raw/labs_vitals.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(c(event, result_val), str_to_lower))

df_labs_vitals <- raw_labs_vitals |> 
    filter(!str_detect(result_val, "[a-z]")) |> 
    mutate(
        censor_low = str_detect(result_val, "<"),
        censor_high = str_detect(result_val, ">"),
        across(result_val, \(x) str_remove_all(x, "<|>")),
        across(result_val, as.numeric)
    ) 
    
raw_locations <- read_excel(paste0(f, "raw/locations.xlsx")) |>
    rename_all(str_to_lower)

raw_meds <- read_excel(paste0(f, "raw/meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

zz_labs <- distinct(raw_labs_vitals, event) |> arrange(event)
zz_hm_meds <- distinct(raw_home_meds, drug_cat) |> arrange(drug_cat)
zz_meds <- distinct(raw_meds, medication) |> arrange(medication)

df_nihss <- raw_labs_vitals |> 
    filter(event == "nih stroke score") |> 
    arrange(encntr_id, event_datetime) |> 
    mutate(across(result_val, as.numeric))

df_nihss_admit <- df_nihss |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, nihss_admit = result_val)

df_diag_primary <- raw_diagnosis |> 
    filter(diag_priority == 1) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, primary_icd10 = icd_10_code, icd_10_description)

l_drug_cats <- c(
    "ACE inhibitors with thiazides",
    "HMG-CoA reductase inhibitors (statins)",
    "PCSK9 inhibitors",
    "alpha-adrenoreceptor antagonists",
    "angiotensin II inhibitors",
    "angiotensin II inhibitors with thiazides",
    "angiotensin converting enzyme (ACE) inhibitors",
    "angiotensin receptor blockers and neprilysin inhibitors",
    "antiadrenergic agents, centrally acting",
    "antiadrenergic agents, peripherally acting",
    "antihyperlipidemic combinations",
    "beta blockers with thiazides",
    "beta blockers, cardioselective",
    "beta blockers, non-cardioselective",
    "calcium channel blocking agents",
    "cholesterol absorption inhibitors",
    "coumarins and indanediones",
    "factor Xa inhibitors",
    "fibric acid derivatives",
    "miscellaneous antihyperlipidemic agents",
    "miscellaneous cardiovascular agents",
    "miscellaneous coagulation modifiers",
    "platelet aggregation inhibitors",
    "thiazide and thiazide-like diuretics",
    "vasodilators"
)

df_home_meds <- raw_home_meds |> 
    filter(drug_cat %in% l_drug_cats)

df_arrive <- select(raw_demographics, encntr_id, arrive_datetime)

df_tpa <- raw_meds |> 
    filter(medication %in% c("alteplase", "tenecteplase")) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_tpa_hrs = difftime(med_datetime, arrive_datetime, units = "hours"),
        across(arrive_tpa_hrs, as.numeric)
    ) |> 
    select(encntr_id, fin, med_datetime, medication:admin_route, arrive_tpa_hrs)

df_fibrinolytic <- df_tpa |> 
    distinct(encntr_id) |> 
    mutate(fibrinolytic = TRUE)

l_labs_baseln <- c(
    "alk phos",
    "alt",
    "ast",
    "bili total",
    "bun",
    "creatinine lvl",
    "glucose lvl",
    "hgb",
    "hct",
    "platelet",
    "inr",
    "pt",
    "ptt",
    "fibrinogen lvl",
    "hgb a1c",
    "hdl",
    "ldl (calculated)",
    "ldl direct",
    "chol",
    "trig"
)

df_labs_baseln <- df_labs_vitals |> 
    filter(event %in% l_labs_baseln) |> 
    arrange(encntr_id, event_datetime) |> 
    distinct(encntr_id, event, .keep_all = TRUE) |> 
    select(encntr_id, event, result_val) |> 
    mutate(
        across(event, \(x) str_remove_all(x, " lvl|\\(|\\)")),
        across(event, \(x) str_replace_all(x, " ", "_"))
    ) |> 
    pivot_wider(names_from = event, values_from = result_val, names_prefix = "admit_", names_sort = TRUE)

df_bp <- df_labs_vitals |> 
    filter(
        str_detect(event, "systolic|diastolic"),
        !censor_low,
        !censor_high
    ) |> 
    arrange(encntr_id, event_datetime) |> 
    select(encntr_id, fin, event_datetime, event, result_val)

df_bp_admit <- df_bp |> 
    mutate(sbp = str_detect(event, "systolic")) |> 
    distinct(encntr_id, sbp, .keep_all = TRUE) |> 
    mutate(across(sbp, \(x) if_else(x, "sbp", "dbp"))) |> 
    select(-fin, -event, -event_datetime) |> 
    pivot_wider(names_from = sbp, values_from = result_val, names_prefix = "admit_")

df_eptif_bolus <- raw_meds |> 
    filter(
        medication == "eptifibatide",
        is.na(iv_event)
    ) 

df_eptif_drip <- raw_meds |> 
    filter(
        medication == "eptifibatide",
        !is.na(iv_event)
    ) |> 
    mutate(rate_adj = if_else(!is.na(rate_unit), rate, NA_real_)) |> 
    group_by(encntr_id) |> 
    fill(rate_adj, rate_unit, .direction = "downup") 
    # drip_runtime(.rate = rate_adj) |> 
    # summarize_drips(.rate = rate_adj)

data_patients <- raw_demographics |> 
    select(encntr_id:weight, los, admit_datetime, disch_datetime, disch_disposition) |> 
    left_join(df_diag_primary, by = "encntr_id") |> 
    left_join(df_fibrinolytic, by = "encntr_id") |> 
    left_join(df_nihss_admit, by = "encntr_id") |> 
    left_join(df_bp_admit, by = "encntr_id") |> 
    left_join(df_labs_baseln, by = "encntr_id") |> 
    select(-encntr_id)

data_nihss <- df_nihss |> 
    select(fin, event_datetime, nihss = result_val)

data_home_meds <- df_home_meds |> 
    select(-encntr_id, -order_id)

data_tpa <- select(df_tpa, -encntr_id)

data_bp <- df_bp |> 
    distinct(encntr_id, event, event_datetime, .keep_all = TRUE) |> 
    pivot_wider(names_from = event, values_from = result_val) |> 
    select(-encntr_id)
