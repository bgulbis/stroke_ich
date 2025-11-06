library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "isc_jessica")

raw_fins <- read_excel(paste0(f, "raw/fin_list.xlsx"), col_types = "text") |> 
    rename_all(str_to_lower)

mbo_fin <- concat_encounters(raw_fins$fin)
print(mbo_fin)

raw_demog <- read_excel(paste0(f, "raw/demographics.xlsx")) |> 
    rename_all(str_to_lower)

raw_diagnosis <- read_excel(paste0(f, "raw/diagnosis.xlsx")) |> 
    rename_all(str_to_lower)

raw_dialysis <- read_excel(paste0(f, "raw/dialysis.xlsx")) |> 
    rename_all(str_to_lower)

raw_home_meds <- read_excel(paste0(f, "raw/home_meds.xlsx")) |> 
    rename_all(str_to_lower)

raw_labs_vitals <- read_excel(paste0(f, "raw/labs_vitals.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

raw_meds <- read_excel(paste0(f, "raw/meds.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_procs <- read_excel(paste0(f, "raw/procedures.xlsx")) |> 
    rename_all(str_to_lower)

df_diagnosis <- raw_diagnosis |> 
    mutate(
        pmh = case_when(
            str_detect(icd_10_code, "I10|I15.2|I15.8|I15.9") ~ "htn",
            str_detect(icd_10_code, "I63.9") ~ "cva",
            str_detect(icd_10_code, "E78.5") ~ "hld",
            str_detect(icd_10_code, "I50") ~ "chf",
            str_detect(icd_10_code, "I48") ~ "afib",
            str_detect(icd_10_code, "E11") ~ "dm",
            str_detect(icd_10_code, "N18|E08.22|E09.22|E10.22|E11.22|E13.22|I12|I13") ~ "ckd",
            str_detect(icd_10_code, "N18.6") ~ "esrd",
            str_detect(icd_10_code, "I25") ~ "cad",
            str_detect(icd_10_code, "I24.9|I20.0|I21|I22") ~ "acs"
        )
    ) |> 
    filter(!is.na(pmh)) |> 
    distinct(encntr_id, pmh) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = pmh, values_from = value, names_sort = TRUE)

df_complications <- raw_diagnosis |> 
    mutate(
        complication = case_when(
            str_detect(icd_10_code, "N39") ~ "uti",
            str_detect(icd_10_code, "I69.1|I61.8|I61.9|G97.32") ~ "hemorhg_transform",
            str_detect(icd_10_code, "G31.9") ~ "neuro_worsen",
            str_detect(icd_10_code, "J69") ~ "asp_pna",
            str_detect(icd_10_code, "I82.4") ~ "dvt",
            str_detect(icd_10_code, "I26") ~ "pe",
            str_detect(icd_10_code, "I63|I61") ~ "stroke",
            str_detect(icd_10_code, "R58") ~ "hemorhg_complication",
            str_detect(icd_10_code, "I61") ~ "symptom_ich",
            str_detect(icd_10_code, "I61.8|I61.9") ~ "nontraum_ich",
            str_detect(icd_10_code, "T78.3") ~ "angioedema",
            str_detect(icd_10_code, "R99") ~ "death"
        )
    ) |> 
    filter(!is.na(complication)) |> 
    distinct(encntr_id, complication) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = complication, values_from = value, names_sort = TRUE, names_prefix = "complication_")

df_procs <- raw_procs |> 
    distinct(encntr_id, icd_10_pcs) |> 
    select(encntr_id, thrombectomy = icd_10_pcs)
    
df_labs_admit <- raw_labs_vitals |> 
    filter(event %in% c("glasgow coma score", "nih stroke score", "systolic blood pressure", "arterial systolic bp 1")) |> 
    distinct(encntr_id, event, .keep_all = TRUE) |> 
    left_join(raw_demog[c("encntr_id", "arrive_datetime")], by = "encntr_id") |> 
    filter(event_datetime <= arrive_datetime + hours(12)) |> 
    mutate(
        event_abbrev = case_when(
            event == "glasgow coma score" ~ "gcs",
            event == "nih stroke score" ~ "nihss",
            event == "systolic blood pressure" ~ "sbp",
            event == "arterial systolic bp 1" ~ "sbp_art"
        )
    ) |> 
    select(encntr_id, event_abbrev, result_val) |> 
    pivot_wider(names_from = event_abbrev, values_from = result_val, names_sort = TRUE, names_prefix = "admit_")

# df_sbp_admit <- raw_labs_vitals |> 
#     filter(
#         event %in% c("systolic blood pressure", "arterial systolic bp 1"),
#         !str_detect(result_val, "[A-Za-z]")
#     ) |> 
#     distinct(encntr_id, .keep_all = TRUE) |> 
#     select(encntr_id, sbp_admit = result_val)

df_sbp_24h <- raw_labs_vitals |> 
    inner_join(raw_demog[c("encntr_id", "admit_datetime")], by = "encntr_id") |> 
    filter(
        event %in% c("systolic blood pressure", "arterial systolic bp 1"),
        !str_detect(result_val, "[A-Za-z]"),
        event_datetime <= admit_datetime + hours(24)
    ) |> 
    arrange(encntr_id, event_datetime)

df_sbp_lt90 <- raw_labs_vitals |> 
    filter(
        event %in% c("systolic blood pressure", "arterial systolic bp 1"),
        !str_detect(result_val, "[A-Za-z]")
    ) |> 
    mutate(
        across(result_val, \(x) str_remove_all(x, "<|>")),
        across(result_val, as.numeric)
    ) |> 
    filter(result_val < 90) 
    
df_sbp_low <- df_sbp_lt90 |> 
    summarize(sbp_low_datetime = min(event_datetime), .by = encntr_id)

df_egfr_low <- raw_labs_vitals |> 
    filter(
        event == "egfr",
        !str_detect(result_val, "[A-Za-z]")
    ) |> 
    mutate(
        across(result_val, \(x) str_remove_all(x, "<|>")),
        across(result_val, as.numeric)
    ) |> 
    filter(result_val < 30) |> 
    summarize(egfr_low_datetime = min(event_datetime), .by = encntr_id)

df_dialysis <- raw_dialysis |> 
    distinct(encntr_id, event) |> 
    mutate(
        dialysis = case_when(
            str_detect(event, regex("hemodialysis", ignore_case = TRUE)) ~ "hd",
            str_detect(event, regex("crrt", ignore_case = TRUE)) ~ "crrt"
        ),
        value = TRUE
    ) |> 
    select(encntr_id, dialysis, value) |> 
    pivot_wider(names_from = dialysis, values_from = value)

zz_home_meds <- distinct(raw_home_meds, drug_cat) |> arrange(drug_cat)

df_home_meds <- raw_home_meds |> 
    filter(
        str_detect(drug_cat, regex("ace inhibitors|angiotensin", ignore_case = TRUE)) |
            str_detect(medication, regex("olmesartan", ignore_case = TRUE))
    ) |> 
    mutate(
        home_med = case_when(
            str_detect(drug_cat, regex("ace inhibitors|angiotensin convert", ignore_case = TRUE)) ~ "acei",
            str_detect(drug_cat, regex("angiotensin ii", ignore_case = TRUE)) | str_detect(medication, regex("olmesartan", ignore_case = TRUE)) ~ "arb",
            drug_cat == "angiotensin receptor blockers and neprilysin inhibitors" ~ "arni"
        ),
        value = TRUE
    ) |> 
    filter(!is.na(home_med)) |> 
    distinct(encntr_id, home_med, value) |> 
    pivot_wider(names_from = home_med, values_from = value, names_prefix = "home_")

zz_meds <- distinct(raw_meds, medication) |> arrange(medication)

df_tpa <- raw_meds |> 
    filter(medication %in% c("alteplase", "tenecteplase")) |> 
    distinct(encntr_id, medication) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = medication, values_from = value)

df_bp_meds <- raw_meds |> 
    filter(!medication %in% c("alteplase", "tenecteplase")) |> 
    med_runtime() |> 
    summarize(
        across(dose_start, min),
        across(dose_stop, max),
        across(duration, sum),
        .by = c(encntr_id, medication, course_count)
    ) |> 
    mutate(
        med_grp = case_when(
            medication %in% c("captopril", "enalapril", "lisinopril") ~ "acei",
            medication %in% c("losartan", "olmesartan", "valsartan") ~ "arb",
            medication == "sacubitril-valsartan" ~ "arni",
            .default = "other_bp"
        )
    ) |> 
    summarize(
        across(dose_start, min),
        across(duration, sum),
        .by = c(encntr_id, med_grp)
    ) |> 
    left_join(raw_demog[c("encntr_id", "admit_datetime")], by = "encntr_id") |> 
    mutate(
        hosp_day = difftime(dose_start, admit_datetime, units = "days"),
        across(hosp_day, as.numeric),
        across(duration, \(x) x / 24),
        across(c(hosp_day, duration), ceiling),
        .by = c(encntr_id, med_grp)
    ) |> 
    select(encntr_id, med_grp, hosp_day, duration) |> 
    pivot_wider(names_from = med_grp, values_from = c(hosp_day, duration), names_glue = "{med_grp}_{.value}", names_sort = TRUE)

df_bp_agents <- raw_meds |> 
    filter(
        !medication %in% c("alteplase", "tenecteplase", "nitroglycerin"),
        is.na(iv_event)
    ) |> 
    med_runtime() |> 
    summarize(
        across(dose_start, min),
        across(dose_stop, max),
        across(c(duration, num_doses), sum),
        .by = c(encntr_id, medication, course_count)
    ) |> 
    summarize(
        # across(dose_start, min),
        # across(c(duration, num_doses), sum),
        across(num_doses, sum),
        .by = c(encntr_id, medication)
    ) |> 
    pivot_wider(names_from = medication, values_from = num_doses, names_sort = TRUE)

df_bp_drips <- raw_meds |> 
    filter(
        !medication %in% c("alteplase", "tenecteplase"),
        !is.na(iv_event)
    ) |> 
    drip_runtime() |> 
    filter(!is.na(rate)) |> 
    summarize_drips() |> 
    summarize(
        across(duration, sum),
        .by = c(encntr_id, medication)
    ) |> 
    pivot_wider(names_from = medication, values_from = duration, names_sort = TRUE, names_prefix = "drip_")

data_patients <- raw_demog |> 
    left_join(df_diagnosis, by = "encntr_id") |> 
    left_join(df_procs, by = "encntr_id") |> 
    left_join(df_labs_admit, by = "encntr_id") |> 
    # left_join(df_sbp_admit, by = "encntr_id") |> 
    left_join(df_sbp_low, by = "encntr_id") |> 
    left_join(df_egfr_low, by = "encntr_id") |> 
    left_join(df_dialysis, by = "encntr_id") |> 
    left_join(df_home_meds, by = "encntr_id") |> 
    left_join(df_bp_meds, by = "encntr_id") |> 
    left_join(df_bp_agents, by = "encntr_id") |> 
    left_join(df_bp_drips, by = "encntr_id") |> 
    left_join(df_tpa, by = "encntr_id") |> 
    left_join(df_complications, by = "encntr_id") |> 
    select(-encntr_id)

data_sbp_lt90 <- df_sbp_lt90 |> 
    select(-encntr_id, -event_id)

data_sbp_24h <- df_sbp_24h |> 
    select(-encntr_id, -event_id)

l <- list(
    "patients" = data_patients,
    "sbp_lt90" = data_sbp_lt90,
    "sbp_first_24h" = data_sbp_24h
)

write.xlsx(l, paste0(f, "final/jessica_data.xlsx"), overwrite = TRUE)
