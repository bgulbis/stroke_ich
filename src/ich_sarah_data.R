library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)
library(themebg)
library(broom)

f <- set_data_path("stroke_ich", "ich_sarah")

tz <- locale(tz = "US/Central")

extra_fins <- read_excel(paste0(f, "raw/extra_fins.xlsx"))
    # mutate(across(fin, as.character))

mbo_extras <- concat_encounters(extra_fins$fin)
print(mbo_extras)

# raw_pts <- read_excel(paste0(f, "raw/patient_list.xlsx")) |>
#     rename_all(str_to_lower) |>
#     mutate(
#         across(fin, as.numeric),
#         # across(c(admit_src, disch_disposition), str_to_lower),
#         across(starts_with("excl_"), as.logical)
#     )

rehabs <- c("TR TIRR", "GH Rehab", "HH Rehab", "HH Trans Care", "KR Katy Rehab", "SE REHAB")

raw_pts <- read_csv(paste0(f, "raw/patients.csv")) |>
    rename_all(str_to_lower) |> 
    group_by(encntr_id) |> 
    mutate(
        across(fin, as.character),
        start_datetime = if_else(
            tfr_facility %in% rehabs, 
            arrive_datetime - hours(12),
            min(arrive_datetime, tfr_arrive_datetime, na.rm = TRUE), 
            arrive_datetime
        )
    )

df_include <- raw_pts |>
    group_by(encntr_id) |>
    mutate(exclude = sum(excl_pregnant, excl_transfer, excl_early_death, na.rm = TRUE)) |>
    filter(exclude == 0, first_sbp >= 150) |> 
    ungroup() |> 
    select(fin, encntr_id, admit_datetime, start_datetime)

mbo_fin <- concat_encounters(df_include$fin, 950)
print(mbo_fin)


# raw data ----------------------------------------------------------------

raw_codes <- read_csv(paste0(f, "raw/codes.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_diagnosis <- read_csv(paste0(f, "raw/diagnosis.csv")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_glucoses <- read_csv(paste0(f, "raw/glucoses.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_labs <- read_csv(paste0(f, "raw/labs.csv")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_locations <- read_csv(paste0(f, "raw/locations.csv")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character)) |> 
    arrange(fin, unit_count)

raw_meds_bp <- read_csv(paste0(f, "raw/meds_bp.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower)) |> 
    mutate(across(fin, as.character))

raw_meds_vasop <- read_csv(paste0(f, "raw/meds_vasop.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_outpt_meds <- read_csv(paste0(f, "raw/outpt_meds.csv")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_sbp <- read_csv(paste0(f, "raw/sbp.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    arrange(fin, event_datetime, event_id) |> 
    mutate(across(fin, as.character))

raw_surgeries <- read_csv(paste0(f, "raw/surgeries.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_icp <- read_csv(paste0(f, "raw/icp.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_readmits <- read_csv(paste0(f, "raw/readmits.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character))

raw_imaging <- read_csv(paste0(f, "raw/imaging.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, as.character)) |> 
    arrange(fin, event_datetime)

df_imaging <- raw_imaging |> 
    inner_join(df_include, by = "fin") |> 
    filter(event_datetime > start_datetime) |> 
    mutate(event_hour = floor_date(event_datetime, unit = "hours")) |> 
    distinct(fin, event_hour, .keep_all = TRUE) |> 
    count(fin, name = "num_imaging")
    
df_diagnosis <- raw_diagnosis |> 
    mutate(
        icd_code = str_sub(icd_10_code, end = 3L),
        icd_group = case_when(
            icd_code == "I48" ~ "afib",
            icd_code == "F17" ~ "smoker",
            icd_code == "I68" ~ "cereb_amyloid",
            icd_code == "I63" ~ "stroke",
            icd_code == "E10" ~ "htn",
            icd_code == "E78" ~ "hyperlipid",
            icd_code == "E08" ~ "diabetes",
            icd_code == "I61" ~ "intravent_hemor",
            icd_code == "I46" ~ "code"
        )
    ) |> 
    arrange(fin, icd_code, icd_10_code) |> 
    distinct(fin, icd_code, .keep_all = TRUE) |> 
    select(-icd_code, -source_string) |>  
    pivot_wider(names_from = icd_group, values_from = icd_10_code, names_prefix = "pmh_") |> 
    select(fin, intravent_hemor = pmh_intravent_hemor, everything(), -pmh_code)

# df_ich <- raw_diagnosis |> 
#     filter(str_detect(icd_10_code, "I61"))

# x <- distinct(df_home_meds, drug_cat)

cat_bp <- "ACE|angiotensin|beta blockers|calcium channel blocking|antihypertensive|thiazide|vasodilators"

df_home_bp_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Home Med",
        str_detect(drug_cat, cat_bp)
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "antihtn")

df_home_antiplt_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Home Med",
        drug_cat == "platelet aggregation inhibitors"
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "antiplt")

df_home_anticoag_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Home Med",
        drug_cat %in% c(
            "coumarins and indanediones", 
            "factor Xa inhibitors", 
            "thrombin inhibitors"
        )
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "anticoag")

df_home_statin_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Home Med",
        drug_cat == "HMG-CoA reductase inhibitors (statins)"
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "statin")

data_home_meds <- df_home_bp_meds |> 
    bind_rows(df_home_anticoag_meds, df_home_antiplt_meds, df_home_statin_meds) |> 
    arrange(fin, med_cat, medication) 

df_home_meds <- data_home_meds |> 
    distinct(fin, med_cat) |> 
    mutate(med = TRUE) |> 
    pivot_wider(names_from = med_cat, values_from = med, names_prefix = "home_") 

df_dc_bp_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Disch Rx",
        str_detect(drug_cat, cat_bp)
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "antihtn")

df_dc_antiplt_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Disch Rx",
        drug_cat == "platelet aggregation inhibitors"
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "antiplt")

df_dc_anticoag_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Disch Rx",
        drug_cat %in% c(
            "coumarins and indanediones", 
            "factor Xa inhibitors", 
            "thrombin inhibitors"
        )
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "anticoag")

df_dc_statin_meds <- raw_outpt_meds |> 
    filter(
        med_type == "Disch Rx",
        drug_cat == "HMG-CoA reductase inhibitors (statins)"
    ) |> 
    distinct(fin, medication, med_type) |> 
    mutate(med_cat = "statin")

data_dc_meds <- df_dc_bp_meds |> 
    bind_rows(df_dc_anticoag_meds, df_dc_antiplt_meds, df_dc_statin_meds) |> 
    arrange(fin, med_cat, medication) 

df_dc_meds <- data_dc_meds |> 
    distinct(fin, med_cat) |> 
    mutate(med = TRUE) |> 
    pivot_wider(names_from = med_cat, values_from = med, names_prefix = "dc_") 

df_labs_admit <- raw_labs |> 
    filter(lab_timing == "ADMIT") |> 
    select(-lab_timing) |> 
    pivot_wider(names_from = event, values_from = lab_result) |> 
    mutate(across(`LDL Direct`, ~coalesce(., `LDL (Calculated)`))) |> 
    select(
        fin, 
        hdl_admit = HDL, 
        ldl_admit = `LDL Direct`, 
        gcs_admit = `Glasgow Coma Score`, 
        nihss_admit = `NIH Stroke Score`
    )

df_labs_24h <- raw_labs |> 
    filter(lab_timing == "24_HR") |> 
    select(-lab_timing) |> 
    pivot_wider(names_from = event, values_from = lab_result) |> 
    select(fin, gcs_24h = `Glasgow Coma Score`, nihss_24h = `NIH Stroke Score`)

df_labs_disch <- raw_labs |> 
    filter(lab_timing == "DISCH") |> 
    select(fin, gcs_disch = lab_result) 
    
data_bp_meds <- raw_meds_bp |> 
    filter(medication != "mannitol") |> 
    mutate(
        route = case_when(
            str_detect(admin_route, "IV") ~ "iv",
            admin_route %in% c("PO", "GT", "NJ", "NG", "DHT", "OGT", "T FEED", "PEG") ~ "po",
            TRUE ~ "other"
        )
    ) |> 
    distinct(fin, medication, route) |> 
    arrange(fin, medication) 

df_bp_meds_24h <- data_bp_meds |> 
    mutate(med_grp = if_else(route == "po", "oral_antihtn", medication)) |> 
    distinct(fin, med_grp) |> 
    mutate(dose = TRUE) |> 
    filter(!med_grp %in% c("furosemide", "metoprolol", "clonidine", "diltiazem", "bumetanide", "verapamil")) |> 
    pivot_wider(names_from = med_grp, values_from = dose, names_prefix = "first_24h_meds_") 

# y <- distinct(raw_meds_bp, admin_route)

# df_sbp_first <- raw_sbp |> 
#     arrange(fin, event_datetime, event_id) |> 
#     distinct(fin, .keep_all = TRUE) |> 
#     select(fin, sbp_arrive = result_val)

df_sbp <- raw_sbp |> 
    inner_join(df_include, by = "fin")

data_sbp_daily <- df_sbp |> 
    mutate(
        admit_bp_days = difftime(event_datetime, admit_datetime, units = "days"),
        across(admit_bp_days, as.numeric),
        hosp_day = trunc(admit_bp_days)
    ) |> 
    group_by(fin, hosp_day) |> 
    summarize(across(result_val, list(min = min, median = median), na.rm = TRUE, .names = "sbp_{.fn}"), .groups = "drop") |> 
    filter(hosp_day >= -1) |> 
    arrange(fin, hosp_day) 

df_sbp_72h <- df_sbp |> 
    arrange(fin, event_datetime, event) |> 
    filter(
        event_datetime >= start_datetime,
        event_datetime <= start_datetime + hours(72)
    ) |> 
    distinct(fin, event_datetime, .keep_all = TRUE) |> 
    group_by(fin) |> 
    mutate(
        duration = difftime(lead(event_datetime), event_datetime, units = "hours"),
        across(duration, as.numeric),
        across(duration, ~coalesce(., 1))
    ) 

df_sbp_110 <- df_sbp_72h |> 
    filter(result_val < 110) |> 
    summarize(
        sbp_110_num = n(),
        sbp_110_duration = sum(duration, na.rm = TRUE)
    )

df_sbp_90 <- df_sbp_72h |> 
    filter(result_val < 90) |> 
    summarize(
        sbp_90_num = n(),
        sbp_90_duration = sum(duration, na.rm = TRUE)
    )

df_sbp_120 <- df_sbp_72h |> 
    filter(result_val < 120) |> 
    summarize(
        sbp_120_num = n(),
        sbp_120_duration = sum(duration, na.rm = TRUE)
    )

df_sbp_chg <- df_sbp |> 
    group_by(fin) |> 
    arrange(fin, event_datetime, event) |> 
    filter(
        event_datetime >= start_datetime,
        event_datetime <= start_datetime + hours(30)
    ) |> 
    mutate(
        sbp_first = first(result_val),
        sbp_chg = result_val - sbp_first,
        sbp_chg_pct = sbp_chg / sbp_first,
        event_time_hrs = difftime(event_datetime, start_datetime, units = "hours"),
        across(event_time_hrs, as.numeric)
    )

df_sbp_chg_6h <- df_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 6)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_6h = sbp_chg, sbp_pct_6h = sbp_chg_pct)

df_sbp_chg_12h <- df_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 12)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_12h = sbp_chg, sbp_pct_12h = sbp_chg_pct)

df_sbp_chg_18h <- df_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 18)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_18h = sbp_chg, sbp_pct_18h = sbp_chg_pct)

df_sbp_chg_24h <- df_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 24)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_24h = sbp_chg, sbp_pct_24h = sbp_chg_pct)

# df_sbp_110_duration <- df_sbp_72h |> 
#     group_by(fin) |> 
#     mutate(
#         duration = difftime(lead(event_datetime), event_datetime, units = "hours"),
#         across(duration, as.numeric),
#         across(duration, ~coalesce(., 1)),
#         sbp_110 = result_val < 110,
#         sbp_chg = sbp_110 != lag(sbp_110),
#         across(sbp_chg, ~coalesce(., TRUE)),
#         sbp_chg_num = cumsum(sbp_chg)
#     ) |> 
#     group_by(fin, sbp_110, sbp_chg_num) |> 
#     summarize(across(duration, sum, na.rm = TRUE)) |> 
#     filter(sbp_110)

df_glucoses_num <- raw_glucoses |> 
    count(fin, name = "glucoses_num")

df_glucoses_low <- raw_glucoses |> 
    mutate(
        across(result_val, str_replace_all, pattern = ">|<", replacement = ""),
        across(result_val, as.numeric)
    ) |> 
    filter(result_val < 60) |> 
    count(fin, name = "glucoses_low_num")

df_glucoses_high <- raw_glucoses |> 
    mutate(
        across(result_val, str_replace_all, pattern = ">|<", replacement = ""),
        across(result_val, as.numeric)
    ) |> 
    filter(result_val > 180) |> 
    count(fin, name = "glucoses_high_num")

df_gluc_durations <- raw_glucoses |> 
    group_by(fin) |> 
    arrange(fin, event_datetime) |> 
    mutate(
        across(result_val, str_replace_all, pattern = ">|<", replacement = ""),
        across(result_val, as.numeric),
        duration = difftime(lead(event_datetime), event_datetime, units = "hours"),
        across(duration, as.numeric),
        across(duration, ~coalesce(., 1))
    )

df_gluc_time <- df_gluc_durations |> 
    summarize(across(duration, sum, na.rm = TRUE))

df_gluc_60 <- df_gluc_durations |> 
    filter(result_val < 60) |> 
    summarize(across(duration, sum, na.rm = TRUE)) |> 
    rename(glucoses_low_hrs = duration) |> 
    left_join(df_gluc_time, by = "fin") |> 
    mutate(glucoses_low_pct_time = glucoses_low_hrs / duration * 100) |> 
    select(-duration)

df_gluc_180 <- df_gluc_durations |> 
    filter(result_val > 180) |> 
    summarize(across(duration, sum, na.rm = TRUE)) |> 
    rename(glucoses_high_hrs = duration) |> 
    left_join(df_gluc_time, by = "fin") |> 
    mutate(glucoses_high_pct_time = glucoses_high_hrs / duration * 100) |> 
    select(-duration)

df_gluc_median <- raw_glucoses |> 
    inner_join(df_include, by = "fin") |> 
    mutate(
        across(result_val, str_replace_all, pattern = ">|<", replacement = ""),
        across(result_val, as.numeric),
        across(event, ~"glucose"),
        hosp_day = difftime(event_datetime, admit_datetime, units = "days"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor),
        across(hosp_day, ~if_else(. < 0, 0, .))
    ) |> 
    calc_runtime(hosp_day, .id = fin) |> 
    summarize_data(hosp_day, .id = fin, .result = result_val) |> 
    select(fin, hosp_day, median_result) |> 
    pivot_wider(names_from = hosp_day, names_prefix = "median_glucose_day_", values_from = median_result)

data_vasop <- raw_meds_vasop |> 
    arrange(fin, event_datetime, medication) |> 
    filter(!is.na(iv_event)) |> 
    drip_runtime(.id = fin, .dt_tm = event_datetime, .rate = infusion_rate, .rate_unit = infusion_unit) |> 
    filter(!is.na(infusion_rate)) |> 
    summarize_drips(.id = fin, .rate = infusion_rate) |> 
    select(fin, medication, start_datetime, duration) |> 
    arrange(fin, start_datetime) 

# y <- distinct(raw_surgeries, surgery)

data_surgeries <- raw_surgeries |> 
    filter(str_detect(surgery, "Crani|Shunt")) |> 
    select(fin, surgery_start_datetime, surgery) |> 
    arrange(fin, surgery_start_datetime) 

df_icp <- raw_icp |> 
    inner_join(df_include, by = "fin")

data_codes <- raw_codes |> 
    select(fin, code_datetime = result_datetime) |> 
    arrange(fin, code_datetime) 

df_readmits <- raw_readmits |> 
    arrange(fin, readmit_datetime) |> 
    distinct(fin, .keep_all = TRUE)

data_locations <- raw_locations |> 
    semi_join(df_include, by = "fin") |> 
    arrange(fin, unit_count) 

# df_arrive <- raw_pts |> 
#     select(fin, tmc_arrive_datetime, tfr_arrive_datetime, first_arrive_datetime)

# df_sbp_first <- raw_pts |> 
#     select(fin, sbp_first_datetime, sbp_first)

# df_sbp_first <- raw_sbp |> 
#     arrange(fin, event_datetime, event_id) |> 
#     distinct(fin, .keep_all = TRUE) |> 
#     select(fin, sbp_first_datetime = event_datetime, sbp_first = result_val)

data_patients <- raw_pts |> 
    select(-starts_with("excl")) |> 
    # left_join(df_arrive, by = "fin") |> 
    semi_join(df_include, by = "fin") |> 
    left_join(df_imaging, by = "fin") |> 
    left_join(df_diagnosis, by = "fin") |> 
    left_join(df_home_meds, by = "fin") |> 
    left_join(df_dc_meds, by = "fin") |> 
    left_join(df_labs_admit, by = "fin") |> 
    left_join(df_labs_24h, by = "fin") |> 
    left_join(df_labs_disch, by = "fin") |> 
    # left_join(df_sbp_first, by = "fin") |> 
    mutate(
        first_sbp_220 = first_sbp >= 220,
        first_sbp_150_219 = first_sbp >= 150 & first_sbp < 220,
        first_sbp_lt_150 = first_sbp < 150
    ) |> 
    left_join(df_sbp_chg_6h, by = "fin") |> 
    left_join(df_sbp_chg_12h, by = "fin") |> 
    left_join(df_sbp_chg_18h, by = "fin") |> 
    left_join(df_sbp_chg_24h, by = "fin") |> 
    left_join(df_bp_meds_24h, by = "fin") |> 
    left_join(df_sbp_90, by = "fin") |> 
    left_join(df_sbp_110, by = "fin") |> 
    left_join(df_sbp_120, by = "fin") |> 
    # left_join(df_glucoses_num, by = "fin") |> 
    left_join(df_glucoses_low, by = "fin") |> 
    left_join(df_gluc_60, by = "fin") |> 
    left_join(df_glucoses_high, by = "fin") |> 
    left_join(df_gluc_180, by = "fin") |> 
    left_join(df_gluc_median, by = "fin") |> 
    left_join(df_readmits, by = "fin") |> 
    arrange(fin) |> 
    ungroup() |> 
    select(-encntr_id, -tfr_encntr_id)

l <- list(
    "patients" = data_patients,
    "daily_sbp" = data_sbp_daily,
    "bp_meds_24h" = data_bp_meds,
    "home_meds" = data_home_meds,
    "disch_meds" = data_dc_meds,
    "codes" = data_codes,
    "surgeries" = data_surgeries,
    "vasopressors" = data_vasop,
    "nurse_units" = data_locations
)

write.xlsx(l, paste0(f, "final/ich_data.xlsx"), overwrite = TRUE)


# extra patients ----------------------------------------------------------

extra_pts <- read_csv(paste0(f, "raw/patients_extras.csv")) |>
    rename_all(str_to_lower) |> 
    group_by(encntr_id) |> 
    mutate(
        across(fin, as.character),
        start_datetime = if_else(
            tfr_facility %in% rehabs, 
            arrive_datetime - hours(12),
            min(arrive_datetime, tfr_arrive_datetime, na.rm = TRUE), 
            arrive_datetime
        )
    )

# x <- semi_join(raw_pts, extras_pts, by = "fin")

extra_include <- extra_pts |>
    group_by(encntr_id) |>
    mutate(exclude = sum(excl_pregnant, excl_transfer, excl_early_death, na.rm = TRUE)) |>
    # filter(exclude == 0, first_sbp >= 150) |> 
    ungroup() |> 
    select(fin, encntr_id, admit_datetime, start_datetime)

# y <- semi_join(raw_pts, extras_include, by = "fin")

extra_sbp <- read_csv(paste0(f, "raw/sbp_extras.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    arrange(fin, event_datetime, event_id) |> 
    mutate(across(fin, as.character)) |> 
    inner_join(extras_include, by = "fin") 

extra_sbp_chg <- extra_sbp |> 
    group_by(fin) |> 
    arrange(fin, event_datetime, event) |> 
    filter(
        event_datetime >= start_datetime,
        event_datetime <= start_datetime + hours(30)
    ) |> 
    mutate(
        sbp_first = first(result_val),
        sbp_chg = result_val - sbp_first,
        sbp_chg_pct = sbp_chg / sbp_first,
        event_time_hrs = difftime(event_datetime, start_datetime, units = "hours"),
        across(event_time_hrs, as.numeric)
    )

extra_sbp_chg_6h <- extra_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 6)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_6h = sbp_chg, sbp_pct_6h = sbp_chg_pct)

extra_sbp_chg_12h <- extra_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 12)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_12h = sbp_chg, sbp_pct_12h = sbp_chg_pct)

extra_sbp_chg_18h <- extra_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 18)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_18h = sbp_chg, sbp_pct_18h = sbp_chg_pct)

extra_sbp_chg_24h <- extra_sbp_chg |> 
    mutate(time_diff = abs(event_time_hrs - 24)) |> 
    arrange(fin, time_diff) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_chg_24h = sbp_chg, sbp_pct_24h = sbp_chg_pct)

data_extras <- extra_pts |> 
    left_join(extra_sbp_chg_6h, by = "fin") |> 
    left_join(extra_sbp_chg_12h, by = "fin") |> 
    left_join(extra_sbp_chg_18h, by = "fin") |> 
    left_join(extra_sbp_chg_24h, by = "fin") |> 
    arrange(fin) |> 
    ungroup() |> 
    select(-encntr_id, -tfr_encntr_id)

write.xlsx(data_extras, paste0(f, "final/extras_data.xlsx"), overwrite = TRUE)


# figures -----------------------------------------------------------------

fig_pts <- read_excel(paste0(f, "raw/figs_data.xlsx"), sheet = 2) |> 
    select(fin = FIN, first_sbp_presenting) |> 
    mutate(
        across(fin, as.character),
        group = if_else(first_sbp_presenting >= 220, "gte_220", "150-219"),
        across(group, factor)
    )

df_pts_fig <- raw_pts |> 
    inner_join(fig_pts, by = "fin") |> 
    select(fin, group, start_datetime, first_sbp) 

df_sbp_fig <- raw_sbp |> 
    inner_join(df_pts_fig, by = "fin") |> 
    ungroup() |> 
    mutate(
        across(start_datetime, force_tz, tzone = "US/Central"),
        arrive_event_hrs = difftime(event_datetime, start_datetime, units = "hours"),
        across(arrive_event_hrs, as.numeric)
    ) |> 
    filter(
        arrive_event_hrs > -0.5,
        arrive_event_hrs <= 48
    )

g_fig <- df_sbp_fig |>
    ggplot(aes(x = arrive_event_hrs, y = result_val, linetype = group)) +
    # geom_point(alpha = 0.5, shape = 1) +
    geom_smooth(color = "black") +
    ggtitle("Systolic blood pressure over the first 48 hours") +
    scale_x_continuous("Hours from presentation", breaks = seq(0, 48, 12)) +
    ylab("Systolic blood pressure (mmHg)") +
    scale_linetype_manual("Initial SBP (mmHg)", values = c("dotted", "solid"), labels = c("150-219", ">/= 220")) +
    coord_cartesian(ylim = c(100, 250)) +
    theme_bg_print() +
    theme(legend.position = "top")

g_fig

ggsave(paste0(f, "figs/sbp_graph.jpg"), device = "jpeg", width = 6, height = 4, units = "in")

x <- ggplot_build(g_fig)
df_x <- x$data[[1]]

df_fig <- df_x |>
    select(x, y, group) |>
    pivot_wider(names_from = group, values_from = y)

write.xlsx(df_fig, paste0(f, "final/data_sbp_graph.xlsx"), overwrite = TRUE)

df_sbp_change <- df_pts_fig |>
    rename(sbp_arrive = first_sbp) |> 
    left_join(df_sbp_chg_6h, by = "fin") |>
    left_join(df_sbp_chg_12h, by = "fin") |>
    left_join(df_sbp_chg_18h, by = "fin") |>
    left_join(df_sbp_chg_24h, by = "fin")

df_fig2 <- df_sbp_change |>
    group_by(fin) |>
    # left_join(pt_groups, by = "fin") |>
    select(fin, group, starts_with("sbp_pct")) |>
    pivot_longer(cols=starts_with("sbp_pct")) |>
    mutate(
        across(name, str_replace_all, pattern = "sbp_pct_", replacement = ""),
        across(name, factor, labels = c("6", "12", "18", "24")),
        across(value, ~.*100)
    )

df_fig2 |>
    ggplot(aes(x = name, y = value, color = group)) +
    geom_boxplot() +
    ggtitle("Change in systolic blood pressure over the first 24 hours") +
    xlab("Hours from presentation") +
    ylab("Change in systolic blood pressure (%)") +
    scale_color_brewer("Initial SBP (mmHg)", palette = "Set1", labels = c("150-219", ">/= 220")) +
    theme_bg()

ggsave(paste0(f, "figs/boxplot.jpg"), device = "jpeg", width = 6, height = 4, units = "in")

df_fig2_xl <- df_sbp_change |>
    group_by(fin) |>
    select(fin, group, starts_with("sbp_pct")) |>
    pivot_longer(cols=starts_with("sbp_pct")) |>
    mutate(
        across(name, str_replace_all, pattern = "sbp_pct_", replacement = ""),
        # across(name, str_replace_all, pattern = "h", replacement = ""),
        across(name, factor, labels = c("6", "12", "18", "24")),
        across(value, ~.*100)
    ) |>
    arrange(name, group) |> 
    filter(!is.na(value))

write.xlsx(df_fig2_xl, paste0(f, "final/data_boxplot.xlsx"), overwrite = TRUE)


p1 <- wilcox.test(value ~ group, df_fig2, subset = name == 6) |> glance()
p2 <- wilcox.test(value ~ group, df_fig2, subset = name == 12) |> glance()
p3 <- wilcox.test(value ~ group, df_fig2, subset = name == 18) |> glance()
p4 <- wilcox.test(value ~ group, df_fig2, subset = name == 24) |> glance()

df_p <- bind_rows(p1, p2, p3, p4)

df_fig2_meds <- df_fig2 |> 
    group_by(group, name) |> 
    summarize(across(value, median, na.rm = TRUE)) |> 
    pivot_wider(names_from = group, values_from = value, names_prefix = "median_") |> 
    rename(hour = name) |> 
    bind_cols(df_p) |> 
    mutate(across(p.value, round, digits = 4))

write.xlsx(df_fig2_meds, paste0(f, "final/boxplot_summary.xlsx"), overwrite = TRUE)

