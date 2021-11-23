library(tidyverse)
# library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)

f <- "/Volumes/brgulbis/Data/stroke_ich/ich_sarah/"
# f <- "data/ich_sarah/"

tz <- locale(tz = "US/Central")

# raw_pts <- read_excel(paste0(f, "raw/patient_list.xlsx")) |> 
#     rename_all(str_to_lower)

raw_pts <- read_csv(paste0(f, "raw/patient_list.csv")) |> 
    rename_all(str_to_lower)

df_include <- raw_pts |> 
    filter(
        (is.na(pregnant) | pregnant != "Positive"),
        ((str_detect(admit_src, regex("tfr|transfer", ignore_case = TRUE)) & !is.na(tfr_facility)) |
             !str_detect(admit_src, regex("tfr|transfer", ignore_case = TRUE))),
        (!(los < 2 & str_detect(disch_disposition, "Donor|Deceased"))),
        min_sbp > 100
    )

mbo_fin <- concat_encounters(df_include$fin, 950)
print(mbo_fin)

raw_demog <- read_csv(paste0(f, "raw/ich_demog.csv"), locale = tz) |> 
    rename_all(str_to_lower)

raw_codes <- read_csv(paste0(f, "raw/ich_codes.csv"), locale = tz) |> 
    rename_all(str_to_lower)

raw_diagnosis <- read_csv(paste0(f, "raw/ich_diagnosis.csv")) |> 
    rename_all(str_to_lower)

raw_glucoses <- read_csv(paste0(f, "raw/ich_glucoses.csv"), locale = tz) |> 
    rename_all(str_to_lower)

raw_labs <- read_csv(paste0(f, "raw/ich_labs.csv")) |> 
    rename_all(str_to_lower)

raw_locations <- read_csv(paste0(f, "raw/ich_locations.csv")) |> 
    rename_all(str_to_lower) |> 
    arrange(fin, unit_count)

raw_meds_bp <- read_csv(paste0(f, "raw/ich_meds_bp.csv"), locale = tz) |> 
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_meds_vasop <- read_csv(paste0(f, "raw/ich_meds_vasop.csv"), locale = tz) |> 
    rename_all(str_to_lower)

raw_outpt_meds <- read_csv(paste0(f, "raw/ich_outpt_meds.csv")) |> 
    rename_all(str_to_lower)

raw_sbp <- read_csv(paste0(f, "raw/ich_sbp.csv"), locale = tz) |> 
    rename_all(str_to_lower)

raw_surgeries <- read_csv(paste0(f, "raw/ich_surgeries.csv"), locale = tz) |> 
    rename_all(str_to_lower)

raw_icp <- read_csv(paste0(f, "raw/ich_icp.csv"), locale = tz) |> 
    rename_all(str_to_lower)

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
    distinct(fin, medication, route) 

df_bp_meds_24h <- data_bp_meds |> 
    mutate(med_grp = if_else(route == "po", "oral_antihtn", medication)) |> 
    distinct(fin, med_grp) |> 
    mutate(dose = TRUE) |> 
    filter(!med_grp %in% c("furosemide", "metoprolol", "clonidine", "diltiazem", "bumetanide", "verapamil")) |> 
    pivot_wider(names_from = med_grp, values_from = dose, names_prefix = "first_24h_meds_")

# y <- distinct(raw_meds_bp, admin_route)

df_sbp_first <- raw_sbp |> 
    arrange(fin, event_datetime, event_id) |> 
    distinct(fin, .keep_all = TRUE) |> 
    select(fin, sbp_arrive = result_val)

df_sbp <- raw_sbp |> 
    inner_join(raw_demog[c("fin", "admit_datetime")], by = "fin")

data_sbp_daily <- df_sbp |> 
    mutate(
        admit_bp_days = difftime(event_datetime, admit_datetime, units = "days"),
        across(admit_bp_days, as.numeric),
        hosp_day = trunc(admit_bp_days)
    ) |> 
    group_by(fin, hosp_day) |> 
    summarize(across(result_val, list(min = min, median = median), na.rm = TRUE, .names = "sbp_{.fn}"))

df_sbp_72h <- df_sbp |> 
    arrange(fin, event_datetime, event) |> 
    filter(event_datetime <= admit_datetime + hours(72)) |> 
    distinct(fin, event_datetime, .keep_all = TRUE)

df_sbp_110 <- df_sbp_72h |> 
    filter(result_val < 110) |> 
    group_by(fin) |> 
    mutate(
        duration = difftime(lead(event_datetime), event_datetime, units = "hours"),
        across(duration, as.numeric),
        across(duration, ~coalesce(., 1))
    ) |> 
    summarize(
        sbp_110_num = n(),
        sbp_110_duration = sum(duration, na.rm = TRUE)
    )

df_sbp_90 <- df_sbp_72h |> 
    filter(result_val < 90) |> 
    group_by(fin) |> 
    mutate(
        duration = difftime(lead(event_datetime), event_datetime, units = "hours"),
        across(duration, as.numeric),
        across(duration, ~coalesce(., 1))
    ) |> 
    summarize(
        sbp_90_num = n(),
        sbp_90_duration = sum(duration, na.rm = TRUE)
    )

df_sbp_120 <- df_sbp_72h |> 
    filter(result_val < 120) |> 
    group_by(fin) |> 
    mutate(
        duration = difftime(lead(event_datetime), event_datetime, units = "hours"),
        across(duration, as.numeric),
        across(duration, ~coalesce(., 1))
    ) |> 
    summarize(
        sbp_120_num = n(),
        sbp_120_duration = sum(duration, na.rm = TRUE)
    )

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

data_vasop <- raw_meds_vasop |> 
    arrange(fin, event_datetime, medication) |> 
    rename(
        med_datetime = event_datetime,
        rate = infusion_rate,
        rate_unit = infusion_unit
    ) |> 
    drip_runtime(vars(fin)) |> 
    summarize_drips(vars(fin)) |> 
    select(fin, medication, start_datetime, duration)

y <- distinct(raw_surgeries, surgery)

data_surgeries <- raw_surgeries |> 
    filter(str_detect(surgery, "Crani|Shunt")) |> 
    select(fin, surgery_start_datetime, surgery)

df_icp <- raw_icp |> 
    inner_join(raw_demog[c("fin", "admit_datetime")], by = "fin")

data_codes <- raw_codes |> 
    select(fin, code_datetime = result_datetime)

data_patients <- raw_demog |> 
    left_join(df_diagnosis, by = "fin") |> 
    left_join(df_home_meds, by = "fin") |> 
    left_join(df_dc_meds, by = "fin") |> 
    left_join(df_labs_admit, by = "fin") |> 
    left_join(df_labs_24h, by = "fin") |> 
    left_join(df_labs_disch, by = "fin") |> 
    left_join(df_sbp_first, by = "fin") |> 
    left_join(df_bp_meds_24h, by = "fin") |> 
    left_join(df_sbp_90, by = "fin") |> 
    left_join(df_sbp_110, by = "fin") |> 
    left_join(df_sbp_120, by = "fin")

l <- list(
    "patients" = data_patients,
    "daily_sbp" = data_sbp_daily,
    "bp_meds_24h" = data_bp_meds,
    "home_meds" = data_home_meds,
    "disch_meds" = data_dc_meds,
    "codes" = data_codes,
    "surgeries" = data_surgeries,
    "vasopressors" = data_vasop,
    "nurse_units" = raw_locations
)

write.xlsx(l, paste0(f, "final/ich_data.xlsx"), overwrite = TRUE)
