library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(MESS)

xls <- "data/raw/bp_data_pre-post.xlsx"

# changes to Excel data sheet --------------------------
# rename Admission BP >/= 180 to Admission BP gte 180
# remove linebreak from:
# - "Admission National Institute of Health Stroke Scale"
# - "Total number of antihypertensives at home "
# - "First IV Anti-HTN Medication Date/Time"
# - "First oral Anti-HTN Medication Date/Time"
# - "First prn Anti-HTN Medication Date/Time"
# - "Safety - New stroke during hospitalization?"
# - "Safety - Decreased mental status attributed to low blood pressure"
# fix row 33, row 36; tPA administration date/time - convert unknown to N/A
# fix row 56, Total number of antihypertensives at home - convert to number
# fix Q75, date formating
# fix row 86, Total number of antihypertensive medications at discharge
# convert to number, tPA dosing, rows 1, 4, 21, 33
# rename "Blood Pressure Readings " tab to remove spaces (note trailing space)
# convert to number, rows 5003 to 7924
# Medications: fix dates on F1664, F1665, F1896, F1907
# fixed 8 errors with date/times; see fixes.Rds
# x <- read_rds("data/external/radiographic_fixes.Rds")

# set column types
cols <- c("numeric", rep("text", 2), rep("numeric", 2), rep("date", 2),
          "numeric", rep("text", 3), rep("numeric", 2), rep("text", 2),
          "numeric", "date", rep("text", 38), "numeric", rep("date", 3),
          rep("text", 4), rep("numeric", 2), rep("text", 22), rep("numeric", 7))

main <- read_excel(xls, sheet = "Mainsheet", col_types = cols, na = "N/A") %>%
    rename(patient = `Patient Number`,
           gender = `Gender(M/F)`,
           bmi = `BMI (kg/m2)`) %>%
    filter(!is.na(patient)) %>%
    mutate(group = if_else(patient <= 130, "pre", "post"))

xray <- read_excel(xls, sheet = "Radiographic") %>%
    rename(patient = `Patient Number`) %>%
    filter(!is.na(patient))

bp <- read_excel(xls, sheet = "Blood_Pressure_Readings") %>%
    rename(patient = `Patient Number`,
           bp_datetime = `Date/Time`) %>%
    filter(!is.na(patient))

location <- read_excel(xls, sheet = "DailyLocationBPLabs_NEW") %>%
    rename(patient = `Patient number`) %>%
    filter(!is.na(patient))

meds <- read_excel(xls, sheet = "Medications") %>%
    rename(patient = `Patient Number`,
           med = `Antihypertensive Medication`,
           route = Route,
           dose = `Dose/Rate (mg or mg/hr)`,
           admin_datetime = `Administration Date/Time`) %>%
    mutate(scheduled = `Scheduled/PRN` == "Scheduled") %>%
    mutate_at("med", str_to_lower) %>%
    mutate_at("med", str_trim, side = "both") %>%
    mutate_at("med", str_replace_all, pattern = "no anti-htn.*", "none") %>%
    select(-`Scheduled/PRN`) %>%
    filter(!is.na(patient))

data_pmh <- main %>%
    select(patient, group, starts_with("PMH - ")) %>%
    mutate_at("group", funs(. == "pre")) %>%
    mutate_if(is.character, funs(. == "Yes")) %>%
    rename_all(str_replace_all, pattern = "PMH - ", replacement = "") %>%
    rename_all(str_replace_all, pattern = " |/", "_") %>%
    rename_all(str_to_lower) %>%
    mutate_at("group", funs(case_when(. ~ "pre",
                                      TRUE ~ "post")))

data_bp <- bp %>%
    rename_all(str_to_lower) %>%
    group_by(patient) %>%
    arrange(bp_datetime, .by_group = TRUE) %>%
    summarize_at(c("sbp", "dbp"), funs(first, last)) %>%
    group_by(patient) %>%
    mutate(sbp_admit_dc_diff = sbp_last - sbp_first,
           dbp_admit_dc_diff = dbp_last - dbp_first,
           sbp_first_180 = sbp_first >= 180)

data_meds <- meds %>%
    distinct(patient, med, scheduled) %>%
    group_by(patient, scheduled)

data_nicard <- data_meds %>%
    filter(med == "nicardipine") %>%
    rename(drip = med) %>%
    distinct() %>%
    mutate(nicard_gtt = TRUE) %>%
    ungroup() %>%
    select(-scheduled)

data_meds_num <- data_meds %>%
    summarize(num_meds = n()) %>%
    mutate(scheduled = if_else(scheduled, "num_scheduled", "num_prn", "num_unknown")) %>%
    spread(scheduled, num_meds) %>%
    mutate(num_meds = sum(num_scheduled, num_prn, num_unknown, na.rm = TRUE))

data_meds_common <- meds %>%
    left_join(data_pmh[c("patient", "group", "hypertension")], by = "patient") %>%
    distinct(patient, group, hypertension, med) %>%
    mutate(hypertension = if_else(hypertension, "htn", "no_htn")) %>%
    count(med, group, hypertension) %>%
    arrange(group, hypertension, desc(n), med) %>%
    unite(group_htn, group, hypertension) %>%
    spread(group_htn, n) %>%
    mutate_if(is.integer, funs(coalesce(., 0L)))

data_home_meds <- main %>%
    select(patient, starts_with("HM - ")) %>%
    mutate_if(is.character, funs(. == "Yes")) %>%
    rename_all(str_replace_all, pattern = "HM - ", replacement = "") %>%
    rename_all(str_to_lower) %>%
    gather(med, value, -patient) %>%
    filter(value == TRUE,
           med != "unknown") %>%
    arrange(patient, med) %>%
    mutate_at("med", str_replace_all, pattern = "hctz", replacement = "hydrochlorothiazide")

data_meds_num_home <- data_home_meds %>%
    group_by(patient) %>%
    count() %>%
    rename(num_meds_home = n)

data_meds_hm_inpt <- data_meds %>%
    filter(med != "none") %>%
    mutate_at("med", str_replace_all, pattern = " tartrate| succinate", replacement = "") %>%
    full_join(data_home_meds, by = c("patient", "med")) %>%
    arrange(patient, med) %>%
    mutate(same_med = !is.na(scheduled) & !is.na(value)) %>%
    group_by(patient) %>%
    summarize(num_meds = n(),
              num_inpt = sum(scheduled, na.rm = TRUE),
              num_hm = sum(value, na.rm = TRUE),
              prcnt_hm_cont = if_else(num_hm > 0, sum(same_med, na.rm = TRUE) / num_hm, NA_real_))

data_dc_meds <- main %>%
    select(patient, starts_with("DM - ")) %>%
    mutate_if(is.character, funs(. == "Yes")) %>%
    rename_all(str_replace_all, pattern = "DM - ", "") %>%
    rename_all(str_to_lower) %>%
    gather(med, value, -patient) %>%
    filter(value == TRUE,
           med != "unknown") %>%
    arrange(patient, med) %>%
    mutate_at("med", str_replace_all, pattern = "hctz", replacement = "hydrochlorothiazide")

data_meds_num_dc <- data_dc_meds %>%
    group_by(patient) %>%
    count() %>%
    rename(num_meds_dc = n)

data_meds_count <- data_meds_num_home %>%
    left_join(data_meds_num_dc, by = "patient") %>%
    mutate_if(is.integer, funs(coalesce(., 0L))) %>%
    mutate(diff_home_dc = num_meds_dc - num_meds_home)

data_meds_dc <- data_dc_meds %>%
    left_join(data_pmh[c("patient", "group", "hypertension")], by = "patient") %>%
    distinct(patient, group, hypertension, med) %>%
    mutate(hypertension = if_else(hypertension, "htn", "no_htn")) %>%
    count(med, group, hypertension) %>%
    arrange(group, hypertension, desc(n), med) %>%
    unite(group_htn, group, hypertension) %>%
    spread(group_htn, n) %>%
    mutate_if(is.integer, funs(coalesce(., 0L)))

bp_auc <- bp %>%
    group_by(patient) %>%
    arrange(patient, bp_datetime) %>%
    mutate(duration = as.numeric(difftime(bp_datetime, first(bp_datetime), units = "hours"))) %>%
    summarize(sbp_auc = auc(duration, SBP),
              dbp_auc = auc(duration, DBP),
              duration = as.numeric(difftime(last(bp_datetime), first(bp_datetime), units = "hours"))) %>%
    mutate(sbp_wt_avg = sbp_auc / duration,
           dbp_wt_avg = dbp_auc / duration)

bp_24h <- bp %>%
    group_by(patient) %>%
    arrange(patient, bp_datetime) %>%
    filter(bp_datetime <= first(bp_datetime) + hours(24)) %>%
    summarize(sbp_change_24h = last(SBP) - first(SBP),
              dbp_change_24h = last(DBP) - first(DBP))

bp_48h <- bp %>%
    group_by(patient) %>%
    arrange(patient, bp_datetime) %>%
    filter(bp_datetime <= first(bp_datetime) + hours(48)) %>%
    summarize(sbp_change_48h = last(SBP) - first(SBP),
              dbp_change_48h = last(DBP) - first(DBP))

med_first <- meds %>%
    group_by(patient) %>%
    arrange(patient, admin_datetime) %>%
    filter(admin_datetime == first(admin_datetime)) %>%
    select(patient, med) %>%
    distinct(.keep_all = TRUE)

data_meds_common_gt180 <- meds %>%
    left_join(data_bp[c("patient", "sbp_first_180")], by = "patient") %>%
    left_join(data_pmh[c("patient", "group")], by = "patient") %>%
    mutate_at("sbp_first_180", funs(if_else(., "gte180", "lt180"))) %>%
    distinct(patient, group, med, sbp_first_180) %>%
    count(group, sbp_first_180, med) %>%
    arrange(group, sbp_first_180, desc(n), med) %>%
    unite(group_sbp, group, sbp_first_180) %>%
    spread(group_sbp, n) %>%
    mutate_if(is.integer, funs(coalesce(., 0L)))

dc_bp_goal <- location %>%
    group_by(patient) %>%
    arrange(patient, Day) %>%
    summarize(dc_bp_goal = last(`SBP High`))

daily_goal <- location %>%
    group_by(patient) %>%
    mutate(days = n(),
           goal_recorded = !is.na(`SBP High`)) %>%
    summarize(days = max(days),
              goal_recorded = sum(goal_recorded, na.rm = TRUE)) %>%
    mutate(days_without_goal = days - goal_recorded)

goal_met <- location %>%
    group_by(patient) %>%
    arrange(patient, Day) %>%
    filter(`Was BP goal met?` == "Yes") %>%
    summarize(day_goal_met = first(Day))

convert_logi <- c("transfer",
                  "transfer_nicardipine",
                  "ecg_afib",
                  "tpa_resolve_symptoms",
                  "tpa_occlusion_persist",
                  "intraarterial_tx",
                  "stroke_new",
                  "fall",
                  "syncope",
                  "ams_hypotension")

# fill_zero <- c(names(data_meds_num[-1]), "same_hm_inpt")
fill_zero <- names(data_meds_num[-1])

options(scipen = 999)
data_tidy <- main %>%
    mutate(length_stay = as.numeric(difftime(`Discharge Date/Time`, `Admission Date/Time`, units = "days")),
           time_admit_tpa = as.numeric(difftime(`tPA administration date/time`, `Admission Date/Time`, units = "hours")),
           time_admit_ivhtn = as.numeric(difftime(`First IV Anti-HTN Medication Date/Time`, `Admission Date/Time`, units = "hours")),
           time_admit_pohtn = as.numeric(difftime(`First oral Anti-HTN Medication Date/Time`, `Admission Date/Time`, units = "hours")),
           time_ivhtn_pohtn = as.numeric(difftime(`First oral Anti-HTN Medication Date/Time`, `First IV Anti-HTN Medication Date/Time`, units = "hours")),
           time_admit_prn = as.numeric(difftime(`First prn Anti-HTN Medication Date/Time`, `Admission Date/Time`, units = "hours"))) %>%
    rowwise() %>%
    mutate(first_bpmed = min(time_admit_ivhtn, time_admit_pohtn, time_admit_prn, na.rm = TRUE)) %>%
    # purrrlyr::by_row(~ min(.x$time_admit_ivhtn, .x$time_admit_pohtn, .x$time_admit_prn, na.rm = TRUE), .collate = "rows", .to = "first_bpmed") %>%
    mutate_at("first_bpmed", na_if, y = Inf) %>%
    select(patient:bmi,
           length_stay,
           Disposition,
           transfer = `Transferred from another hospital`,
           transfer_nicardipine = `If yes, nicardipine given?`,
           admit_nihss = `Admission National Institute of Health Stroke Scale`,
           admit_gcs = `Admission Glasgow Coma Scale`,
           dc_gcs = `Discharge: Glasgow Coma Scale`,
           dc_rankin = `Discharge:  Modified rankin scale score`,
           stroke_type = `Type of Ischemic Stroke`,
           ecg_afib = `ECG showing afib at anytime?`,
           tpa = `tPA dosing`,
           time_admit_tpa,
           tpa_resolve_symptoms = `Resolution of symptoms from tPA`,
           tpa_occlusion_persist = `Persistence of occulsion from IV tPA`,
           intraarterial_tx = `Intraarterial therapy`,
           tici = `Thrombolysis in cerebral infarction score`,
           first_bpmed,
           time_admit_ivhtn,
           time_admit_pohtn,
           time_ivhtn_pohtn,
           time_admit_prn,
           stroke_new = `Safety - New stroke during hospitalization?`,
           fall = `Safety - Fall`,
           syncope = `Safety - Syncopal Event`,
           ams_hypotension = `Safety - Decreased mental status attributed to low blood pressure`) %>%
    left_join(data_pmh, by = "patient") %>%
    left_join(data_meds_num, by = "patient") %>%
    left_join(data_meds_hm_inpt[c("patient", "prcnt_hm_cont")], by = "patient") %>%
    left_join(data_meds_count, by = "patient") %>%
    left_join(data_bp, by = "patient") %>%
    left_join(data_nicard, by = "patient") %>%
    left_join(bp_auc, by = "patient") %>%
    left_join(bp_24h, by = "patient") %>%
    left_join(bp_48h, by = "patient") %>%
    left_join(med_first, by = "patient") %>%
    left_join(dc_bp_goal, by = "patient") %>%
    left_join(daily_goal[c("patient", "days_without_goal")], by = "patient") %>%
    left_join(goal_met, by = "patient") %>%
    mutate(goal_met = !is.na(day_goal_met),
           stringent_goal = dc_bp_goal < 180) %>%
    mutate_at(convert_logi, funs(. == "Yes")) %>%
    mutate_at(fill_zero, funs(coalesce(., 0L))) %>%
    mutate_at("nicard_gtt", funs(coalesce(., FALSE))) %>%
    rename_all(str_to_lower)

write_csv(data_tidy, "data/tidy/main-analysis_pre-post.csv")
write_csv(data_meds_common, "data/tidy/common-meds_pre-post.csv")
write_csv(data_meds_dc, "data/tidy/meds-discharge_pre-post.csv")
write_csv(data_meds_common_gt180, "data/tidy/common-meds_bp-gte-180_pre-post.csv")
