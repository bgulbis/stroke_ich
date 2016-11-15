library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(MESS)

xls <- "data/raw/BP Datasheet_10_27_Final.xls"

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
# fix row 86, Total number of antihypertensive medications at discharge
# convert to number, tPA dosing, rows 1, 4, 21, 33
# rename "Blood Pressure Readings " tab to remove spaces (note trailing space)
# convert to number, rows 5003 to 7924
# fixed 8 errors with date/times; see fixes.Rds

# set column types
cols <- c("numeric", rep("text", 2), rep("numeric", 2), rep("date", 2),
          "numeric", rep("text", 3), rep("numeric", 2), rep("text", 2),
          "numeric", "date", rep("text", 38), "numeric", rep("date", 3),
          rep("text", 4), rep("numeric", 2), rep("text", 22), rep("numeric", 7))

main <- read_excel(xls, sheet = "Mainsheet", col_types = cols, na = "N/A") %>%
    rename(patient = `Patient Number`,
           gender = `Gender(M/F)`,
           bmi = `BMI (kg/m2)`) %>%
    filter(!is.na(patient))

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
    dmap_at("med", str_to_lower) %>%
    dmap_at("med", str_trim, side = "both") %>%
    dmap_at("med", str_replace_all, pattern = "no anti-htn.*", "none") %>%
    select(-`Scheduled/PRN`) %>%
    filter(!is.na(patient))

data_pmh <- main %>%
    select(patient, starts_with("PMH - ")) %>%
    dmap_if(is.character, ~ .x == "Yes")

names(data_pmh) <- str_to_lower(str_replace_all(names(data_pmh), "PMH - ", ""))
names(data_pmh) <- str_to_lower(str_replace_all(names(data_pmh), " |/", "_"))

data_bp <- bp %>%
    group_by(patient) %>%
    arrange(bp_datetime) %>%
    summarize(first_sbp = first(SBP),
              first_dbp = first(DBP),
              last_sbp = last(SBP),
              last_dbp = last(DBP),
              sbp_admit_dc_diff = last(SBP) - first(SBP),
              dbp_admit_dc_diff = last(DBP) - first(DBP),
              first_sbp_180 = first(SBP) >= 180)

data_meds <- meds %>%
    distinct(patient, med, scheduled) %>%
    group_by(patient, scheduled)

data_nicard <- data_meds %>%
    filter(med == "nicardipine") %>%
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
    left_join(data_pmh[c("patient", "hypertension")], by = "patient") %>%
    distinct(patient, med, hypertension) %>%
    group_by(hypertension) %>%
    count(med) %>%
    arrange(hypertension, desc(n), med) %>%
    ungroup() %>%
    mutate(hypertension = if_else(hypertension, "htn", "no_htn")) %>%
    spread(hypertension, n) %>%
    dmap_if(is.integer, ~ coalesce(.x, 0L))

hm <- main %>%
    select(patient, starts_with("HM - ")) %>%
    dmap_if(is.character, ~ .x == "Yes")

names(hm) <- str_to_lower(str_replace_all(names(hm), "HM - ", ""))

data_home_meds <- hm %>%
    gather(med, value, -patient) %>%
    filter(value == TRUE,
           med != "unknown") %>%
    arrange(patient, med) %>%
    dmap_at("value", ~ TRUE) %>%
    dmap_at("med", str_replace_all, pattern = "hctz", "hydrochlorothiazide")

data_meds_num_home <- data_home_meds %>%
    group_by(patient) %>%
    count() %>%
    rename(num_meds_home = n)

data_meds_hm_inpt <- data_meds %>%
    filter(med != "none") %>%
    dmap_at("med", str_replace_all, pattern = " tartrate| succinate", replacement = "") %>%
    full_join(data_home_meds, by = c("patient", "med")) %>%
    arrange(patient, med) %>%
    mutate(same_med = !is.na(scheduled) & !is.na(value)) %>%
    group_by(patient) %>%
    summarize(num_meds = n(),
              num_inpt = sum(scheduled, na.rm = TRUE),
              num_hm = sum(value, na.rm = TRUE),
              prcnt_hm_cont = if_else(num_hm > 0, sum(same_med, na.rm = TRUE) / num_hm, NA_real_))

dm <- main %>%
    select(patient, starts_with("DM - ")) %>%
    dmap_if(is.character, ~ .x == "Yes")

names(dm) <- str_to_lower(str_replace_all(names(dm), "DM - ", ""))

data_dc_meds <- dm %>%
    gather(med, value, -patient) %>%
    filter(value == TRUE,
           med != "unknown") %>%
    arrange(patient, med) %>%
    dmap_at("value", ~ TRUE) %>%
    dmap_at("med", str_replace_all, pattern = "hctz", "hydrochlorothiazide")

data_meds_num_dc <- data_dc_meds %>%
    group_by(patient) %>%
    count() %>%
    rename(num_meds_dc = n)

data_meds_count <- data_meds_num_home %>%
    left_join(data_meds_num_dc, by = "patient") %>%
    dmap_if(is.integer, ~ coalesce(.x, 0L)) %>%
    mutate(diff_home_dc = num_meds_dc - num_meds_home)

data_meds_dc <- data_dc_meds %>%
    group_by(med) %>%
    summarize(num = n())

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

fill_zero <- c(names(data_meds_num[-1]), "same_hm_inpt")

options(scipen = 999)
data_tidy <- main %>%
    mutate(length_stay = as.numeric(difftime(`Discharge Date/Time`, `Admission Date/Time`, units = "days")),
           time_admit_tpa = as.numeric(difftime(`tPA administration date/time`, `Admission Date/Time`, units = "hours")),
           time_admit_ivhtn = as.numeric(difftime(`First IV Anti-HTN Medication Date/Time`, `Admission Date/Time`, units = "hours")),
           time_admit_pohtn = as.numeric(difftime(`First oral Anti-HTN Medication Date/Time`, `Admission Date/Time`, units = "hours")),
           time_ivhtn_pohtn = as.numeric(difftime(`First oral Anti-HTN Medication Date/Time`, `First IV Anti-HTN Medication Date/Time`, units = "hours")),
           time_admit_prn = as.numeric(difftime(`First prn Anti-HTN Medication Date/Time`, `Admission Date/Time`, units = "hours"))) %>%
    by_row(~ min(.x$time_admit_ivhtn, .x$time_admit_pohtn, .x$time_admit_prn, na.rm = TRUE), .collate = "rows", .to = "first_bpmed") %>%
    dmap_at("first_bpmed", na_if, y = Inf) %>%
    select(patient:bmi,
           length_stay,
           Disposition,
           transfer = `Transferred from another hospital`,
           transfer_nicardipine = `If yes, nicardipine given?`,
           admit_nihss = `Admission National Institute of Health Stroke Scale`,
           admit_gcs = `Admission Glasgow Coma Scale`,
           dc_gcs = `Discharge: Glasgow Coma Scale`,
           dc_rankin = `Discharge:  Modified rankin scale score `,
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
           fall = `Safety - Fall `,
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
    dmap_at(convert_logi, ~ .x == "Yes") %>%
    dmap_at(fill_zero, ~ coalesce(.x, 0L)) %>%
    dmap_at("nicard_gtt", ~ coalesce(.x, FALSE))

names(data_tidy) <- str_to_lower(names(data_tidy))

write_csv(data_tidy, "data/tidy/main_analysis.csv")
write_csv(data_meds_common, "data/tidy/common_meds.csv")
write_csv(data_meds_dc, "data/tidy/meds_discharge.csv")
