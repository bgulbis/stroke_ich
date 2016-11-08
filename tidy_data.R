library(tidyverse)
library(readxl)
library(stringr)

xls <- "BP Datasheet_10_27_Final.xls"

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

# set column types
cols <- c("numeric", rep("text", 2), rep("numeric", 2), rep("date", 2),
          "numeric", rep("text", 3), rep("numeric", 2), rep("text", 2),
          "numeric", "date", rep("text", 38), "numeric", rep("date", 3),
          rep("text", 4), rep("numeric", 2), rep("text", 22), rep("numeric", 7))

main <- read_excel(xls, sheet = "Mainsheet", col_types = cols, na = "N/A") %>%
    rename(patient = `Patient Number`,
           gender = `Gender(M/F)`,
           pmh_htn = `PMH - Hypertension`,
           bmi = `BMI (kg/m2)`) %>%
    filter(!is.na(patient))

# fixed 8 errors with date/times; see fixes.Rds
xray <- read_excel(xls, sheet = "Radiographic") %>%
    rename(patient = `Patient Number`) %>%
    filter(!is.na(patient))

# rename "Blood Pressure Readings " tab to remove spaces (note trailing space)
# convert to number, rows 5003 to 7924
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

data_tidy <- main %>%
    mutate(length_stay = as.numeric(difftime(`Discharge Date/Time`, `Admission Date/Time`, units = "hours")))

data_bp <- bp %>%
    group_by(patient) %>%
    arrange(bp_datetime) %>%
    summarize(first_sbp = first(SBP),
              first_dbp = first(DBP),
              last_sbp = last(SBP),
              last_dbp = last(DBP))

data_meds <- meds %>%
    distinct(patient, med, scheduled) %>%
    group_by(patient, scheduled)

data_meds_num <- data_meds %>%
    summarize(num_meds = n()) %>%
    mutate(scheduled = if_else(scheduled, "num_scheduled", "num_prn", "num_unknown")) %>%
    spread(scheduled, num_meds) %>%
    mutate(num_meds = sum(num_scheduled, num_prn, num_unknown, na.rm = TRUE))

data_meds_common <- meds %>%
    left_join(main[c("patient", "pmh_htn")], by = "patient") %>%
    distinct(patient, med, pmh_htn) %>%
    group_by(pmh_htn) %>%
    count(med) %>%
    arrange(pmh_htn, desc(n), med)

hm <- main %>%
    select(patient, starts_with("HM - "))
    # mutate_if(is.character, funs(. == "Yes"))

names(hm) <- str_to_lower(str_replace_all(names(hm), "HM - ", ""))

data_home_meds <- hm %>%
    gather(med, value, -patient) %>%
    filter(value == "Yes",
           med != "unknown") %>%
    arrange(patient, med) %>%
    dmap_at("value", ~ TRUE) %>%
    dmap_at("med", str_replace_all, pattern = "hctz", "hydrochlorothiazide")

data_meds_hm_inpt <- data_meds %>%
    filter(med != "none") %>%
    dmap_at("med", str_replace_all, pattern = " tartrate| succinate", replacement = "") %>%
    full_join(data_home_meds, by = c("patient", "med")) %>%
    arrange(patient, med) %>%
    mutate(same_med = !is.na(scheduled) & !is.na(value)) %>%
    group_by(patient) %>%
    summarize(num_meds = n(),
              same_hm_inpt = sum(same_med, na.rm = TRUE))
]
