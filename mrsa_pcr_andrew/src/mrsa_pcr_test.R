library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "mrsa_pcr_andrew")

raw_hosp <- read_excel(paste0(f, "test/hospital_admissions.xlsx"), skip = 11, 
                       col_names = c("range_start", "range_end", "mrn", "encounter_csn", "age", "sex", "race", "ethnicity", 
                                     "length_stay", "disch_disposition", "admit_date", "admit_time", "disch_date", 
                                     "disch_time", "diag_primary", "diag_pmh")) |> 
    rename_all(str_to_lower)

raw_pts <- read_excel(paste0(f, "test/patients.xlsx"), skip = 11, 
                      col_names = c("range_start", "range_end", "mrn", "age", "sex")) |> 
    rename_all(str_to_lower)

df_hosp_only <- anti_join(raw_hosp, raw_pts, by = "mrn")
df_pts_only <- anti_join(raw_pts, raw_hosp, by = "mrn")

n_mrn <- distinct(raw_hosp, mrn)

raw_ha_pcr <- read_excel(paste0(f, "test/ha_mrsa_pcr.xlsx"), skip = 11, 
                      col_names = c("range_start", "range_end", "mrn", "case", "lab_datetime", "lab", "result")) |> 
    rename_all(str_to_lower)

df_no_pcr <- anti_join(raw_hosp, raw_ha_pcr, by = "mrn")
df_no_pcr_pts <- anti_join(raw_pts, raw_ha_pcr, by = "mrn")

raw_7j_pcr <- read_excel(paste0(f, "test/mrsa_pcr_7jones.xlsx"), skip = 11, 
                         col_names = c("range_start", "range_end", "mrn", "lab_datetime", "lab", "result")) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

raw_demog <- read_excel(paste0(f, "test/demographics.xlsx"), skip = 11, 
                       col_names = c("range_start", "range_end", "mrn", "encounter_csn", "age", "sex", "race", "ethnicity", 
                                     "admit_date", "admit_time", "disch_date", "disch_time")) |> 
    rename_all(str_to_lower) |> 
    mutate(
        admit_datetime = ymd_hms(paste(admit_date, admit_time)),
        disch_datetime = ymd_hms(paste(disch_date, disch_time)),
        length_stay = difftime(disch_datetime, admit_datetime, units = "days"),
        across(length_stay, as.numeric)
    )|> 
    select(-range_start, -range_end)
    
df_pcr <- raw_7j_pcr |> 
    inner_join(raw_demog[c("mrn", "encounter_csn", "admit_datetime", "disch_datetime")], by = "mrn", relationship = "many-to-many") |> 
    filter(lab_datetime >= admit_datetime, lab_datetime <= disch_datetime)

n_mrn <- distinct(df_pcr, mrn)

raw_abx <- read_excel(paste0(f, "test/meds.xlsx"), skip = 11, 
                     col_names = c("range_start", "range_end", "mrn", "encounter_csn", "order_id", "medication", 
                                   "route", "med_datetime", "nurse_unit")) |> 
    rename_all(str_to_lower) |> 
    select(-range_start, -range_end)

df_abx_before <- raw_abx |> 
    inner_join(df_pcr, by = c("mrn", "encounter_csn"), relationship = "many-to-many") |> 
    filter(med_datetime < lab_datetime)

df_pts <- anti_join(df_pcr, df_abx_before, by = c("mrn", "encounter_csn")) |> 
    arrange(mrn, admit_datetime) |> 
    distinct(mrn, .keep_all = TRUE)

x <- anti_join(df_pts, raw_pts, by = "mrn")
x2 <- anti_join(df_pts, raw_hosp, by = "mrn")
x3 <- anti_join(raw_pts, df_pts, by = "mrn")
