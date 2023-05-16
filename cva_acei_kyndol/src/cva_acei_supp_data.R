library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)
# library(themebg)
# library(broom)

f <- set_data_path("stroke_ich", "cva_acei_kyndol")

raw_screen <- read_excel(paste0(f, "raw/supp_data_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    select(mrn, encounter) |> 
    mutate(
        across(c(mrn, encounter), as.character),
        across(encounter, str_pad, width = 4, side = "left", pad = "0"),
        # across(
        #     encounter, 
        #     ~if_else(str_length(.) == 3, str_pad(.))
        # ),
        fin = str_c(mrn, encounter, sep = "")
    ) 

# x <- count(raw_screen, med_group)

mbo_fin <- concat_encounters(raw_screen$fin)
print(mbo_fin)

raw_meds <- get_xlsx_data(paste0(f, "raw"), "meds_dialysis_data", "meds")
raw_dialysis <- get_xlsx_data(paste0(f, "raw"), "meds_dialysis_data", "dialysis")

data_meds <- raw_screen |> 
    select(fin) |> 
    full_join(raw_meds, by = "fin") |> 
    mutate(given = TRUE) |> 
    select(-med_datetime) |> 
    pivot_wider(names_from = medication, values_from = given, values_fill = FALSE)

l <- list(
    "meds" = data_meds,
    "med_dates" = raw_meds,
    "dialysis" = raw_dialysis
)

# write.xlsx(l, paste0(f, "final/meds_dialysis_data.xlsx"), overwrite = TRUE)

raw_demog <- get_xlsx_data(paste0(f, "raw"), "supplemental_data", "demographics")
raw_allergy <- get_xlsx_data(paste0(f, "raw"), "supplemental_data", "allergies") |> 
    mutate(across(allergy, str_to_lower))

allergy_meds <- str_c(
    "benazepril",
    "captopril",
    "enalapril",
    "fosinopril",
    "lisinopril",
    "moexipril",
    "perindopril",
    "quinapril",
    "ramipril",
    "trandolapril",
    "ace inhibitor",
    "angiotensin converting enzyme",
    "benicar",
    "irbesartan",
    "telmisartan",
    "losartan",
    "olmesartan",
    "valsartan",
    sep = "|"
)

df_acei_allergy <- raw_allergy |> 
    filter(str_detect(allergy, allergy_meds)) |> 
    mutate(
        value = TRUE,
        across(allergy, str_replace_all, pattern = "angiotensin converting enzyme", replacement = "ace"),
        across(allergy, str_replace_all, pattern = "ace inhibitors", replacement = "ace_inhibitors"),
        across(allergy, str_replace_all, pattern = "benicar", replacement = "olmesartan"),
        across(allergy, str_replace_all, pattern = "-| |amlodipine|maleate|potassium|hydrochlorothiazide|besylate|hydrochloride", replacement = "")
    ) |> 
    distinct(fin, .keep_all = TRUE) |> 
    pivot_wider(names_from = allergy, names_sort = TRUE)

data_supp <- raw_demog |> 
    left_join(df_acei_allergy, by = "fin")

write.xlsx(data_supp, paste0(f, "final/weight_allergy_data.xlsx"), overwrite = TRUE)

raw_master <- read_excel(paste0(f, "raw/master.xlsx"), sheet = "Kyndol's project") |> 
    # rename_all(str_to_lower) |> 
    # select(mrn, encounter) |> 
    mutate(
        mrn_chr = as.character(MRN),
        encntr = str_pad(as.character(Encounter), width = 4, side = "left", pad = "0"),
        # across(encounter, str_pad, width = 4, side = "left", pad = "0"),
        # across(
        #     encounter, 
        #     ~if_else(str_length(.) == 3, str_pad(.))
        # ),
        fin = str_c(mrn_chr, encntr, sep = "")
    ) 

df_dialysis <- raw_dialysis |> 
    mutate(
        dialysis_type = case_when(
            str_detect(event, "CRRT") ~ "CRRT",
            str_detect(event, "Hemodialysis") ~ "HD",
            str_detect(event, "Peritoneal") ~ "PD"
        ),
        dialysis = TRUE
    ) |> 
    distinct(fin, dialysis_type, dialysis) |> 
    pivot_wider(names_from = dialysis_type, values_from = dialysis, values_fill = FALSE)

data_master <- raw_master |> 
    left_join(df_dialysis, by = "fin") |> 
    select(-fin, -mrn_chr, -encntr) |> 
    mutate(across(c(HD, CRRT, PD), \(x) coalesce(x, FALSE)))

write.xlsx(data_master, paste0(f, "final/master_with_dialysis.xlsx"), overwrite = TRUE)

raw_ras_pts <- read_excel(paste0(f, "raw/ras_data_patients.xlsx"), sheet = 1) |> 
    rename_all(str_to_lower) |> 
    select(mrn, encounter) |> 
    mutate(
        across(c(mrn, encounter), as.character),
        across(encounter, \(x) str_pad(x, width = 4, side = "left", pad = "0")),
        fin = str_c(mrn, encounter, sep = "")
    ) 

# raw_ras_2_doses <- read_excel(paste0(f, "raw/ras_data_patients.xlsx"), sheet = 2) |> 
#     rename_all(str_to_lower) |> 
#     select(mrn, encounter) |> 
#     mutate(
#         across(c(mrn, encounter), as.character),
#         across(encounter, \(x) str_pad(x, width = 4, side = "left", pad = "0")),
#         fin = str_c(mrn, encounter, sep = "")
#     ) 
# 
# raw_ras_3_time <- read_excel(paste0(f, "raw/ras_data_patients.xlsx"), sheet = 3) |> 
#     rename_all(str_to_lower) |> 
#     select(mrn, encounter) |> 
#     mutate(
#         across(c(mrn, encounter), as.character),
#         across(encounter, \(x) str_pad(x, width = 4, side = "left", pad = "0")),
#         fin = str_c(mrn, encounter, sep = "")
#     ) 
# 
# raw_ras_4_home <- read_excel(paste0(f, "raw/ras_data_patients.xlsx"), sheet = 4) |> 
#     rename_all(str_to_lower) |> 
#     select(mrn, encounter) |> 
#     mutate(
#         across(c(mrn, encounter), as.character),
#         across(encounter, \(x) str_pad(x, width = 4, side = "left", pad = "0")),
#         fin = str_c(mrn, encounter, sep = "")
#     ) 

mbo_ras_fin <- concat_encounters(raw_ras_pts$fin, 800)
print(mbo_ras_fin)

raw_ras_home_meds <- get_xlsx_data(paste0(f, "raw"), "home_meds_ras")

raw_ras_meds <- get_xlsx_data(paste0(f, "raw"), "^meds_ras")

raw_ras_scr <- get_xlsx_data(paste0(f, "raw"), "scr_ras")

df_ras_pts <- distinct(raw_ras_scr, encntr_id, fin)

df_ras_scr <- raw_ras_scr |> 
    mutate(
        hosp_day = difftime(event_datetime, admit_datetime, units = "days"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor),
        across(result_val, \(x) str_replace_all(x, "<|>", "")),
        across(result_val, as.numeric)
    ) |> 
    filter(
        !is.na(result_val),
        hosp_day >= 0,
        hosp_day <= 20
    ) |> 
    group_by(encntr_id, fin, hosp_day) |> 
    summarize(across(result_val, \(x) max(x, na.rm = TRUE)), .groups = "drop") |> 
    pivot_wider(names_from = hosp_day, names_prefix = "hosp_day_", names_sort = TRUE, values_from = result_val)

df_ras_meds <- raw_ras_meds |> 
    mutate(
        hosp_day = difftime(med_datetime, admit_datetime, units = "days"),
        across(hosp_day, as.numeric),
        across(hosp_day, floor)
    ) |> 
    filter(
        hosp_day >= 0,
        hosp_day <= 20
    ) |> 
    count(encntr_id, fin, hosp_day) |> 
    pivot_wider(names_from = hosp_day, names_prefix = "ras_doses_day_", names_sort = TRUE, values_from = n)

df_ras_days <- raw_ras_meds |> 
    mutate(med_date = floor_date(med_datetime, unit = "day")) |> 
    filter(med_datetime >= admit_datetime) |> 
    distinct(encntr_id, fin, med_date) |> 
    count(encntr_id, fin, name = "num_ras_days")

df_ras_meds_start <- raw_ras_meds |> 
    arrange(encntr_id, fin, med_datetime) |> 
    filter(med_datetime >= admit_datetime) |> 
    distinct(encntr_id, fin, admit_datetime, .keep_all = TRUE) |> 
    select(encntr_id, fin, admit_datetime, ras_start_datetime = med_datetime)

x <- distinct(raw_ras_home_meds, medication)

ras_meds <- c(
    "benazepril",
    "olmesartan",
    "valsartan",
    "captopril",
    "enalapril",
    "irbesartan",
    "lisinopril",
    "telmisartan",
    "losartan",
    "quinapril",
    "ramipril",
    "trandolapril"
)

df_ras_home_meds <- raw_ras_home_meds |> 
    filter(medication %in% ras_meds) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    mutate(home_ras = TRUE)

data_ras_scr <- df_ras_pts |> 
    left_join(df_ras_scr, by = c("encntr_id", "fin")) |> 
    select(-encntr_id)

data_ras_meds <- df_ras_pts |> 
    left_join(df_ras_meds, by = c("encntr_id", "fin")) |> 
    select(-encntr_id)

data_ras_start <- df_ras_meds_start |> 
    inner_join(df_ras_days, by = c("encntr_id", "fin")) |> 
    select(-encntr_id)

data_ras_home_meds <- df_ras_pts |> 
    left_join(df_ras_home_meds, by = c("encntr_id", "fin")) |> 
    select(-encntr_id) |> 
    mutate(across(home_ras, \(x) coalesce(x, FALSE)))

l2 <- list(
    "daily_scr" = data_ras_scr,
    "daily_ras_intake" = data_ras_meds,
    "start_time_rasi" = data_ras_start,
    "ras_home_med" = data_ras_home_meds
)

write.xlsx(l2, paste0(f, "final/data_ras.xlsx"), overwrite = TRUE)
