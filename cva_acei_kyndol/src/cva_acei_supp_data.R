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
