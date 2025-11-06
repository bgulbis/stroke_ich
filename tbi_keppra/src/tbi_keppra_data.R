library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "tbi_keppra")

raw_pts <- read_excel(paste0(f, "raw/tbi_patients.xlsx"), sheet = 3, range = cell_cols("A")) |> 
    rename_all(str_to_lower)

mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)

raw_psych <- read_excel(paste0(f, "raw/fins_psych_meds.xlsx")) |> 
    rename_all(str_to_lower) |> 
    filter(!is.na(fin))

mbo_psych <- concat_encounters(raw_psych$fin)
print(mbo_psych)

df_meds_sz <- raw_psych |> 
    select(fin, levetiracetam, divalproex_sodium, valproic_acid) |> 
    mutate(across(fin, as.character))

raw_psych_meds <- read_excel(paste0(f, "raw/tbi_psych_doses.xlsx")) |> 
    rename_all(str_to_lower)

df_psych_meds <- df_meds_sz |> 
    left_join(raw_psych_meds, by = "fin") |> 
    filter(med_datetime > levetiracetam | med_datetime > divalproex_sodium | med_datetime > valproic_acid) |> 
    distinct(fin, medication, .keep_all = TRUE) |> 
    select(fin, medication) |> 
    mutate(
        value = TRUE,
        across(medication, str_to_lower)
    ) |> 
    pivot_wider(names_from = "medication", names_sort = TRUE, values_from = value)

data_psych_meds <- df_meds_sz |> 
    select(fin) |> 
    left_join(df_psych_meds, by = "fin")

write.xlsx(data_psych_meds, paste0(f, "final/psych_meds.xlsx"), overwrite = TRUE)
