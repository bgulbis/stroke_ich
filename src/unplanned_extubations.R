library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

raw_fin <- read_excel(
    "U:/Data/stroke_ich/sophie/raw/nsicu_unplanned_extubation_patients.xlsx", 
    range = "C4:D68", 
    col_names = c("fin", "extubation_datetime")
) |> 
    mutate(across(fin, str_remove, pattern = "^0"))

pts_fin <- edwr::concat_encounters(raw_fin$fin)
print(pts_fin)

raw_meds <- read_excel(
    "U:/Data/stroke_ich/sophie/raw/unplanned_extubation_sedatives.xlsx"
) |> 
    rename_all(str_to_lower) |> 
    mutate(across(event_id, as.character))

df_bolus <- raw_fin |> 
    left_join(raw_meds, by = "fin") |> 
    filter(
        is.na(iv_event),
        dose > 0,
        med_datetime > extubation_datetime - hours(24),
        med_datetime <= extubation_datetime
    ) |> 
    group_by(fin, extubation_datetime, medication) |> 
    summarize(
        num_bolus_doses = n(),
        last_bolus_datetime = max(med_datetime)
    )

df_cont <- raw_fin |> 
    left_join(raw_meds, by = "fin") |> 
    filter(
        !is.na(iv_event),
        med_datetime > extubation_datetime - hours(24),
        med_datetime <= extubation_datetime + hours(3)
    ) |> 
    rename(rate_unit = rate_units) |> 
    drip_runtime(.grp_var = vars(fin, extubation_datetime)) |> 
    summarize_drips(.grp_var = vars(fin, extubation_datetime))

df_sedatives <- df_cont |> 
    select(fin, extubation_datetime, medication, start_datetime, stop_datetime) |> 
    full_join(df_bolus, by = c("fin", "extubation_datetime", "medication")) |> 
    arrange(fin, medication)

df_final <- raw_fin |> 
    left_join(df_sedatives, by = "fin")

write.xlsx(df_final, "U:/Data/stroke_ich/sophie/final/unplan_extub_sedatives.xlsx", overwrite = TRUE)
