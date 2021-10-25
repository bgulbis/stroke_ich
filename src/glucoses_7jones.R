library(tidyverse)
library(readxl)
library(mbohelpr)

f <- "U:/Data/stroke_ich/glucoses/raw/"

raw_insulin <- read_excel(paste0(f, "glucose_control_7jones.xlsx"), sheet = "insulin_drips") |>
    rename_all(str_to_lower)

raw_glucose <- read_excel(paste0(f, "glucose_control_7jones.xlsx"), sheet = "glucoses") |>
    rename_all(str_to_lower)

df_drip <- raw_insulin |> 
    drip_runtime(vars(encntr_id)) |> 
    summarize_drips(vars(encntr_id))

df_drip_times <- df_drip |> 
    select(encntr_id, start_datetime, stop_datetime)

df_drip_pts <- df_drip |> 
    distinct(encntr_id) |> 
    mutate(drip = TRUE)

data_drip_duration <- df_drip |> 
    summarize(across(c(duration, infusion_run_time), list(mean = mean, sd = sd, median = median, iqr = IQR)))

data_drip_duration_pt <- df_drip |> 
    group_by(encntr_id) |> 
    summarize(across(c(duration, infusion_run_time), sum, na.rm = TRUE)) |> 
    mutate(drip = TRUE)

df_glucose <- raw_glucose |> 
    filter(!is.na(result_units)) |> 
    mutate(
        censor_high = str_detect(result_val, ">"),
        censor_low = str_detect(result_val, "<"),
        across(result_val, str_replace_all, pattern = ">|<", replacement = ""),
        across(result_val, as.numeric)
    )

df_gluc_drip_event <- df_glucose |> 
    inner_join(df_drip_times, by = "encntr_id") |> 
    filter(
        lab_datetime >= start_datetime,
        lab_datetime <= stop_datetime
    ) |> 
    distinct(event_id) |> 
    mutate(drip = TRUE)

df_gluc_nodrip_event <- df_glucose |> 
    anti_join(df_gluc_drip_event, by = "event_id") |> 
    distinct(event_id) |> 
    mutate(drip = FALSE)

df_gluc_event_type = bind_rows(df_gluc_drip_event, df_gluc_nodrip_event) 

df_glucose_detail <- df_glucose |> 
    left_join(df_gluc_event_type, by = "event_id") |> 
    mutate(above_goal = result_val > 180)

data_glucose_pt <- df_glucose_detail |> 
    group_by(encntr_id) |> 
    summarize(across(result_val, list(mean = mean, sd = sd, median = median, iqr = IQR), .names = "glucose_{.fn}")) |> 
    left_join(data_drip_duration_pt, by = "encntr_id") |> 
    mutate(across(drip, ~coalesce(., FALSE)))

df_glucose_pcnt <- df_glucose_detail |> 
    arrange(encntr_id, lab_datetime) |> 
    group_by(encntr_id) |> 
    mutate(
        duration = difftime(lead(lab_datetime), lab_datetime, units = "hours"),
        # start_time = difftime(lab_datetime, first(lab_datetime), units = "hours"),
        across(duration, as.numeric),
        across(duration, ~coalesce(., 1))
    )

df_gluc_duration <- df_glucose_pcnt |> 
    group_by(encntr_id) |> 
    summarize(across(duration, sum, na.rm = TRUE, .names = "total_duration")) 

df_gluc_duration_high <- df_glucose_pcnt |> 
    group_by(encntr_id, above_goal) |> 
    summarize(across(duration, sum, na.rm = TRUE)) |> 
    inner_join(df_gluc_duration, by = "encntr_id") |> 
    ungroup() |> 
    mutate(
        pct_time = duration / total_duration * 100,
        across(above_goal, ~if_else(., "pct_time_above_180", "pct_time_lte_180"))
    ) 

data_gluc_avg <- df_gluc_duration_high |> 
    ungroup() |> 
    select(-duration, -total_duration) |> 
    pivot_wider(names_from = above_goal, values_from = pct_time) |> 
    summarize(across(starts_with("pct_"), list(mean = mean, sd = sd, median = median, iqr = IQR), na.rm = TRUE))

data_gluc_avg2 <- df_gluc_duration_high |> 
    ungroup() |> 
    select(-duration, -total_duration) |> 
    pivot_wider(names_from = above_goal, values_from = pct_time) |> 
    mutate(across(starts_with("pct_"), ~coalesce(., 0))) |> 
    summarize(across(starts_with("pct_"), list(mean = mean, sd = sd), na.rm = TRUE))

data_gluc_count <- df_glucose_pcnt |> 
    add_count(encntr_id, name = "pt_readings") |> 
    group_by(encntr_id, pt_readings) |> 
    summarize(across(above_goal, sum, na.rm = TRUE)) |> 
    mutate(pct_readings_high = above_goal / pt_readings * 100)
