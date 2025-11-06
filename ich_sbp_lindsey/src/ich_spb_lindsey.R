library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)
library(themebg)

f <- set_data_path("stroke_ich", "ich_sbp_lindsey")
tz <- "US/Central"
cdt <- locale(tz = tz)
dt_fmt <- "%m/%d/%Y %I:%M:%S %p"

raw_screen <- read_excel(paste0(f, "raw/patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(dialysis:dabigatran, as.logical))

zz_tfr <- distinct(raw_screen, admit_src) |> arrange(admit_src)

df_screen <- raw_screen |> 
    filter(is.na(pregnant), is.na(dialysis), !str_detect(admit_src, regex("tfr", ignore_case = TRUE))) |> 
    mutate(anticoag = warfarin | enoxaparin | apixaban | rivaroxaban | edoxaban | dabigatran)

summary(df_screen)

mbo_id <- concat_encounters(df_screen$encntr_id)
print(mbo_id)

raw_codes <- read_excel(paste0(f, "raw/code_blues.xlsx")) |>
    rename_all(str_to_lower) |>
    mutate(across(ends_with("_datetime"), ~force_tz(.x, tzone = tz)))

raw_demographics <- read_excel(paste0(f, "raw/demographics.xlsx")) |>
    rename_all(str_to_lower) |>
    mutate(across(ends_with("_datetime"), ~force_tz(.x, tzone = tz)))

raw_diagnosis <- read_excel(paste0(f, "raw/diagnosis.xlsx")) |>
    rename_all(str_to_lower)

raw_dialysis <- read_excel(paste0(f, "raw/dialysis.xlsx")) |>
    rename_all(str_to_lower) |>
    mutate(across(ends_with("_datetime"), ~force_tz(.x, tzone = tz)))

raw_home_meds <- read_excel(paste0(f, "raw/home_meds.xlsx")) |>
    rename_all(str_to_lower)

raw_imaging <- read_excel(paste0(f, "raw/imaging.xlsx")) |>
    rename_all(str_to_lower) |>
    mutate(across(ends_with("_datetime"), ~force_tz(.x, tzone = tz)))

raw_ins_outs <- read_csv(paste0(f, "raw/ins_outs.csv"), locale = cdt) |>
    rename_all(str_to_lower) |>
    mutate(across(c(event, child_event, event_details), str_to_lower))

raw_labs_vitals <- read_csv(paste0(f, "raw/labs_vitals.csv"), locale = cdt) |>
    rename_all(str_to_lower) |>
    filter(
        !result_val %in% c(
            "Yes", 
            "See Note", 
            "Previously Completed", 
            "Documented to clear task",
            "Charted on Incorrect Order", 
            "Charted by Someone Else"
        )
    ) |> 
    mutate(
        across(event, str_to_lower),
        censor_low = str_detect(result_val, "<"),
        censor_high = str_detect(result_val, ">"),
        across(result_val, \(x) str_remove_all(x, ">|<|Severe disability|Moderately severe disability|Moderate disability|Slight disability|No significant disability|No symptoms|The patient has expired")),
        across(result_val, as.numeric)
    )

zz_labs <- distinct(raw_labs_vitals, event) |> arrange(event)

raw_locations <- read_excel(paste0(f, "raw/locations.xlsx")) |>
    rename_all(str_to_lower) |>
    mutate(across(ends_with("_datetime"), ~force_tz(.x, tzone = tz)))

raw_meds <- read_csv(paste0(f, "raw/meds.csv"), locale = cdt) |>
    rename_all(str_to_lower) |>
    mutate(across(medication, str_to_lower))

df_sbp <- raw_labs_vitals |> 
    filter(str_detect(event, "systolic")) |> 
    inner_join(raw_demographics[c("encntr_id", "arrive_datetime")], by = "encntr_id")  |> 
    arrange(encntr_id, event_datetime)
    
df_sbp_max3h <- df_sbp |> 
    filter(event_datetime <= arrive_datetime + hours(3)) |> 
    arrange(encntr_id, desc(result_val)) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    # summarize(across(result_val, max), .by = encntr_id)
    filter(result_val >= 220) |> 
    select(encntr_id, max_sbp_datetime = event_datetime, max_sbp_3h = result_val)

df_sbp_mean3h <- df_sbp |> 
    filter(event_datetime <= arrive_datetime + hours(3)) |> 
    summarize(
        across(result_val, list(min = min, mean = mean, sd = sd, median = median, max = max, first = first, last = last), .names = "{.fn}_sbp_3h"),
        .by = encntr_id
    ) |> 
    filter(max_sbp_3h >= 220)
    
df_sbp_chg <- df_sbp |> 
    inner_join(df_sbp_max3h, by = "encntr_id") |> 
    filter(
        event_datetime > max_sbp_datetime,
        event_datetime <= arrive_datetime + hours(24),
        result_val > 60
    ) |> 
    mutate(
        sbp_cutoff = max_sbp_3h * 0.75,
        sbp_lt25 = result_val > sbp_cutoff,
        max_hr = floor(as.numeric(difftime(event_datetime, max_sbp_datetime, units = "hours")))
    ) 
    # summarize(
    #     n_sbp = n_distinct(event_id),
    #     n_sbp_lt25 = sum(sbp_lt25),
    #     across(result_val, list(min = min, mean = mean, last = last), .names = "{.fn}_sbp_24h"),
    #     .by = c(encntr_id, max_hr, max_sbp_3h)
    # ) 
    # mutate(
    #     sbp_chg_last = (max_sbp_3h - last_sbp_24h) / max_sbp_3h,
    #     sbp_chg_min = (max_sbp_3h - min_sbp_24h) / max_sbp_3h,
    #     sbp_chg_mean = (max_sbp_3h - mean_sbp_24h) / max_sbp_3h
    # )

df_sbp_chg |> 
    filter(sbp_lt25) |> 
    ggplot(aes(max_hr)) +
    geom_histogram()

df_n <- df_sbp_chg |> 
    mutate(across(starts_with("sbp_chg"), \(x) x < 0.25)) |> 
    summarize(across(starts_with("sbp_chg"), sum))

df_sbp_24h <- df_sbp |> 
    filter(
        event_datetime >= arrive_datetime,
        event_datetime < arrive_datetime + hours(24),
        result_val > 50
    ) |> 
    arrange(encntr_id, event_datetime) |> 
    mutate(arrive_hr = as.numeric(difftime(event_datetime, arrive_datetime, units = "hours")))

df_sbp_24h |> 
    ggplot(aes(x = arrive_hr, y = result_val)) +
    geom_point(alpha = 0.3) +
    geom_smooth() +
    geom_hline(yintercept = 165, color = "grey50", linetype = "dashed") +
    scale_x_continuous("Time from arrival (hr)", seq(0, 24, 4)) +
    scale_y_continuous("SBP (mmHg)", seq(0, 400, 50)) +
    theme_bg()
