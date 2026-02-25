library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "screen")

raw_meds <- read_excel(
    paste0(f, "raw/apix_load.xlsx"), 
    skip = 11, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "order_id", "order_name", "med_datetime", 
                  "dose", "dose_unit", "route", "freq", "nurse_unit")
) |> 
    rename_all(str_to_lower) |> 
    # mutate(across(medication, str_to_lower)) |> 
    select(-range_start, -range_end) |> 
    arrange(mrn, encounter_csn, med_datetime)

df_apix_start <- raw_meds |> 
    filter(
        str_detect(order_name, "apixaban"),
        str_detect(nurse_unit, "JONES")
    ) |> 
    distinct(mrn, encounter_csn, .keep_all = TRUE) |> 
    mutate(apix_load = dose >= 10) |> 
    select(mrn, encounter_csn, apix_start_datetime = med_datetime, apix_start_dose = dose, apix_load, apix_loc = nurse_unit)

sum(df_apix_start$apix_load)

df_anticoag_prior <- raw_meds |> 
    inner_join(df_apix_start, by = c("mrn", "encounter_csn")) |> 
    filter(
        !str_detect(order_name, "apixaban"),
        med_datetime < apix_start_datetime,
        !(str_detect(order_name, "enoxaparin") & dose <= 40)
    )

x <- distinct(df_anticoag_prior, order_name) |> arrange(order_name)

df_drips_prior <- df_anticoag_prior |> 
    filter(freq == "Continuous") |> 
    summarize(
        drip_start = first(med_datetime),
        drip_stop = last(med_datetime),
        .by = c(mrn, encounter_csn, order_name, order_id, apix_start_datetime, apix_load)
    ) |> 
    mutate(
        duration = difftime(drip_stop, drip_start, units = "days"),
        other_stop_apix = difftime(apix_start_datetime, drip_stop, units = "days"),
        across(c(duration, other_stop_apix), as.numeric)
    ) |> 
    filter(other_stop_apix < 5) |> 
    summarize(
        across(order_name, last),
        across(duration, sum),
        across(other_stop_apix, min),
        .by = c(mrn, encounter_csn, apix_load)
    ) |> 
    mutate(
        full_load = duration > 6,
        medication = case_when(
            str_detect(order_name, "heparin") ~ "heparin",
            str_detect(order_name, "bivalirudin") ~ "bivalirudin",
            str_detect(order_name, "argatroban") ~ "argatroban"
        )
    ) |> 
    select(-order_name)

sum(df_drips_prior$full_load)

df_anticoag_other <- df_anticoag_prior |> 
    filter(freq != "Continuous") |> 
    mutate(
        medication = case_when(
            str_detect(order_name, "warfarin") ~ "warfarin",
            str_detect(order_name, "enoxaparin") ~ "enoxaparin"
        )
    ) |> 
    summarize(
        other_start = first(med_datetime),
        other_stop = last(med_datetime),
        .by = c(mrn, encounter_csn, medication, apix_start_datetime, apix_load)
    ) |> 
    mutate(
        duration = difftime(other_stop, other_start, units = "days"),
        other_stop_apix = difftime(apix_start_datetime, other_stop, units = "days"),
        across(c(duration, other_stop_apix), as.numeric)
    ) |> 
    filter(other_stop_apix < 5) |> 
    summarize(
        across(medication, last),
        across(duration, sum),
        across(other_stop_apix, min),
        .by = c(mrn, encounter_csn, apix_load)
    ) |> 
    mutate(full_load = duration > 6)

df_pts <- bind_rows(df_drips_prior, df_anticoag_other) |> 
    summarize(
        across(medication, last),
        across(duration, sum),
        across(other_stop_apix, min),
        .by = c(mrn, encounter_csn, apix_load)
    ) |> 
    mutate(
        full_load = apix_load | duration > 6,
        other_load = !apix_load & duration > 6
    )

sum(df_pts$full_load)
sum(df_pts$other_load)
sum(df_pts$apix_load)
