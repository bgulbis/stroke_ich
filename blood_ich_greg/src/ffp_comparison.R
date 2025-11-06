library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)
# library(themebg)
library(broom)

f <- set_data_path("stroke_ich", "blood_ich_greg")

raw_greg <- read_excel(paste0(f, "greg_data.xlsx")) |> 
    rename_all(str_to_lower)

df_greg <- raw_greg |> 
    select(
        group = `ac (warf=1, doac=2)`,
        weight = `weight (kg)`,
        ffp_4 = `ffp within 4 hours`,
        ffp_24 = `ffp 24h`
    ) |> 
    mutate(
        across(group, \(x) if_else(x == 1, "warfarin", "doac")),
        across(group, factor),
        ffp_4_kg = ffp_4 / weight,
        ffp_24_kg = ffp_24 / weight
    )

df_ffp4 <- df_greg |> 
    filter(ffp_4_kg > 0) 

df_ffp4_med <- df_ffp4 |> 
    group_by(group) |> 
    summarize(
        n = n(),
        across(
            ffp_4_kg,
            list(
                median = median,
                q25 = ~quantile(., probs = 0.25, na.rm = TRUE),
                q75 = ~quantile(., probs = 0.75, na.rm = TRUE)
            )
        )
    )

df_ffp4_p <- wilcox.test(ffp_4_kg ~ group, data = df_ffp4) |> 
    tidy()
    
df_ffp24 <- df_greg |> 
    filter(ffp_24_kg > 0) 

df_ffp24_med <- df_ffp24 |> 
    group_by(group) |>
    summarize(
        n = n(),
        across(
            ffp_24_kg,
            list(
                median = median,
                q25 = ~quantile(., probs = 0.25, na.rm = TRUE),
                q75 = ~quantile(., probs = 0.75, na.rm = TRUE)
            )
        )
    )

df_ffp24_p <- wilcox.test(ffp_24_kg ~ group, data = df_ffp24) |> 
    tidy()

