library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)
library(themebg)           

pts <- read_excel(
    "U:/Data/stroke_ich/hannah/raw/sbp_fins.xlsx", 
    col_names = c("fin", "sbp_mean", "sbp_median", "sbp_goal"), 
    col_types = c("text", "numeric", "numeric", "numeric"),
    skip = 1
) %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    select(fin, sbp_goal)

mbo_fin <- edwr::concat_encounters(pts$fin)
print(mbo_fin)

raw_sbp <- read_csv("U:/Data/stroke_ich/hannah/raw/ich_hannah_sbp.csv", locale = locale(tz = "US/Central")) %>%
    rename_all(str_to_lower) %>%
    mutate(across(c(encntr_id, fin, event_id), as.character)) %>%
    arrange(encntr_id, vital_datetime)
    
df_sbp_daily <- raw_sbp %>%
    mutate(vital_date = floor_date(vital_datetime, unit = "day")) %>%
    group_by(fin, vital_date) %>%
    summarize(across(result_val, list(mean = mean, median = median), na.rm = TRUE, .names = "{.fn}"))

df_sbp_goal <- raw_sbp %>%
    group_by(fin) %>%
    mutate(
        sbp_goal_hrs = difftime(vital_datetime, first(vital_datetime), units = "hours"),
        across(sbp_goal_hrs, as.numeric)
    ) %>%
    left_join(pts, by = "fin") %>%
    filter(result_val < sbp_goal) %>%
    distinct(fin, .keep_all = TRUE) %>%
    select(fin, sbp_goal_hrs)

l <- list(
    "sbp_daily" = df_sbp_daily,
    "sbp_goal" = df_sbp_goal
)

write.xlsx(l, paste0("U:/Data/stroke_ich/hannah/final/sbp_data_", today(), ".xlsx"))

