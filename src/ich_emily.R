library(tidyverse)
library(readxl)
library(openxlsx)

pts <- read_excel("U:/Data/stroke_ich/emily/raw/patients.xlsx") %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    mutate(across(fin, as.character))
   
mbo_fin <- edwr::concat_encounters(pts$fin)
print(mbo_fin)

pts_sbp <- read_excel("U:/Data/stroke_ich/emily/raw/patients_sbp.xlsx") %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    mutate(across(fin, as.character))

# mbo_fin2 <- edwr::concat_encounters(pts_sbp$fin)
# print(mbo_fin2)

df_sbp <- read_excel("U:/Data/stroke_ich/emily/raw/ich_sbp.xlsx") %>%
    rename_all(str_to_lower) %>%
    mutate(across(result_val, as.numeric))

df_sbp_arrive <- df_sbp %>%
    arrange(fin, event_datetime) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_arrive = result_val)

df_sbp_6hr <- df_sbp %>%
    mutate(sbp6 = abs(6 - arrive_event_hrs)) %>%
    arrange(fin, sbp6) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_6h = result_val)

df_sbp_12hr <- df_sbp %>%
    mutate(sbp12 = abs(12 - arrive_event_hrs)) %>%
    arrange(fin, sbp12) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_12h = result_val)

df_sbp_18hr <- df_sbp %>%
    mutate(sbp18 = abs(18 - arrive_event_hrs)) %>%
    arrange(fin, sbp18) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_18h = result_val)

df_sbp_24hr <- df_sbp %>%
    mutate(sbp24 = abs(24 - arrive_event_hrs)) %>%
    arrange(fin, sbp24) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_24h = result_val)

df_sbp_change <- df_sbp_arrive %>%
    left_join(df_sbp_6hr, by = "fin") %>%
    left_join(df_sbp_12hr, by = "fin") %>%
    left_join(df_sbp_18hr, by = "fin") %>%
    left_join(df_sbp_24hr, by = "fin") %>%
    mutate(
        sbp_chg_6h = (sbp_6h - sbp_arrive) / sbp_arrive,
        sbp_chg_12h = (sbp_12h - sbp_arrive) / sbp_arrive,
        sbp_chg_18h = (sbp_18h - sbp_arrive) / sbp_arrive,
        sbp_chg_24h = (sbp_24h - sbp_arrive) / sbp_arrive
    )

write.xlsx(df_sbp_change, "U:/Data/stroke_ich/emily/final/ich_sbp_change.xlsx")
