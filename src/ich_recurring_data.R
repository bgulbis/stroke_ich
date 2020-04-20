library(tidyverse)
library(lubridate)
# library(zoo)
library(data.table)

tz <- "US/Central"
tz_locale <- locale(tz = tz)
# data_month <- mdy("2/1/2019", tz = tz)
data_month <- floor_date(rollback(now(), FALSE, FALSE), unit = "month")

month_abbrv <- format(data_month, "%Y-%m")

dir_data <- "data/tidy/mbo"

target_units <- c("HH 7J", "HH STRK")

summary_fx <- list(
    mean = mean, 
    sd = sd, 
    median = median,
    q25 = ~quantile(., probs = 0.25, na.rm = TRUE),
    q75 = ~quantile(., probs = 0.75, na.rm = TRUE)
)

# helper functions -------------------------------------

get_data <- function(path, pattern, col_types = NULL) {
    f <- list.files(path, pattern, full.names = TRUE)
    
    n <- f %>%
        purrr::map_int(~ nrow(data.table::fread(.x, select = 1L)))
    
    f[n > 0] %>%
        purrr::map_df(
            readr::read_csv,
            locale = tz_locale,
            col_types = col_types
        ) %>%
        rename_all(stringr::str_to_lower)
}

# dt_vitals <- list.files(dir_data, "vitals", full.names = TRUE) %>%
#     lapply(fread) %>%
#     rbindlist()
#     
# names(dt_vitals) <- str_to_lower(names(dt_vitals))
# 
# dt_vitals <- dt_vitals[, event_datetime := as_datetime(event_datetime, tz = tz, format = "%Y-%m-%dT%H:%M:%S")]
# 
# data -------------------------------------------------

data_patients <- get_data(dir_data, "patients")

data_vitals <- get_data(dir_data, "vitals") %>%
    filter(
        !is.na(result),
        result > 40,
        result < 250
    )

df_admit_unit <- data_vitals %>%
    filter(
        admit_event_hr >= 0,
        nurse_unit %in% target_units
    ) %>%
    arrange(encounter_id, admit_event_hr) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    select(encounter_id, nurse_unit_admit = nurse_unit)

df_sbp_gt150_arrive <- data_vitals %>%
    arrange(encounter_id, arrive_event_hr) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    inner_join(df_admit_unit, by = "encounter_id") %>%
    mutate(sbp_gt150_arrive = result > 150) %>%
    select(encounter_id, sbp_gt150_arrive)

df_pts <- df_admit_unit %>%
    inner_join(df_sbp_gt150_arrive, by = "encounter_id")

# sbp mean/median --------------------------------------

dt_vitals <- data.table(data_vitals)[
    admit_event_hr >= 0,
    .(
        encounter_id, 
        event_id,
        admit_event_hr, 
        event_datetime, 
        result
    )
    ][order(encounter_id, event_datetime)]

dt_window <- dt_vitals[, .(
    encounter_id, 
    event_id, 
    window_low = event_datetime - hours(2), 
    window_high = event_datetime
)]

dt_max_sbp <- dt_vitals[
    dt_window, 
    on=.(
        encounter_id, 
        event_datetime >= window_low, 
        event_datetime <= window_high
    ), 
    .(max_sbp = max(result)), 
    by=.EACHI
    ][, event_datetime := NULL
      ][, head(.SD, 1), by = c("encounter_id", "event_datetime")]

dt_sbp_lt150 <- dt_vitals[
    dt_max_sbp, on=.(encounter_id, event_datetime)
    ][, `:=`(
        time_first = difftime(
            event_datetime, 
            first(event_datetime), 
            units = "hours")
    )
    ][time_first >= 2 & max_sbp < 150
      ][, head(.SD, 1), by = encounter_id]


df_sbp_arrive <- data_vitals %>%
    arrange(encounter_id, arrive_event_hr) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    inner_join(df_pts, by = "encounter_id") %>%
    add_count(nurse_unit_admit, sbp_gt150_arrive) %>%
    group_by(nurse_unit_admit, sbp_gt150_arrive, n) %>%
    summarize_at("result", summary_fx, na.rm = TRUE)

df_sbp_admit <- data_vitals %>%
    filter(admit_event_hr >= 0) %>%
    arrange(encounter_id, admit_event_hr) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    inner_join(df_pts, by = "encounter_id") %>%
    add_count(nurse_unit_admit, sbp_gt150_arrive) %>%
    group_by(nurse_unit_admit, sbp_gt150_arrive, n) %>%
    summarize_at("result", summary_fx, na.rm = TRUE)

df_sbp_admit_unit <- data_vitals %>%
    filter(
        admit_event_hr >= 0,
        nurse_unit %in% target_units
    ) %>%
    arrange(encounter_id, admit_event_hr) %>%
    distinct(encounter_id, .keep_all = TRUE) %>%
    inner_join(df_pts, by = "encounter_id") %>%
    add_count(nurse_unit_admit, sbp_gt150_arrive) %>%
    group_by(nurse_unit_admit, sbp_gt150_arrive, n) %>%
    summarize_at("result", summary_fx, na.rm = TRUE)

# time from admit until sbp < 150 for at least 2 hours
df_sbp_lt150 <- dt_sbp_lt150 %>%
    as_tibble() %>%
    inner_join(df_pts, by = "encounter_id") %>%
    add_count(nurse_unit_admit, sbp_gt150_arrive) %>%
    group_by(nurse_unit_admit, sbp_gt150_arrive, n) %>%
    summarize_at("admit_event_hr", summary_fx, na.rm = TRUE)
