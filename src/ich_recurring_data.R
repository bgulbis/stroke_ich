library(tidyverse)
library(lubridate)

tz <- "US/Central"
tz_locale <- locale(tz = tz)
# data_month <- mdy("2/1/2019", tz = tz)
data_month <- floor_date(rollback(now(), FALSE, FALSE), unit = "month")

month_abbrv <- format(data_month, "%Y-%m")

dir_data <- "data/tidy/mbo"

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

# data -------------------------------------------------

data_patients <- get_data(dir_data, "patients")
data_vitals <- get_data(dir_data, "vitals")


# sbp --------------------------------------------------

df_sbp <- data_vitals %>%
    filter(
        event %in% c("Systolic Blood Pressure", "Arterial Systolic BP 1")
    ) %>%
    arrange(encounter_id, event_datetime) 


