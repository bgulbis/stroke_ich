library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(edwr)

# runs MBO query
#   * Scheduled Queries/ich_stroke_weekly
#       - Facility (Curr): HH HERMANN
#       - Diagnosis Code: I61.0;I61.1;I61.2;I61.3;I61.4;I61.5;I61.6;I61.8;I61.9
#       - Last 7 days

edwr_class <- function(x, new_class) {
    after <- match(new_class, class(x), nomatch = 0L)

    class(x) <- append(
        class(x),
        c(new_class, "tbl_edwr"),
        after = after
    )

    attr(x, "data") <- "mbo"

    x
}

tz <- "uS/Central"

raw <- list.files("data/raw/ich_weekly", full.names = TRUE) %>%
    sort()

n_files <- length(raw)

update_time <- raw[n_files] %>%
    str_replace_all("patients_ich_|\\.xlsx", "") %>%
    ymd_hms()

# find patients admitted to stroke unit first

data_locations <- raw[n_files] %>%
    read_excel(
        sheet = "locations",
        skip = 2,
        col_names = c(
            "millennium.id",
            "arrive.datetime",
            "depart.datetime",
            "unit.name"
        ),
        col_types = c(
            "numeric",
            "date",
            "date",
            "text"
        )
    ) %>%
    edwr_class("locations") %>%
    mutate_at("millennium.id", as.character) %>%
    mutate_at(
        c("arrive.datetime", "depart.datetime"),
        with_tz,
        tzone = tz
    ) %>%
    tidy_data() %>%
    filter(!str_detect(location, "HH ED|HH ER|HH VU|HH ADMT")) %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, unit.count) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    filter(location == "HH STRK")

data_patients <- raw[n_files] %>%
    read_excel(
        sheet = "patients",
        skip = 2,
        col_names = c(
            "millennium.id",
            "fin",
            "admit.datetime",
            "discharge.datetime",
            "facility",
            "age"
        ),
        col_types = c(
            "numeric",
            "text",
            "date",
            "date",
            "text",
            "numeric"
        )
    ) %>%
    edwr_class("vitals") %>%
    mutate_at("millennium.id", as.character) %>%
    mutate_at(
        c("admit.datetime", "discharge.datetime"),
        with_tz,
        tzone = tz
    ) %>%
    semi_join(data_locations, by = "millennium.id")

data_vitals <- raw[n_files] %>%
    read_excel(
        sheet = "vitals",
        skip = 2,
        col_names = c(
            "millennium.id",
            "vital.datetime",
            "vital",
            "vital.result",
            "vital.result.units",
            "vital.location"
        ),
        col_types = c(
            "numeric",
            "date",
            "text",
            "text",
            "text",
            "text"
        )
    ) %>%
    edwr_class("vitals") %>%
    mutate_at("millennium.id", as.character) %>%
    mutate_at("vital.datetime", with_tz, tzone = tz) %>%
    mutate_at("vital.result", as.numeric) %>%
    mutate_at("vital", str_to_lower) %>%
    semi_join(data_locations, by = "millennium.id")

vitals_sbp <- data_vitals %>%
    filter(
        vital %in% c(
            "systolic blood pressure",
            "arterial systolic bp 1"
        )
    ) %>%
    arrange(millennium.id, vital.datetime) %>%
    group_by(millennium.id) %>%
    mutate(
        sbp.160 = (vital.result < 160 & lag(vital.result) < 160),
        sbp.140 = (vital.result < 140 & lag(vital.result) < 140),
        bp.time = difftime(
            lead(vital.datetime),
            vital.datetime,
            units = "min"
        )
    ) %>%
    mutate_at("bp.time", as.numeric)

vitals_hr <- data_vitals %>%
    filter(
        vital %in% c(
            "apical heart rate",
            "peripheral pulse rate"
        )
    ) %>%
    arrange(
        millennium.id,
        vital.datetime
    ) %>%
    select(
        millennium.id,
        vital.datetime,
        hr = vital.result
    )

vitals <- vitals_sbp %>%
    full_join(
        vitals_hr,
        by = c("millennium.id", "vital.datetime")
    ) %>%
    arrange(millennium.id, vital.datetime) %>%
    left_join(data_patients, by = "millennium.id") %>%
    ungroup() %>%
    select(
        fin,
        admit.datetime,
        vital,
        vital.datetime,
        bp.time,
        vital.result,
        hr,
        sbp.160,
        sbp.140,
        vital.location
    )

write.xlsx(
    vitals,
    paste0(
        "data/external/",
        format(update_time, "%Y-%m-%d"),
        "_ich_sbp_data.xlsx"
    )
)
