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

tz <- "US/Central"
stroke <- "HH STRK"

raw <- list.files("data/raw/ich_weekly", full.names = TRUE) %>%
    sort()

n_files <- length(raw)

update_time <- raw[n_files] %>%
    str_replace_all(
        "data/raw/ich_weekly/ich_stroke_weekly_|\\.xlsx",
        ""
    ) %>%
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
    filter(location == stroke)

data_patients <- raw[n_files] %>%
    read_excel(
        sheet = "patients",
        skip = 2,
        col_names = c(
            "millennium.id",
            "fin",
            "arrival.datetime",
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
            "date",
            "text",
            "numeric"
        )
    ) %>%
    edwr_class("vitals") %>%
    mutate_at("millennium.id", as.character) %>%
    mutate_at(
        c("arrival.datetime", "admit.datetime", "discharge.datetime"),
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
    # filter(vital.location == stroke)

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
        sbp.150 = (vital.result < 150 & lag(vital.result) < 150),
        # sbp.140 = (vital.result < 140 & lag(vital.result) < 140),
        bp.time = difftime(
            lead(vital.datetime),
            vital.datetime,
            units = "min"
        )
    ) %>%
    mutate_at("bp.time", as.numeric) %>%
    left_join(
        data_patients[c("millennium.id", "arrival.datetime", "admit.datetime")], 
        by = "millennium.id"
    ) %>%
    group_by(millennium.id) %>% 
    arrange(millennium.id, vital.datetime) %>%
    mutate(
        admit.vital.hours = difftime(
            vital.datetime, 
            admit.datetime, 
            units = "hours"
        ),
        arrival.vital.hours = difftime(
            vital.datetime, 
            arrival.datetime, 
            units = "hours"
        )
    ) 

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
    
vitals_sbp_150 <- vitals_sbp %>%
    filter(vital.result < 150) %>%
    mutate(
        next.sbp.150 = difftime(
            lead(vital.datetime),
            vital.datetime,
            units = "hours"
        )
    ) %>%
    ungroup() %>%
    select(millennium.id, vital.datetime, vital.result, next.sbp.150)

vitals_cum_sbp_150 <- vitals_sbp %>%
    # filter(vital.result < 150) %>%
    mutate(
        next.vital = difftime(
            lead(vital.datetime),
            vital.datetime,
            units = "hours"
        ),
        lt.150 = vital.result < 150
    ) %>%
    filter(lt.150) %>%
    group_by(millennium.id) %>%
    mutate_at("next.vital", as.numeric) %>%
    mutate(cum.sbp.150 = cumsum(next.vital)) %>%
    ungroup() %>%
    select(millennium.id, vital.datetime, vital.result, cum.sbp.150)

vitals <- vitals_sbp %>%
    left_join(
        vitals_sbp_150, 
        by = c("millennium.id", "vital.datetime", "vital.result")
    ) %>%
    left_join(
        vitals_cum_sbp_150, 
        by = c("millennium.id", "vital.datetime", "vital.result")
    ) %>%
    left_join(
        vitals_hr,
        by = c("millennium.id", "vital.datetime")
    ) %>%
    left_join(data_patients[c("millennium.id", "fin")], by = "millennium.id") %>%
    arrange(millennium.id, vital.datetime) %>%
    ungroup() %>%
    select(
        fin,
        arrival.datetime,
        admit.datetime,
        vital,
        vital.datetime,
        bp.time,
        vital.result,
        hr,
        sbp.150,
        vital.location,
        admit.vital.hours,
        arrival.vital.hours,
        next.sbp.150,
        cum.sbp.150
    )

# save data to W: drive

write.xlsx(
    vitals,
    paste0(
        "/mnt/hgfs/W_Pharmacy/Stroke Unit/",
        format(update_time, "%Y-%m-%d"),
        "_ich_sbp_data.xlsx"
    )
)
