library(tidyverse)
library(lubridate)
library(readxl)
library(edwr)

dir_raw <- "data/raw/ich"

# run MBO query
#   * Patients - by ICD
#       - Facility (Curr): HH HERMANN
#       - Admit Date: 12/31/2016 - 7/2/2017
#       - Diagnosis Code: I61.0;I61.1;I61.2;I61.3;I61.4;I61.5;I61.6;I61.8;I61.9
#       - Diagnosis Type: FINAL;DISCHARGE;BILLING

pts_ich <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

mbo_id <- concat_encounters(pts_ich$millennium.id)

# run MBO queries
#   * Diagnosis - ICD9/10-CM
#   * Location History

# find patients admitted to stroke unit first

locations <- read_data(dir_raw, "location", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    filter(!str_detect(location, "HH ED|HH ER|HH VU|HH ADMT")) %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, unit.count) %>%
    distinct(millennium.id, location) %>%
    filter(location == "HH STRK")

diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    semi_join(locations, by = "millennium.id") %>%
    filter(
        diag.type == "FINAL",
        str_detect(diag.code, "I61")
    ) %>%
    arrange(millennium.id, desc(diag.seq)) %>%
    distinct(millennium.id, .keep_all = TRUE)

mbo_ich_id <- concat_encounters(diagnosis$millennium.id)

# run MBO query
#   * Identifiers - by Millennium Encounter Id

id <- read_data(dir_raw, "identifiers", FALSE) %>%
    as.id()

pts <- id %>%
    left_join(diagnosis, by = "millennium.id") %>%
    select(-millennium.id)

write.csv(
    pts,
    "data/external/ich_patients.csv",
    row.names = FALSE
)

# include patients -------------------------------------

include <- read_excel(
    paste(dir_raw, "include_pts.xlsx", sep = "/"),
    col_names = "fin",
    col_types = "text",
    skip = 1
) %>%
    left_join(id, by = "fin")

mbo_incl <- concat_encounters(include$millennium.id)

# run MBO query
#   * Encounters
#   * Vitals - BP
#   * Vitals - HR

encntr <- dir_raw %>%
    read_data("encounters", FALSE) %>%
    as.encounters()

vitals_raw <- dir_raw %>%
    read_data("vitals", FALSE) %>%
    as.vitals()

vitals_sbp <- vitals_raw %>%
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

vitals_hr <-vitals_raw %>%
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
    left_join(id, by = "millennium.id") %>%
    left_join(encntr, by = "millennium.id") %>%
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

write.csv(
    vitals,
    "data/external/ich_sbp_data.csv",
    row.names = FALSE
)

