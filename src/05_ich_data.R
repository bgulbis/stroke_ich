library(tidyverse)
library(lubridate)
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
