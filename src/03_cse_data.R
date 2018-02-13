library(tidyverse)
library(lubridate)
library(readxl)
library(edwr)

dir_raw <- "data/raw/cse"

pts <- read_excel("data/raw/cse/patient_list.xlsx") %>%
    rename(fin = `FIN#`) %>%
    mutate_at("fin", str_trim)

mbo_fin <- concat_encounters(pts$fin)

# run MBO query
#   * Identifiers - by FIN

id <- read_data(dir_raw, "id-fin", FALSE) %>%
    as.id()

mbo_id <- concat_encounters(id$millennium.id)

# run MBO query
#   * Vitals - BP

vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals() %>%
    filter(vital %in% c("systolic blood pressure", "arterial systolic bp 1")) %>%
    arrange(millennium.id, vital.datetime) %>%
    group_by(millennium.id) %>%
    mutate(sbp_160 = (vital.result < 160 & lag(vital.result) < 160),
           sbp_140 = (vital.result < 140 & lag(vital.result) < 140)) %>%
    left_join(id, by = "millennium.id") %>%
    ungroup() %>%
    select(fin, vital, vital.datetime, vital.result, sbp_160, sbp_140,
           vital.location)

write.csv(vitals, "data/external/sbp_data.csv", row.names = FALSE)
