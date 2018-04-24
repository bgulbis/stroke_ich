library(tidyverse)
library(lubridate)
library(readxl)
library(edwr)

dir_raw <- "data/raw/cse"

pts <- read_excel("data/raw/cse/post_patients.xlsx") %>%
    rename(fin = `FIN#`) %>%
    mutate_at("fin", str_trim)

mbo_fin <- concat_encounters(pts$fin)

# run MBO query
#   * Identifiers - by FIN

id <- read_data(dir_raw, "post_id-fin", FALSE) %>%
    as.id()

mbo_id <- concat_encounters(id$millennium.id)

# run MBO query
#   * Vitals - BP

vitals_sbp <- dir_raw %>%
    read_data("post_vitals", FALSE) %>%
    as.vitals() %>%
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

vitals_hr <- dir_raw %>%
    read_data("post_vitals", FALSE) %>%
    as.vitals() %>%
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
    ungroup() %>%
    select(
        fin,
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
    "data/external/post_sbp_data.csv",
    row.names = FALSE
)

# vitals %>%
#     ggplot(aes(x = bp.time, y = vital.result)) +
#     geom_point(shape = 1) +
#     geom_smooth()
#
# library(qicharts2)
# vitals %>%
#     # filter(fin == "473129689367") %>%
#     qic(x = bp.time, y = vital.result, data = ., chart = "i")
