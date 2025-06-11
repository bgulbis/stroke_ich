library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "abx_7jones")

raw_icu_admissions <- read_excel(paste0(f, "raw/icu_admissions.xlsx"), skip = 9) |>
    rename_all(str_to_lower)

raw_antibiotics <- read_excel(paste0(f, "raw/antibiotics.xlsx"), skip = 9) |>
    rename_all(str_to_lower)

df_icu <- raw_icu_admissions |>
    select(mrn, icu_start = `icu stay start`, icu_end = `icu stay end`) |>
    arrange(mrn, icu_start) |>
    mutate(
        icu_off = difftime(icu_start, lag(icu_end), units = "hours"),
        across(icu_off, as.numeric),
        icu_count = is.na(icu_off) | icu_off > 8,
        across(icu_count, as.numeric),
        across(icu_count, cumsum),
        .by = mrn
    ) |>
    summarize(
        across(icu_start, first),
        across(icu_end, last),
        .by = c(mrn, icu_count)
    ) |>
    filter(!is.na(icu_end))

df_abx <- raw_antibiotics |>
    select(mrn, order_id = `order id`, med_datetime = `administration instant`, medication = `component simple generic name`, route, dose = `administered dose amount`, dose_unit = `administered dose unit`, nurse_unit = department) |>
    mutate(
        across(medication, str_to_lower),
        across(medication, \(x) str_remove_all(x, "dextrose| in|sodium chloride|water for injection sterile|nacl|hcl|d5w")),
        across(medication, str_trim)
    ) |>
    arrange(mrn, order_id, med_datetime, medication)

# zz_abx <- distinct(df_abx, medication) |> arrange(medication)

excl_abx <- c("acyclovir", "acyclovir sodium", "amphotericin b liposome", "atovaquone", "bictegravir-emtricitab-tenofov", "elviteg-cobic-emtricit-tenofaf", 
    "emtricitabine-tenofovir af", "ethambutol", "fluconazole", "flucytosine", "hydroxychloroquine sulfate", "isoniazid", "oseltamivir phosphate", "posaconazole", 
    "pyrazinamide", "remdesivir", "rifaximin", "valacyclovir", "voriconazole")

df_abx_times <- df_abx |>
    med_runtime(.id = mrn) |>
    summarize(
        across(dose_start, first),
        across(dose_stop, last),
        across(c(num_doses, duration), sum),
        .by = c(mrn, medication, course_count)
    ) |>
    filter(!medication %in% excl_abx) 

df_abx_icu <- df_abx_times |>
    inner_join(df_icu, by = "mrn", relationship = "many-to-many") |>
    mutate(
        icu = int_overlaps(interval(dose_start, dose_stop), interval(icu_start, icu_end)),
        start_icu = dose_start >= icu_start & dose_start < icu_end,
        across(duration, \(x) x / 24)
    ) |>
    filter(
        dose_start < icu_start + days(7),
        icu
    ) |>
    arrange(mrn, dose_start, medication)

df_abx_icu7d <- df_abx_icu |>
    distinct(mrn, medication, .keep_all = TRUE) |>
    count(mrn, name = "num_abx")

l <- list(
    "abx" = df_abx_icu,
    "num_abx_7days" = df_abx_icu7d
)

write.xlsx(l, paste0(f, "final/abx_7jones.xlsx"), overwrite = TRUE)
