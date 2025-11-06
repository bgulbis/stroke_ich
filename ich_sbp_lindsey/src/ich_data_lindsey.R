library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)
library(themebg)

f <- set_data_path("stroke_ich", "ich_sbp_lindsey")
tz <- "US/Central"
cdt <- locale(tz = tz)
dt_fmt <- "%m/%d/%Y %I:%M:%S %p"

raw_fin <- read_excel(paste0(f, "raw/fin_list.xlsx")) |> 
    rename_all(str_to_lower)

raw_oac <-  read_excel(paste0(f, "raw/home_oac_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(c(dialysis, warfarin, enoxaparin, apixaban, rivaroxaban), as.logical))

df_oac <- raw_oac |> 
    filter(
        (warfarin | apixaban | rivaroxaban | edoxaban | dabigatran),
        is.na(pregnant),
        is.na(dialysis)
    )


df_fix <- raw_fin |> 
    filter(str_length(fin) < 12) |> 
    mutate(
        mrn = str_sub(fin, 1, 8),
        visit = str_sub(fin, 9, -1),
        across(visit, \(x) str_pad(x, 4, side = "left", pad = 0)),
        fix_fin = str_c(mrn, visit)
    ) |> 
    select(fin = fix_fin)

df_fin <- raw_fin |> 
    filter(str_length(fin) == 12) |> 
    bind_rows(df_fix)

mbo_fin <- concat_encounters(df_fin$fin, 980)
print(mbo_fin)

raw_demog <- read_excel(paste0(f, "raw/data_demographics.xlsx")) |> 
    rename_all(str_to_lower)

raw_bp <- read_excel(paste0(f, "raw/data_bp_values.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(result_val, as.numeric))

raw_kcentra <- read_excel(paste0(f, "raw/data_kcentra.xlsx")) |> 
    rename_all(str_to_lower)

raw_transfuse <- read_excel(paste0(f, "raw/data_transfusion.xlsx")) |> 
    rename_all(str_to_lower)

raw_io_blood <- read_excel(paste0(f, "raw/data_io_blood.xlsx")) |> 
    rename_all(str_to_lower)

raw_home_meds <- read_excel(paste0(f, "raw/data_home_meds.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

df_times <- raw_demog |> 
    select(fin, arrive_datetime)

df_kcentra <- raw_kcentra |> 
    inner_join(df_times, by = "fin") |> 
    filter(med_datetime <= arrive_datetime + hours(6)) |> 
    distinct(fin) |> 
    mutate(kcentra = TRUE)

# x <- distinct(raw_transfuse, product) |> arrange(product)

df_ffp_transfused <- raw_transfuse |> 
    filter(str_detect(product, "Plasma")) |> 
    inner_join(df_times, by = "fin") |> 
    filter(event_datetime <= arrive_datetime + hours(6)) |> 
    distinct(fin)

# x <- distinct(raw_io_blood, event) |> arrange(event)

df_ffp_io <- raw_io_blood |> 
    filter(event == "FFP Volume") |> 
    inner_join(df_times, by = "fin") |> 
    filter(event_datetime <= arrive_datetime + hours(6)) |> 
    distinct(fin)

df_ffp <- df_ffp_transfused |> 
    bind_rows(df_ffp_io) |> 
    distinct(fin) |> 
    mutate(ffp = TRUE)

df_sbp <- raw_bp |> 
    filter(
        str_detect(event, "Systolic"),
        !is.na(result_val),
        result_val > 40
    ) |> 
    inner_join(df_times, by = "fin") |> 
    mutate(arrive_event_hr = as.numeric(difftime(event_datetime, arrive_datetime, units = "hours")))

# df_sbp |> 
#     filter(
#         arrive_event_hr > 0,
#         arrive_event_hr <= 24
#     ) |> 
#     ggplot(aes(x = arrive_event_hr, y = result_val)) +
#     geom_point(shape = 1, alpha = 0.4) +
#     geom_vline(xintercept = 4, color = "green") +
#     geom_smooth() +
#     scale_x_continuous(breaks = seq(0, 24, 4))
    
df_sbp_max_top3 <- df_sbp |>
    filter(
        arrive_event_hr > 0,
        arrive_event_hr <= 4
    ) |>
    slice_max(result_val, n = 3, by = fin) |> 
    summarize(across(result_val, mean), .by = fin) |>
    mutate(
        sbp_25pct = result_val * 0.75,
        across(c(result_val, sbp_25pct), \(x) round(x, 0))
    ) |> 
    select(fin, sbp_max_top3 = result_val, sbp_25pct)

# df_sbp_max_4h <- df_sbp |>
#     filter(
#         arrive_event_hr > 0,
#         arrive_event_hr <= 4
#     ) |>
#     summarize(across(result_val, max), .by = fin) |>
#     select(fin, sbp_max_4h = result_val) |> 
#     left_join(df_sbp_max_top3, by = "fin")


# df_sbp_mean_0h <- df_sbp |> 
#     filter(event_datetime <= arrive_datetime + hours(1)) |> 
#     summarize(across(result_val, mean), .by = fin) |> 
#     mutate(across(result_val, \(x) round(x, 0))) |> 
#     select(fin, sbp_mean_0h = result_val)

# df_sbp_4h <- df_sbp |> 
#     filter(
#         arrive_event_hr > 0,
#         arrive_event_hr <= 4
#     ) |> 
#     mutate(event = "sbp") |> 
#     calc_runtime(.id = fin) |> 
#     summarize_data(.id = fin, .result = result_val) |> 
#     select(fin, sbp_initial = first_result, sbp_mean_first_4h = time_wt_avg, sbp_median_first_4h = median_result, sbp_max_first_4h = max_result)
# 
# df_sbp_24h <- df_sbp |> 
#     filter(
#         arrive_event_hr > 4,
#         arrive_event_hr <= 24
#     ) |> 
#     mutate(event = "sbp") |> 
#     calc_runtime(.id = fin) |> 
#     summarize_data(.id = fin, .result = result_val) |> 
#     select(fin, sbp_last = last_result, sbp_mean_4h_24h = time_wt_avg, sbp_median_4h_24h = median_result, sbp_max_4h_24h = max_result)


df_sbp_mean_4h <- df_sbp |> 
    filter(
        arrive_event_hr > 3,
        arrive_event_hr <= 5
    ) |> 
    summarize(across(result_val, mean), .by = fin) |> 
    mutate(across(result_val, \(x) round(x, 0))) |> 
    select(fin, sbp_mean_4h = result_val) |> 
    inner_join(df_sbp_max_top3, by = "fin") |> 
    mutate(
        sbp_pct_4h = 1 - (sbp_mean_4h / sbp_max_top3),
        sbp_decr_25pct_4h = sbp_mean_4h <= sbp_25pct
    )

# df_sbp_decr_25pct_4h <- df_sbp_mean_4h |> 
#     filter(sbp_decr_25pct_4h)

df_sbp_mean_24h <- df_sbp |> 
    # semi_join(df_sbp_decr_25pct_4h, by = "fin") |> 
    filter(
        arrive_event_hr > 23,
        arrive_event_hr <= 25
    ) |> 
    summarize(across(result_val, mean), .by = fin) |> 
    mutate(across(result_val, \(x) round(x, 0))) |> 
    select(fin, sbp_mean_24h = result_val) |> 
    inner_join(df_sbp_max_top3, by = "fin") |> 
    mutate(
        sbp_pct_24h = 1 - (sbp_mean_24h / sbp_max_top3),
        sbp_decr_25pct_24h = sbp_mean_24h <= sbp_25pct
    )

df_sbp_decr_25pct_24h <- df_sbp |> 
    # semi_join(df_sbp_decr_25pct_4h, by = "fin") |> 
    filter(
        arrive_event_hr > 4,
        arrive_event_hr <= 24
    ) |>     
    inner_join(df_sbp_max_top3, by = "fin") |> 
    mutate(sbp_decr_25pct = result_val <= sbp_25pct) |> 
    summarize(
        n_sbp = n(),
        across(sbp_decr_25pct, sum),
        .by = fin
    ) |> 
    mutate(pct_sbp_25pct_decr = sbp_decr_25pct / n_sbp)


# df_sbp_25pct_4h <- df_sbp |> 
#     filter(
#         arrive_event_hr > 3,
#         arrive_event_hr <= 5
#     ) |> 
#     summarize(across(result_val, list(mean = mean, median = median), .names = "sbp_{.fn}_4h"), .by = fin) |> 
#     left_join(df_sbp_4h, by = "fin") |> 
#     mutate(
#         sbp_25pct_decr_mean_4h = 1 - (sbp_mean_4h / sbp_max_first_4h),
#         sbp_25pct_decr_median_4h = 1 - (sbp_median_4h / sbp_max_first_4h)
#     )

# df_sbp_drop <- raw_demog |> 
#     select(fin) |> 
#     left_join(df_sbp_mean_0h, by = "fin") |> 
#     # left_join(df_sbp_mean_4h, by = "fin") |> 
#     # left_join(df_sbp_mean_24h, by = "fin") |> 
#     left_join(df_sbp_max_4h, by = "fin") |> 
#     mutate(
#         sbp_25pct_mean = sbp_mean_0h * 0.75,
#         sbp_25pct_max = sbp_max_4h * 0.75
#         # sbp_pct_drop_from_mean_4h = 1 - (sbp_mean_4h / sbp_mean_0h),
#         # sbp_pct_drop_from_max_4h = 1 - (sbp_mean_4h / sbp_max_4h),
#         # sbp_pct_drop_from_mean_24h = 1 - (sbp_mean_24h / sbp_mean_0h),
#         # sbp_pct_drop_from_max_24h = 1 - (sbp_mean_24h / sbp_max_4h),
#         # sbp_25pct_drop_mean_4h = sbp_mean_4h <= sbp_25pct_drop_mean,
#         # sbp_25pct_drop_max_4h = sbp_mean_4h <= sbp_25pct_drop_max
#     )
    
# x <- distinct(raw_home_meds, medication) |> arrange(medication)

df_home_oac <- raw_home_meds |> 
    filter(medication %in% c("apixaban", "dabigatran", "enoxaparin", "heparin", "rivaroxaban", "warfarin")) |> 
    distinct(fin, medication) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = medication, values_from = value, names_prefix = "home_")

data_patients <- raw_demog |> 
    select(fin) |> 
    left_join(df_home_oac, by = "fin") |> 
    left_join(df_ffp, by = "fin") |> 
    left_join(df_kcentra, by = "fin") |> 
    left_join(df_sbp_mean_4h, by = "fin") |> 
    left_join(df_sbp_mean_24h, by = c("fin", "sbp_max_top3", "sbp_25pct")) |> 
    left_join(df_sbp_decr_25pct_24h, by = "fin") |> 
    mutate(sbp_decr_sustained = sbp_decr_25pct_4h & sbp_decr_25pct_24h)

write.xlsx(data_patients, paste0(f, "final/sbp_data_lindsey.xlsx"), overwrite = TRUE)    
