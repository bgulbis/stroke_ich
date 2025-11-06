library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "pe_jamie")

# raw_pts <- get_xlsx_data(paste0(f, "raw"), "tbi_pe_imaging")
# 
# raw_pts_trauma <- read_excel(paste0(f, "raw/tbi_pe_trauma.xlsx")) |> 
#     rename_all(str_to_lower)
# 
# df_trauma_pts <- anti_join(raw_pts_trauma, raw_pts, by = "fin")


raw_pts <- read_excel(paste0(f, "raw/fin_list.xlsx")) |> 
    rename_all(str_to_lower) 

df_all <- raw_pts |> 
    select(fin = all_data) |> 
    filter(!is.na(fin)) |> 
    mutate(across(fin, as.character))

mbo_all <- concat_encounters(df_all$all_data)
print(mbo_all)

df_plt <- raw_pts |> 
    select(fin = antiplatelets) |> 
    filter(!is.na(fin)) |> 
    mutate(across(fin, as.character))

mbo_plt <- concat_encounters(df_plt$antiplatelets)
print(mbo_plt)

raw_demographics <- read_excel(paste0(f, "raw/tbi_pe_demographics.xlsx")) |>
    rename_all(str_to_lower)

raw_imaging <- read_excel(paste0(f, "raw/tbi_pe_imaging.xlsx")) |>
    rename_all(str_to_lower)

raw_meds <- get_xlsx_data(paste0(f, "raw"), "meds") |> 
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_vent_extubations <- read_excel(paste0(f, "raw/tbi_pe_vent_extubations.xlsx")) |>
    rename_all(str_to_lower)

raw_vent_times <- read_excel(paste0(f, "raw/tbi_pe_vent_times.xlsx")) |>
    rename_all(str_to_lower)


tmp_vent_last <- raw_vent_times |>
    filter(event == "Vent Stop Time") |>
    arrange(fin, result_datetime) |>
    summarize(across(result_datetime, \(x) max(x, na.rm = TRUE)), .by = fin) |>
    rename(last_vent_datetime = result_datetime)

df_vent <- raw_vent_extubations |>
    bind_rows(raw_vent_times) |>
    filter(event %in% c("Vent Start Time", "Extubation Event")) |>
    mutate(
        across(result_datetime, \(x) coalesce(x, event_datetime)),
        across(event, \(x) str_replace_all(x, pattern = c("Vent Start Time" = "intubation", "Extubation Event" = "extubation")))
    ) |>
    arrange(fin, result_datetime) |>
    mutate(new_event = event != lag(event) | is.na(lag(event)), .by = fin) |>
    mutate(across(new_event, cumsum)) |>
    distinct(fin, new_event, .keep_all = TRUE) |>
    filter(!(new_event == 1 & event == "extubation")) |>
    mutate(
        vent = TRUE,
        vent_n = cumsum(vent),
        .by = c(fin, event)
    ) |>
    select(fin, vent_n, event, result_datetime) |>
    spread(event, result_datetime) |>
    select(
        fin,
        vent_n,
        intubate_datetime = intubation,
        extubate_datetime = extubation
    ) |>
    left_join(tmp_vent_last, by = "fin") |>
    left_join(raw_demographics[c("fin", "disch_datetime")], by = "fin") |>
    mutate(
        across(extubate_datetime, \(x) coalesce(x, last_vent_datetime)),
        across(extubate_datetime, \(x) coalesce(x, disch_datetime)),
        across(extubate_datetime, \(x) if_else(x < intubate_datetime, disch_datetime, x)),
        vent_days = difftime(extubate_datetime, intubate_datetime, units = "days"),
        across(vent_days, as.numeric)
    ) |>
    summarize(
        across(vent_days, sum),
        .by = fin
    )

zz_meds <- distinct(raw_meds, medication) |> arrange(medication)

df_meds_anticoag <- raw_meds |> 
    semi_join(df_all, by = "fin") |> 
    left_join(raw_demographics[c("fin", "weight")], by = "fin") |> 
    mutate(across(weight, as.numeric)) |> 
    filter(
        (medication == "enoxaparin" & dose > 40) |
            (medication == "enoxaparin" & dose <= 40 & weight <= 45) |
            (medication == "heparin" & !is.na(iv_event)) |
            (!medication %in% c("enoxaparin", "heparin"))
    ) |> 
    distinct(fin, medication) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = medication, values_from = value, names_sort = TRUE)

df_meds_antiplt <- raw_meds |> 
    semi_join(df_plt, by = "fin") |> 
    filter(medication %in% c("aspirin", "clopidogrel")) |> 
    distinct(fin, medication) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = medication, values_from = value, names_sort = TRUE)

zz_imaging <- distinct(raw_imaging, scan) |> arrange(scan)

df_imaging <- raw_imaging |> 
    distinct(fin, scan) |> 
    mutate(value = TRUE) |> 
    pivot_wider(names_from = scan, values_from = value, names_sort = TRUE)

data_anticoag <- df_all |> 
    left_join(df_vent, by = "fin") |> 
    left_join(df_meds_anticoag, by = "fin") |> 
    left_join(df_imaging, by = "fin")

data_antiplt <- df_plt |> 
    left_join(df_meds_antiplt, by = "fin")

l <- list(
    "anticoag_pts" = data_anticoag,
    "antiplt_pts" = data_antiplt
)

write.xlsx(l, paste0(f, "final/data_jamie.xlsx"), overwrite = TRUE)
