library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "sah_vol_sophie")

raw_pts <- read_excel(paste0(f, "raw/sah_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(fin, \(x) str_remove_all(x, "-")))

mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)

raw_ins_outs <- read_csv(paste0(f, "raw/ins_outs.csv")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

raw_labs_vitals <- read_csv(paste0(f, "raw/labs_vitals.csv")) |>
    rename_all(str_to_lower) |> 
    mutate(across(event, str_to_lower))

df_labs <- raw_labs_vitals |> 
    mutate(event_date = floor_date(event_datetime, unit = "day")) |> 
    arrange(encntr_id, fin, event_datetime, event) |> 
    distinct(fin, event_date, event, .keep_all = TRUE) |> 
    select(fin, event_date, event, result_val) |> 
    pivot_wider(names_from = event, values_from = result_val) |> 
    arrange(fin, event_date)

df_io <- raw_ins_outs |> 
    mutate(
        event_date = floor_date(event_datetime, unit = "day"),
        shift_datetime = event_datetime - hours(7),
        shift_date = floor_date(shift_datetime, unit = "day")
    )

df_io_date <- df_io |> 
    mutate(foley = str_detect(event, "indwelling cath")) |> 
    summarize(
        across(c(io_volume, foley), \(x) sum(x, na.rm = TRUE)),
        .by = c(fin, event_date, io_type)
    ) |> 
    mutate(
        across(foley, \(x) x > 0),
        across(io_type, str_to_lower)
    ) |> 
    pivot_wider(names_from = io_type, values_from = c(io_volume, foley)) |> 
    arrange(fin, event_date) |> 
    mutate(
        across(io_volume_out, \(x) coalesce(x, 0)),
        across(foley_out, \(x) coalesce(x, FALSE)),
        io_net = io_volume_in - io_volume_out) |> 
    select(fin, event_date, io_in = io_volume_in, io_out = io_volume_out, io_net, foley = foley_out)
    
df_io_shift <- df_io |> 
    mutate(foley = str_detect(event, "indwelling cath")) |> 
    summarize(
        across(c(io_volume, foley), \(x) sum(x, na.rm = TRUE)),
        .by = c(fin, shift_date, io_type)
    ) |> 
    mutate(
        across(foley, \(x) x > 0),
        across(io_type, str_to_lower)
    ) |> 
    pivot_wider(names_from = io_type, values_from = c(io_volume, foley)) |> 
    arrange(fin, shift_date) |> 
    mutate(
        across(io_volume_out, \(x) coalesce(x, 0)),
        across(foley_out, \(x) coalesce(x, FALSE)),
        io_net = io_volume_in - io_volume_out) |> 
    select(fin, event_date = shift_date, io_in = io_volume_in, io_out = io_volume_out, io_net, foley = foley_out)

l <- list(
    "labs" = df_labs,
    "io_by_shift" = df_io_shift,
    "io_by_date" = df_io_date
)

write.xlsx(l, paste0(f, "final/sah_volume_data.xlsx"), overwrite = TRUE)
