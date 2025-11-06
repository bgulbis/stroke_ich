library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)
library(themebg)

f <- set_data_path("stroke_ich", "ich_sbp_lindsey")
tz <- "US/Central"
cdt <- locale(tz = tz)
dt_fmt <- "%m/%d/%Y %I:%M:%S %p"

raw_fin <- read_excel(paste0(f, "raw/regis_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    rename(home_oac = `home oac`) |> 
    mutate(
        across(home_oac, str_to_lower),
        across(home_oac, \(x) x == "yes"),
        across(fin, as.character)
    ) |> 
    select(fin, home_oac)

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


raw_data <- read_excel(paste0(f, "raw/regis_patients_data.xlsx")) |> 
    rename_all(str_to_lower) 

data_pts <- raw_fin |> 
    left_join(raw_data, by = "fin")

write.xlsx(data_pts, paste0(f, "final/regis_patients_data.xlsx"), overwrite = TRUE)
