library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "ich_sbp_lindsey")

raw_screen <- read_excel(paste0(f, "raw/patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    mutate(across(dialysis, as.logical))

zz_tfr <- distinct(raw_screen, admit_src) |> arrange(admit_src)

df_screen <- raw_screen |> 
    filter(is.na(pregnant), is.na(dialysis), !str_detect(admit_src, regex("tfr", ignore_case = TRUE)))

mbo_id <- concat_encounters(df_screen$encntr_id)
print(mbo_id)
