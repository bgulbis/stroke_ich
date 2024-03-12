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

mbo_fin <- concat_encounters(raw_fin$fin, 980)
print(mbo_fin)
