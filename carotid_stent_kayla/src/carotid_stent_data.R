library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "carotid_stent_kayla")

raw_screen <- get_xlsx_data(paste0(f, "raw"), pattern = "screening")

mbo_fin <- concat_encounters(raw_screen$fin)
print(mbo_fin)
