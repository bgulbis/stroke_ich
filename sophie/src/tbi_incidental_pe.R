library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "sophie")

raw_pts <- get_xlsx_data(paste0(f, "raw"), "tbi_pe_imaging")

raw_pts_trauma <- read_excel(paste0(f, "raw/tbi_pe_trauma.xlsx")) |> 
    rename_all(str_to_lower)

df_trauma_pts <- anti_join(raw_pts_trauma, raw_pts, by = "fin")
