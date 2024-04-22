library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "tbi_pe_sophie")

# raw_pts <- get_xlsx_data(paste0(f, "raw"), "tbi_pe_imaging")
# 
# raw_pts_trauma <- read_excel(paste0(f, "raw/tbi_pe_trauma.xlsx")) |> 
#     rename_all(str_to_lower)
# 
# df_trauma_pts <- anti_join(raw_pts_trauma, raw_pts, by = "fin")


raw_pts <- read_excel(paste0(f, "raw/tbi_pe_patients.xlsx")) |> 
    rename_all(str_to_lower) |> 
    distinct(fin, .keep_all = TRUE)
                          
mbo_fin <- concat_encounters(raw_pts$fin)
print(mbo_fin)

raw_demographics <- read_excel(paste0(f, "raw/demographics.xlsx")) |>
    rename_all(str_to_lower)


