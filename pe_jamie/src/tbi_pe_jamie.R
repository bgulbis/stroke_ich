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
    select(all_data) |> 
    filter(!is.na(all_data))

mbo_all <- concat_encounters(df_all$all_data)
print(mbo_all)

df_plt <- raw_pts |> 
    select(antiplatelets) |> 
    filter(!is.na(antiplatelets))

mbo_plt <- concat_encounters(df_plt$antiplatelets)
print(mbo_plt)
