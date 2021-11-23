library(tidyverse)
# library(readxl)
library(mbohelpr)

# f <- "/Volumes/brgulbis/Data/stroke_ich/ich_sarah/"
f <- "data/ich_sarah/"

# raw_pts <- read_excel(paste0(f, "raw/patient_list.xlsx")) |> 
#     rename_all(str_to_lower)

raw_pts <- read_csv(paste0(f, "raw/patient_list.csv")) |> 
    rename_all(str_to_lower)

df_include <- raw_pts |> 
    filter(
        (is.na(pregnant) | pregnant != "Positive"),
        ((str_detect(admit_src, regex("tfr|transfer", ignore_case = TRUE)) & !is.na(tfr_facility)) |
             !str_detect(admit_src, regex("tfr|transfer", ignore_case = TRUE))),
        (!(los < 2 & str_detect(disch_disposition, "Donor|Deceased"))),
        min_sbp > 100
    )

mbo_fin <- concat_encounters(df_include$fin, 950)
print(mbo_fin)
