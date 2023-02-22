library(tidyverse)
library(readxl)
library(mbohelpr)
library(lubridate)
library(openxlsx)
# library(themebg)
# library(broom)

f <- set_data_path("stroke_ich", "cvt_ariel")

raw_list <- read_excel(paste0(f, "raw/stroke_patient_list.xlsx")) |> 
    mutate(across(`Enc - Patient Account`, as.character))

df_all_pts <- select(raw_list, fin = `Enc - Patient Account`)

mbo_fin <- concat_encounters(df_all_pts$fin, 990)
print(mbo_fin)

df_hep_pts <- get_xlsx_data(paste0(f, "raw"), "hep_enox_pts")

data_hep_pts <- semi_join(raw_list, df_hep_pts, by = c("Enc - Patient Account" = "fin")) |> 
    filter(`Enc - Patient Type Desc` == "Inpatient")

write.xlsx(data_hep_pts, paste0(f, "final/ufh_enox_patients.xlsx"), overwrite = TRUE)
