library(tidyverse)
library(readxl)
library(mbohelpr)

f <- set_data_path("stroke_ich", "sophie")

raw_screen <- read_excel(paste0(f, "raw/tbi_codes.xlsx")) |> 
    distinct(TBI_CODE) |> 
    arrange(TBI_CODE)
