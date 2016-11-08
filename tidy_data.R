library(tidyverse)
library(readxl)

xls <- "BP Datasheet_10_27_Final.xls"

# rename Admission BP >/= 180 to Admission BP gte 180
# remove linebreak from:
# - "Admission National Institute of Health Stroke Scale"
# - "Total number of antihypertensives at home "
# - "First IV Anti-HTN Medication Date/Time"
# - "First oral Anti-HTN Medication Date/Time"
# - "First prn Anti-HTN Medication Date/Time"
# - "Safety - New stroke during hospitalization?"
# - "Safety - Decreased mental status attributed to low blood pressure"
# fix row 33, row 36; tPA administration date/time - convert unknown to N/A
# fix row 56, Total number of antihypertensives at home - convert to number
# fix row 86, Total number of antihypertensive medications at discharge

# set column types
cols <- c("numeric", rep("text", 2), rep("numeric", 2), rep("date", 2),
          "numeric", rep("text", 3), rep("numeric", 2), rep("text", 3), "date",
          rep("text", 38), "numeric", rep("date", 3), rep("text", 4),
          rep("numeric", 2), rep("text", 22), rep("numeric", 7))

main <- read_excel(xls, sheet = "Mainsheet", col_types = cols, na = "N/A") %>%
    filter(!is.na(`Patient Number`))

# fixed 8 errors with date/times; see fixes.Rds
xray <- read_excel(xls, sheet = "Radiographic")

# rename "Blood Pressure Readings " tab to remove spaces (note trailing space)
bp <- read_excel(xls, sheet = "Blood_Pressure_Readings")

location <- read_excel(xls, sheet = "DailyLocationBPLabs_NEW")
