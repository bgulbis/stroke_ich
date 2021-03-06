library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)
library(themebg)           

pts <- read_excel(
    "U:/Data/stroke_ich/isc_hannah/external/isc_patient_list.xlsx",
    sheet = "Demo_1"
) %>%
    rename_all(str_to_lower)

mbo_fin <- edwr::concat_encounters(pts$fin)
print(mbo_fin)

# data tidying ------------------------------------------------------------

f <- "U:/Data/stroke_ich/isc_hannah/raw/isc_data_2020-08-13.xlsx"

df_demog <- read_excel(f, sheet = "demographics") %>%
    rename_all(str_to_lower) %>%
    semi_join(pts, by = "fin")

df_sbp <- read_excel(f, sheet = "sbp") %>%
    rename_all(str_to_lower) %>%
    arrange(fin, vital_datetime) %>%
    mutate_at("result_val", as.numeric) %>%
    semi_join(pts, by = "fin")
    
df_home_meds <- read_excel(f, sheet = "home_meds") %>%
    rename_all(str_to_lower) %>%
    semi_join(pts, by = "fin")

# blood pressure ----------------------------------------------------------

df_sbp_admit <- df_sbp %>%
    distinct(fin, .keep_all = TRUE) %>%
    select(fin, sbp_initial = result_val)

df_sbp_daily <- df_sbp %>%
    mutate(vital_date = floor_date(vital_datetime, unit = "day")) %>%
    group_by(fin, vital_date) %>%
    summarize_at(
        "result_val", 
        list(
            median = median, 
            min = min, 
            max = max
        ),
        na.rm = TRUE
    )

df_sbp_disch <- df_sbp %>%
    group_by(fin) %>%
    mutate(
        time_last = difftime(
            last(vital_datetime), 
            vital_datetime, 
            units = "hours"
        )
    ) %>%
    filter(time_last < 24) %>%
    summarize_at(
        "result_val", 
        list(
            sbp_disch_median = median, 
            sbp_disch_min = min, 
            sbp_disch_max = max
        ),
        na.rm = TRUE
    )

# home meds ---------------------------------------------------------------

df_count_home_meds <- df_home_meds %>%
    mutate_at("home_med", str_to_lower) %>%
    filter(drug_cat != "antianginal agents") %>%
    distinct(fin, home_med) %>%
    group_by(fin) %>%
    count(fin, name = "num_home_meds")

df_cat_home_meds <- df_home_meds %>%
    mutate_at("drug_cat", str_to_lower) %>%
    filter(drug_cat != "antianginal agents") %>%
    distinct(fin, drug_cat) %>%
    mutate(value = TRUE) %>%
    pivot_wider(names_from = "drug_cat", values_from = "value", values_fill = FALSE)


# locations ---------------------------------------------------------------

df_locations <- read_excel("U:/Data/stroke_ich/isc_hannah/raw/isc_locations.xlsx") %>%
    rename_all(str_to_lower) %>%
    arrange(fin, unit_count)

df_first_unit <- df_locations %>%
    filter(
        !nurse_unit %in% c(
            "CY CYCC",
            "CY CYED",
            "CY CYVU",
            "HC S VUPD",
            "HH EDHH",
            "HH EDTR",
            "HH EREV",
            "HH S EDHH",
            "HH S EDTR",
            "HH S EREV",
            "HH S VUHH",
            "HH VUHH"
        )
    ) %>%
    distinct(fin, .keep_all = TRUE) %>%
    select(fin, initial_unit = nurse_unit, unit_los)

df_icu <- df_first_unit %>%
    filter(initial_unit %in% c("HH 7J", "HH 8WJP")) %>%
    select(fin, icu_los = unit_los)

df_nurse_unit <- df_first_unit %>%
    select(fin, initial_unit) %>%
    left_join(df_icu, by = "fin")

# x <- distinct(df_first_unit, nurse_unit)


# alteplase ---------------------------------------------------------------

df_tpa <- read_excel("U:/Data/stroke_ich/isc_hannah/raw/isc_alteplase.xlsx") %>%
    rename_all(str_to_lower) %>%
    distinct(fin) %>%
    mutate(tpa = TRUE)

# final data --------------------------------------------------------------

final_data <- df_demog %>%
    left_join(df_nurse_unit, by = "fin") %>%
    left_join(df_tpa, by = "fin") %>%
    left_join(df_count_home_meds, by = "fin") %>%
    left_join(df_sbp_admit, by = "fin") %>%
    left_join(df_sbp_disch, by = "fin") %>%
    mutate_at("tpa", ~coalesce(., FALSE))

final_home_meds <- df_demog %>%
    select(fin) %>%
    left_join(df_cat_home_meds, by = "fin")

final_sbp_daily <- df_demog %>%
    select(fin) %>%
    left_join(df_sbp_daily, by = "fin")

l <- list(
    "demographics" = final_data,
    "home_meds" = final_home_meds,
    "sbp_daily" = final_sbp_daily
)

write.xlsx(l, paste0("U:/Data/stroke_ich/isc_hannah/final/isc_data_final_", today(), ".xlsx"))


# graphs ------------------------------------------------------------------

df_oe <- read_excel("U:/Data/stroke_ich/isc_hannah/raw/isc_los_o-e-ratio.xlsx") %>%
    select(
        fin = `Encounter Number`,
        los_oe = `LOS O/E`
    ) %>%
    mutate(across(fin, str_replace_all, pattern = "C0", replacement = ""))

df_sbp_fig <- df_sbp %>%
    semi_join(pts, by = "fin") %>%
    inner_join(final_data[c("fin", "los")], by = "fin") %>%
    inner_join(df_oe, by = "fin") %>%
    mutate(across(los_oe, ~. >= 0.8)) %>% 
    group_by(fin) %>%
    mutate(time = as.numeric(difftime(vital_datetime, first(vital_datetime), units = "hours"))) 

df_sbp_fig %>%
    filter(time < 96) %>%
    ggplot(aes(x = time, y = result_val, linetype = los_oe)) +
    # geom_point(alpha = 0.5, shape = 1) +
    geom_smooth(color = "black") +
    # ggtitle("Figure 1. Systolic blood pressure in stroke patients with delayed discharge") +
    scale_x_continuous("Hours from arrival", breaks = seq(0, 96, 24)) +
    ylab("Systolic blood pressure (mmHg)") +
    scale_linetype_manual(NULL, values = c("solid", "dotted"), labels = c("LOS O/E < 0.8", "LOS O/E >/= 0.8")) +
    coord_cartesian(ylim = c(100, 200)) +
    theme_bg_print() +
    theme(legend.position = "top")

ggsave("figs/isc_fig1_sbp.jpg", device = "jpeg", width = 8, height = 6, units = "in")
