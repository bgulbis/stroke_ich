library(tidyverse)
library(readxl)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("stroke_ich", "carotid_stent_kayla")

raw_screen <- get_xlsx_data(paste0(f, "raw"), pattern = "screening")

mbo_fin <- concat_encounters(raw_screen$fin)
print(mbo_fin)

raw_demographics <- read_excel(paste0(f, "raw/demographics.xlsx")) |>
    rename_all(str_to_lower)

raw_diagnosis <- read_excel(paste0(f, "raw/diagnosis.xlsx")) |>
    rename_all(str_to_lower)

raw_home_meds <- read_excel(paste0(f, "raw/home_meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

raw_labs_vitals <- read_excel(paste0(f, "raw/labs_vitals.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(c(event, result_val), str_to_lower))

raw_labs_asa <- read_excel(paste0(f, "raw/labs_asa.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(c(event, result_val), str_to_lower))

df_labs_vitals <- raw_labs_vitals |> 
    bind_rows(raw_labs_asa) |>
    filter(!str_detect(result_val, "[a-z]")) |> 
    mutate(
        censor_low = str_detect(result_val, "<"),
        censor_high = str_detect(result_val, ">"),
        across(result_val, \(x) str_remove_all(x, "<|>")),
        across(result_val, as.numeric)
    ) 
    
raw_locations <- read_excel(paste0(f, "raw/locations.xlsx")) |>
    rename_all(str_to_lower)

raw_meds <- read_excel(paste0(f, "raw/meds.xlsx")) |>
    rename_all(str_to_lower) |> 
    mutate(across(medication, str_to_lower))

zz_labs <- distinct(raw_labs_vitals, event) |> arrange(event)
zz_hm_meds <- distinct(raw_home_meds, drug_cat) |> arrange(drug_cat)
zz_meds <- distinct(raw_meds, medication) |> arrange(medication)

df_nihss <- raw_labs_vitals |> 
    filter(event == "nih stroke score") |> 
    arrange(encntr_id, event_datetime) |> 
    mutate(across(result_val, as.numeric))

df_nihss_admit <- df_nihss |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, nihss_admit = result_val)

df_diag_primary <- raw_diagnosis |> 
    filter(diag_priority == 1) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, primary_icd10 = icd_10_code, icd_10_description)

l_drug_cats <- c(
    "ACE inhibitors with thiazides",
    "HMG-CoA reductase inhibitors (statins)",
    "PCSK9 inhibitors",
    "alpha-adrenoreceptor antagonists",
    "angiotensin II inhibitors",
    "angiotensin II inhibitors with thiazides",
    "angiotensin converting enzyme (ACE) inhibitors",
    "angiotensin receptor blockers and neprilysin inhibitors",
    "antiadrenergic agents, centrally acting",
    "antiadrenergic agents, peripherally acting",
    "antihyperlipidemic combinations",
    "beta blockers with thiazides",
    "beta blockers, cardioselective",
    "beta blockers, non-cardioselective",
    "calcium channel blocking agents",
    "cholesterol absorption inhibitors",
    "coumarins and indanediones",
    "factor Xa inhibitors",
    "fibric acid derivatives",
    "miscellaneous antihyperlipidemic agents",
    "miscellaneous cardiovascular agents",
    "miscellaneous coagulation modifiers",
    "platelet aggregation inhibitors",
    "thiazide and thiazide-like diuretics",
    "vasodilators"
)

df_home_meds <- raw_home_meds |> 
    filter(drug_cat %in% l_drug_cats)

df_arrive <- select(raw_demographics, encntr_id, arrive_datetime)

df_tpa <- raw_meds |> 
    filter(medication %in% c("alteplase", "tenecteplase")) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_tpa_hrs = difftime(med_datetime, arrive_datetime, units = "hours"),
        across(arrive_tpa_hrs, as.numeric)
    ) |> 
    select(encntr_id, fin, med_datetime, medication:admin_route, arrive_tpa_hrs)

df_fibrinolytic <- df_tpa |> 
    distinct(encntr_id) |> 
    mutate(fibrinolytic = TRUE)

l_labs_baseln <- c(
    "alk phos",
    "alt",
    "ast",
    "bili total",
    "bun",
    "creatinine lvl",
    "glucose lvl",
    "hgb",
    "hct",
    "platelet",
    "inr",
    "pt",
    "ptt",
    "fibrinogen lvl",
    "hgb a1c",
    "hdl",
    "ldl (calculated)",
    "ldl direct",
    "chol",
    "trig"
)

df_labs_baseln <- df_labs_vitals |> 
    filter(event %in% l_labs_baseln) |> 
    arrange(encntr_id, event_datetime) |> 
    distinct(encntr_id, event, .keep_all = TRUE) |> 
    select(encntr_id, event, result_val) |> 
    mutate(
        across(event, \(x) str_remove_all(x, " lvl|\\(|\\)")),
        across(event, \(x) str_replace_all(x, " ", "_"))
    ) |> 
    pivot_wider(names_from = event, values_from = result_val, names_prefix = "admit_", names_sort = TRUE)

df_bp <- df_labs_vitals |> 
    filter(
        str_detect(event, "systolic|diastolic"),
        !censor_low,
        !censor_high
    ) |> 
    arrange(encntr_id, event_datetime) |> 
    select(encntr_id, fin, event_datetime, event, result_val)

df_bp_admit <- df_bp |> 
    mutate(sbp = str_detect(event, "systolic")) |> 
    distinct(encntr_id, sbp, .keep_all = TRUE) |> 
    mutate(across(sbp, \(x) if_else(x, "sbp", "dbp"))) |> 
    select(-fin, -event, -event_datetime) |> 
    pivot_wider(names_from = sbp, values_from = result_val, names_prefix = "admit_")

df_eptif_bolus <- raw_meds |> 
    filter(
        medication == "eptifibatide",
        is.na(iv_event)
    ) |> 
    arrange(encntr_id, med_datetime) |> 
    mutate(
        med_hour = floor_date(med_datetime, unit = "hour"),
        across(
            dose, \(x) case_when(
                dose_unit == "mg" ~ x * 1000,
                dose_unit == "mL" ~ x * 75 / 100 * 1000,
                .default = x
            )
        ),
        across(dose_unit, \(x) if_else(dose_unit == "mg" | dose_unit == "mL", "microgram", x))
    ) |> 
    distinct(encntr_id, med_hour, .keep_all = TRUE) |> 
    summarize(
        across(med_datetime, first),
        across(dose, sum),
        num_bolus = n(),
        .by = c(encntr_id, dose_unit)
    ) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_eptif_bolus_hrs = difftime(med_datetime, arrive_datetime, units = "hours"),
        across(arrive_eptif_bolus_hrs, as.numeric)
    ) |> 
    select(encntr_id, bolus_datetime = med_datetime, arrive_eptif_bolus_hrs, bolus_dose = dose, dose_unit, num_bolus) 

df_eptif_pts <- raw_meds |> 
    filter(
        medication == "eptifibatide",
        !is.na(iv_event)
    ) |> 
    arrange(encntr_id, med_datetime) |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_eptif_drip_hrs = difftime(med_datetime, arrive_datetime, units = "hours"),
        across(arrive_eptif_drip_hrs, as.numeric)
    ) |> 
    select(encntr_id, eptif_drip_start = med_datetime, arrive_eptif_drip_hrs)

df_weight <- raw_demographics |> 
    select(encntr_id, weight)

df_eptif_drip <- raw_meds |> 
    filter(
        medication == "eptifibatide",
        !is.na(iv_event),
        iv_event != "Bolus"
    ) |> 
    left_join(df_weight, by = "encntr_id") |> 
    mutate(
        across(order_weight, \(x) coalesce(x, weight)),
        across(freetext_rate, str_to_lower),
        across(freetext_rate, \(x) str_remove_all(x, "titrate|see comments|as directed|continue at|for 6-hours.|over 24 hours|run at|infusion rate -")),
        free_rate = str_extract(freetext_rate, "[0-9]\\.?[0-9]{0,2}"),
        across(c(free_rate, order_weight, concentration, volume), as.numeric),
        across(concentration, \(x) coalesce(x, 75)),
        freerate_units = str_extract(freetext_rate, "microgram[s]?/kg/min|mcg/kg/min|ml/h[r]?|cc/hr|ml per hour|ml/hour"),
        across(
            freerate_units, \(x) case_when(
                x %in% c("ml/h", "cc/hr", "ml per hour", "ml/hour") ~ "ml/hr",
                x %in% c("micrograms/kg/min", "mcg/kg/min") ~ "microgram/kg/min",
                .default = x
            )
        ),
        rate_adj = if_else(iv_event %in% c("Begin Bag", "Rate Change"), rate, NA_real_),
        across(
            rate_adj, \(x) case_when(
                rate_unit == "mg/hr" ~ x * 1000 / 60 / order_weight,
                rate_unit == "microgram/min" ~ x / order_weight,
                rate_unit == "mg/kg/hr" ~ x * 1000 / 60,
                rate_unit == "microgram/kg/hr" ~ x / 60,
                rate_unit == "mg/min" ~ x * 1000 / order_weight,
                rate_unit == "mg/kg/min" ~ x * 1000,
                is.na(x) & dose_unit == "mL" & !is.na(concentration) ~ dose * concentration / volume * 1000 / 60 / order_weight,
                is.na(x) & dose_unit == "mg" ~ dose * 1000 / 60 / order_weight,
                .default = x
            )
        ),
        rate_adj_unit = "microgram/kg/min"
    ) |> 
    # group_by(encntr_id) |> 
    # fill(rate_adj, rate_unit, .direction = "downup") |> 
    # ungroup()
    drip_runtime(.rate = rate_adj, .rate_unit = rate_adj_unit) |> 
    summarize_drips(.rate = rate_adj, .rate_unit = rate_adj_unit)

df_antiplt <- raw_meds |> 
    filter(medication %in% c("aspirin", "clopidogrel", "prasugrel", "ticagrelor")) |> 
    med_runtime() |> 
    summarize(
        first_dose = first(dose),
        max_dose = max(dose),
        avg_dose = mean(dose),
        last_dose = last(dose),
        num_doses = sum(num_doses),
        start_datetime = first(dose_start),
        stop_datetime = last(dose_stop),
        across(duration, last),
        .by = c(encntr_id, medication, course_count)
    ) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_antiplt_hrs = difftime(start_datetime, arrive_datetime, units = "hours"),
        across(arrive_antiplt_hrs, as.numeric)
    ) |> 
    select(-arrive_datetime)

df_antiplt_start <- df_antiplt |> 
    distinct(encntr_id, medication, .keep_all = TRUE) |> 
    select(encntr_id, medication, arrive_antiplt_hrs) |> 
    pivot_wider(names_from = medication, values_from = arrive_antiplt_hrs, names_glue = "{medication}_start_hrs")

df_vfnow <- df_labs_vitals |> 
    filter(event == "plav effect plt") |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_plav_effect_hrs = difftime(event_datetime, arrive_datetime, units = "hours"),
        across(arrive_plav_effect_hrs, as.numeric)
    ) |> 
    arrange(encntr_id, event_datetime)

df_vfnow_first <- df_vfnow |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, plav_effect_plt = result_val, arrive_plav_effect_hrs)

df_asa_effect <- df_labs_vitals |> 
    filter(event == "asa effect plt") |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        arrive_asa_effect_hrs = difftime(event_datetime, arrive_datetime, units = "hours"),
        across(arrive_asa_effect_hrs, as.numeric)
    ) |> 
    arrange(encntr_id, event_datetime)

df_asa_effect_first <- df_asa_effect |> 
    distinct(encntr_id, .keep_all = TRUE) |> 
    select(encntr_id, asa_effect_plt = result_val, arrive_asa_effect_hrs)

l_anticoag_meds <- c(
    "apixaban",
    "enoxaparin",
    "heparin",
    "rivaroxaban",
    "warfarin"
)

df_anticoag <- raw_meds |> 
    filter(
        medication %in% l_anticoag_meds,
        is.na(iv_event),
        medication != "heparin",
        !(medication == "enoxaparin" & dose <= 40)
    ) |> 
    med_runtime() |> 
    summarize(
        first_dose = first(dose),
        max_dose = max(dose),
        avg_dose = mean(dose),
        last_dose = last(dose),
        num_doses = sum(num_doses),
        start_datetime = first(dose_start),
        stop_datetime = last(dose_stop),
        across(duration, last),
        .by = c(encntr_id, medication, course_count)
    ) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        anticoag_start_hrs = difftime(start_datetime, arrive_datetime, units = "hours"),
        anticoag_stop_hrs = difftime(stop_datetime, arrive_datetime, units = "hours"),
        across(c(anticoag_start_hrs, anticoag_stop_hrs), as.numeric)
    ) |> 
    select(-arrive_datetime)

df_heparin_drip <- raw_meds |> 
    filter(
        !is.na(iv_event),
        medication == "heparin"
    ) |> 
    drip_runtime() |> 
    summarize_drips() |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        anticoag_start_hrs = difftime(start_datetime, arrive_datetime, units = "hours"),
        anticoag_stop_hrs = difftime(stop_datetime, arrive_datetime, units = "hours"),
        across(c(anticoag_start_hrs, anticoag_stop_hrs), as.numeric)
    ) |> 
    select(-arrive_datetime)

df_anticoag_first <- df_anticoag |>     
    select(encntr_id, medication, anticoag_start_hrs, anticoag_start_hrs) |> 
    bind_rows(df_heparin_drip[c("encntr_id", "medication", "anticoag_start_hrs", "anticoag_stop_hrs")]) |> 
    arrange(encntr_id, anticoag_start_hrs) |> 
    left_join(df_eptif_drip[c("encntr_id", "start_datetime")], by = "encntr_id") |>
    left_join(df_arrive, by = "encntr_id") |>
    mutate(
        eptif_stop = difftime(start_datetime, arrive_datetime, units = "hours"),
        across(eptif_stop, as.numeric)
    ) |>
    filter(anticoag_start_hrs > eptif_stop | is.na(eptif_stop)) |>
    distinct(encntr_id, .keep_all = TRUE) |> 
    rename(anticoag = medication)

l_antihtn_meds <- c(
    "amlodipine",
    "atenolol",
    "bisoprolol",
    "bumetanide",
    "captopril",
    "carvedilol",
    "clonidine",
    "diltiazem",
    "doxazosin",
    "enalapril",
    "esmolol",
    "furosemide",
    "hydralazine",
    "hydrochlorothiazide",
    "hydrochlorothiazide-triamterene",
    "irbesartan",
    "labetalol",
    "lisinopril",
    "losartan",
    "metolazone",
    "metoprolol",
    "nicardipine",
    "nifedipine",
    "nimodipine",
    "nitroglycerin",
    "olmesartan",
    "propranolol",
    "ramipril",
    "sacubitril-valsartan",
    "sotalol",
    "torsemide",
    "valsartan",
    "verapamil"
)

df_antihtn <- raw_meds |> 
    filter(
        medication %in% l_antihtn_meds,
        is.na(iv_event)
    ) |> 
    med_runtime() |> 
    summarize(
        first_dose = first(dose),
        max_dose = max(dose),
        avg_dose = mean(dose),
        last_dose = last(dose),
        num_doses = sum(num_doses),
        start_datetime = first(dose_start),
        stop_datetime = last(dose_stop),
        across(duration, last),
        .by = c(encntr_id, medication, course_count)
    ) |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        antihtn_start_hrs = difftime(start_datetime, arrive_datetime, units = "hours"),
        across(antihtn_start_hrs, as.numeric)
    ) |> 
    select(-arrive_datetime)

df_antihtn_drip <- raw_meds |> 
    filter(
        medication %in% l_antihtn_meds,
        !is.na(iv_event)
    ) |> 
    drip_runtime() |> 
    filter(!is.na(rate)) |> 
    summarize_drips() |> 
    left_join(df_arrive, by = "encntr_id") |> 
    mutate(
        antihtn_start_hrs = difftime(start_datetime, arrive_datetime, units = "hours"),
        across(antihtn_start_hrs, as.numeric)
    ) |> 
    select(-arrive_datetime)

zz_nurse_units <- distinct(raw_locations, nurse_unit) |> arrange(nurse_unit)

df_icu_los <- raw_locations |> 
    filter(nurse_unit %in% c("HH 7J", "HH CCU", "HH CVICU", "HH HFIC", "HH NVIC", "HH S MICU", "HH S STIC")) |> 
    summarize(
        icu_los = sum(unit_los),
        .by = encntr_id
    )

data_patients <- raw_demographics |> 
    select(encntr_id:weight, los, admit_datetime, disch_datetime, disch_disposition) |> 
    left_join(df_icu_los, by = "encntr_id") |> 
    left_join(df_diag_primary, by = "encntr_id") |> 
    left_join(df_fibrinolytic, by = "encntr_id") |> 
    left_join(df_nihss_admit, by = "encntr_id") |> 
    left_join(df_bp_admit, by = "encntr_id") |> 
    left_join(df_labs_baseln, by = "encntr_id") |> 
    left_join(df_eptif_bolus, by = "encntr_id") |> 
    left_join(df_eptif_pts, by = "encntr_id") |> 
    left_join(df_antiplt_start, by = "encntr_id") |> 
    left_join(df_vfnow_first, by = "encntr_id") |> 
    left_join(df_asa_effect_first, by = "encntr_id") |>
    left_join(df_anticoag_first, by = "encntr_id") |> 
    select(-encntr_id)

data_nihss <- df_nihss |> 
    select(fin, event_datetime, nihss = result_val)

data_home_meds <- df_home_meds |> 
    select(-encntr_id, -order_id)

data_tpa <- select(df_tpa, -encntr_id)

data_bp <- df_bp |> 
    distinct(encntr_id, event, event_datetime, .keep_all = TRUE) |> 
    pivot_wider(names_from = event, values_from = result_val) |> 
    select(-encntr_id)

data_eptif_drip <- df_eptif_drip |> 
    left_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, medication:rate_adj_unit, first_rate, max_rate, avg_rate = time_wt_avg_rate, duration)

data_antiplt <- df_antiplt |> 
    left_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, everything(), -encntr_id)

data_vfnow <- df_vfnow |> 
    # left_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, event_datetime, event:result_units, arrive_plav_effect_hrs)

data_asa_effect <- df_asa_effect |> 
    # left_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, event_datetime, event:result_units, arrive_asa_effect_hrs)

data_antihtn <- df_antihtn |> 
    select(encntr_id, medication, antihtn_start_hrs) |> 
    bind_rows(df_antihtn_drip[c("encntr_id", "medication", "antihtn_start_hrs")]) |> 
    arrange(encntr_id, antihtn_start_hrs) |> 
    left_join(raw_demographics[c("encntr_id", "fin")], by = "encntr_id") |> 
    select(fin, everything(), -encntr_id)

l <- list(
    "patients" = data_patients,
    "nihss" = data_nihss,
    "home_meds" = data_home_meds,
    "tpa" = data_tpa,
    "blood_pressures" = data_bp,
    "eptifibatide_drips" = data_eptif_drip,
    "antiplatelet_doses" = data_antiplt,
    "plavix_effect_results" = data_vfnow,
    "asa_effect_results" = data_asa_effect,
    "anithtn_meds" = data_antihtn
)

write.xlsx(l, paste0(f, "final/carotid_stent_data.xlsx"), overwrite = TRUE)
