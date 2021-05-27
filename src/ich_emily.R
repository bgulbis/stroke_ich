library(tidyverse)
library(readxl)
library(openxlsx)
library(themebg)

pts <- read_excel("U:/Data/stroke_ich/emily/raw/patients.xlsx") %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    mutate(across(fin, as.character))
   
mbo_fin <- edwr::concat_encounters(pts$fin)
print(mbo_fin)

pts_sbp <- read_excel("U:/Data/stroke_ich/emily/raw/patients_sbp.xlsx") %>%
    rename_all(str_to_lower) %>%
    distinct() %>%
    mutate(across(fin, as.character))

# mbo_fin2 <- edwr::concat_encounters(pts_sbp$fin)
# print(mbo_fin2)

df_sbp <- read_excel("U:/Data/stroke_ich/emily/raw/ich_sbp.xlsx") %>%
    rename_all(str_to_lower) %>%
    mutate(across(result_val, as.numeric))

df_sbp_arrive <- df_sbp %>%
    arrange(fin, event_datetime) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_arrive = result_val)

df_sbp_6hr <- df_sbp %>%
    mutate(sbp6 = abs(6 - arrive_event_hrs)) %>%
    arrange(fin, sbp6) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_6h = result_val)

df_sbp_12hr <- df_sbp %>%
    mutate(sbp12 = abs(12 - arrive_event_hrs)) %>%
    arrange(fin, sbp12) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_12h = result_val)

df_sbp_18hr <- df_sbp %>%
    mutate(sbp18 = abs(18 - arrive_event_hrs)) %>%
    arrange(fin, sbp18) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_18h = result_val)

df_sbp_24hr <- df_sbp %>%
    mutate(sbp24 = abs(24 - arrive_event_hrs)) %>%
    arrange(fin, sbp24) %>%
    distinct(fin, .keep_all = TRUE) %>%
    semi_join(pts_sbp, by = "fin") %>%
    select(fin, sbp_24h = result_val)

df_sbp_change <- df_sbp_arrive %>%
    left_join(df_sbp_6hr, by = "fin") %>%
    left_join(df_sbp_12hr, by = "fin") %>%
    left_join(df_sbp_18hr, by = "fin") %>%
    left_join(df_sbp_24hr, by = "fin") %>%
    mutate(
        sbp_chg_6h = (sbp_6h - sbp_arrive) / sbp_arrive,
        sbp_chg_12h = (sbp_12h - sbp_arrive) / sbp_arrive,
        sbp_chg_18h = (sbp_18h - sbp_arrive) / sbp_arrive,
        sbp_chg_24h = (sbp_24h - sbp_arrive) / sbp_arrive
    )

write.xlsx(df_sbp_change, "U:/Data/stroke_ich/emily/final/ich_sbp_change.xlsx")


# graph -------------------------------------------------------------------

pt_groups <- read_excel("U:/Data/stroke_ich/emily/raw/patient_groups.xlsx") %>%
    select(
        aki = `AKI Group FIN`,
        no_aki = `No AKI Group FIN`
    ) %>%
    pivot_longer(cols = c(aki, no_aki), names_to = "group", values_to = "fin") %>%
    mutate(across(fin, as.character))

df_fig <- df_sbp %>%
    inner_join(pt_groups, by = "fin") %>%
    filter(
        arrive_event_hrs >= 0,
        arrive_event_hrs <= 48
    )

g_fig <- df_fig %>%
    ggplot(aes(x = arrive_event_hrs, y = result_val, linetype = group)) +
    # geom_point(alpha = 0.5, shape = 1) +
    geom_smooth(color = "black") +
    # ggtitle("Figure 1. Systolic blood pressure in stroke patients with delayed discharge") +
    scale_x_continuous("Hours from arrival", breaks = seq(0, 48, 12)) +
    ylab("Systolic blood pressure (mmHg)") +
    scale_linetype_manual(NULL, values = c("solid", "dotted"), labels = c("AKI", "No AKI")) +
    coord_cartesian(ylim = c(100, 250)) +
    theme_bg_print() +
    theme(legend.position = "top")

g_fig

x <- ggplot_build(g_fig)
df_x <- x$data[[1]]

df_fig <- df_x %>%
    select(x, y, group) %>%
    pivot_wider(names_from = group, values_from = y)

write.xlsx(df_fig, "U:/Data/stroke_ich/emily/final/data_sbp_graph.xlsx")


g_fig2 <- df_fig %>%
    filter(arrive_event_hrs <= 24) %>%
    ggplot(aes(x = arrive_event_hrs, y = result_val, linetype = group)) +
    # geom_point(alpha = 0.5, shape = 1) +
    geom_smooth(color = "black") +
    # ggtitle("Figure 1. Systolic blood pressure in stroke patients with delayed discharge") +
    scale_x_continuous("Hours from arrival", breaks = seq(0, 24, 6)) +
    ylab("Systolic blood pressure (mmHg)") +
    scale_linetype_manual(NULL, values = c("solid", "dotted"), labels = c("AKI", "No AKI")) +
    coord_cartesian(ylim = c(100, 250)) +
    theme_bg_print() +
    theme(legend.position = "top")

g_fig2

