# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)
library(readr)

# Upload data
Output <- read_csv("~/Desktop/malnutrition/Data/Output.csv")

# Select and clean data
Data <- Output %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72, B11, V025) %>%
  filter(HW70 < 9990, HW71 < 9990, HW72 < 9990) %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),
    underweight = ifelse(HW71 < -200, 1, 0),
    wasting = ifelse(HW72 < -200, 1, 0),
    birth_interval = case_when(
      is.na(B11) | B11 == 0 ~ "First Birth",
      B11 < 24 ~ "Less than 24",
      B11 >= 24 & B11 <= 47 ~ "24-47",
      B11 >= 48 ~ "48 and above"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    birth_interval = factor(birth_interval, levels = c("First Birth", "Less than 24", "24-47", "48 and above"))
  ) %>%
  drop_na(birth_interval)  # remove cases with invalid or missing interval

# Summarize indicators by Birth Interval and Place of Residence
summary_table_b11 <- Data %>%
  group_by(birth_interval, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted),
    Underweight = sum(underweight),
    Wasting = sum(wasting)
  ) %>%
  mutate(
    Percent_Stunted = round(Stunted / Total_Children * 100, 1),
    Percent_Underweight = round(Underweight / Total_Children * 100, 1),
    Percent_Wasting = round(Wasting / Total_Children * 100, 1)
  )

# Format and display results using gt
summary_table_b11 %>%
  select(birth_interval, V025,
         Total_Children,
         Stunted, Percent_Stunted,
         Underweight, Percent_Underweight,
         Wasting, Percent_Wasting) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Summary by Birth Interval (B11) and Place of Residence"
  ) %>%
  cols_label(
    birth_interval = "Birth Interval (Months)",
    V025 = "Residence",
    Total_Children = "Total",
    Stunted = "Stunted (n)",
    Percent_Stunted = "Stunted (%)",
    Underweight = "Underweight (n)",
    Percent_Underweight = "Underweight (%)",
    Wasting = "Wasting (n)",
    Percent_Wasting = "Wasting (%)"
  ) %>%
  tab_spanner(
    label = "Stunting",
    id = "stunting_span",
    columns = c(Stunted, Percent_Stunted)
  ) %>%
  tab_spanner(
    label = "Underweight",
    id = "underweight_span",
    columns = c(Underweight, Percent_Underweight)
  ) %>%
  tab_spanner(
    label = "Wasting",
    id = "wasting_span",
    columns = c(Wasting, Percent_Wasting)
  ) %>%
  opt_row_striping()
