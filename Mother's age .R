# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)
library(readr)

# Upload data
Output <- read_csv("~/Desktop/malnutrition/Output.csv")

# Select and clean data
Data <- Output %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72, V012, V025) %>%
  na.omit() %>%
  filter(HW70 < 9990, HW71 < 9990, HW72 < 9990) %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),
    underweight = ifelse(HW71 < -200, 1, 0),
    wasting = ifelse(HW72 < -200, 1, 0),
    Mothers_age = case_when(
      V012 < 20 ~ "<20",
      V012 >= 20 & V012 <= 29 ~ "20-29",
      V012 >= 30 & V012 <= 49 ~ "30-49"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Mothers_age = factor(Mothers_age, levels = c("<20", "20-29", "30-49"))
  )

# Summarize indicators by Mother's Age and Place of Residence
summary_table <- Data %>%
  group_by(Mothers_age, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted),
    Underweight = sum(underweight),
    Wasting = sum(wasting)
  ) %>%
  mutate(
    Percent_Stunted = round((Stunted / Total_Children) * 100, 1),
    Percent_Underweight = round((Underweight / Total_Children) * 100, 1),
    Percent_Wasting = round((Wasting / Total_Children) * 100, 1)
  )

summary_table %>%
  select(Mothers_age, V025,
         Total_Children,
         Stunted, Percent_Stunted,
         Underweight, Percent_Underweight,
         Wasting, Percent_Wasting) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Summary by Mother's Age and Place of Residence"
  ) %>%
  cols_label(
    Mothers_age = "Mother's Age",
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
