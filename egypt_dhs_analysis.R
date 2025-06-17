---
  title: "for presentation"
author: "ABIOLA AJAYI"
date: "2024-11-19"
output:
  pdf_document: default
html_document: default
---
  
  ```{r}

## MOTHER'S AGE




# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Mother's Age (V012)
dataind_sub <- datainD %>%
  select(CASEID, V012, V025)  # Include V012 for Mother's Age

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )


# Create new variables for malnutrition indicators and categorize Mother's Age
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition
    Mothers_age = case_when(                     # Categorize Mother's Age
      V012 < 20 ~ "<20",
      V012 >= 20 & V012 <= 29 ~ "20-29",
      V012 >= 30 & V012 <= 49 ~ "30-49"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Mothers_age = factor(Mothers_age, levels = c("<20", "20-29", "30-49"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Mother's Age and Place of Residence
malnutrition_by_mothers_age_residence <- merged_data %>%
  group_by(Mothers_age, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Mothers_age, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Calculate total children and counts for each Mother's Age category in urban and rural areas for each indicator
calculate_counts <- function(data, category, residence, indicator) {
  total_children <- data %>%
    filter(Mothers_age == category, V025 == residence) %>%
    nrow()
  
  total_indicator <- data %>%
    filter(Mothers_age == category, V025 == residence, !!sym(indicator) == 1) %>%
    nrow()
  
  return(c(total_indicator, total_children))
}

# Create a helper function to calculate p-values for a given indicator
calculate_pvalues <- function(data, indicator) {
  # Urban
  urban_counts <- sapply(unique(data$Mothers_age), function(age) calculate_counts(data, age, "Urban", indicator))
  pvalue_urban <- prop.test(urban_counts[1, ], urban_counts[2, ])$p.value
  
  # Rural
  rural_counts <- sapply(unique(data$Mothers_age), function(age) calculate_counts(data, age, "Rural", indicator))
  pvalue_rural <- prop.test(rural_counts[1, ], rural_counts[2, ])$p.value
  
  return(c(pvalue_urban, pvalue_rural, pvalue_urban, pvalue_rural, pvalue_urban, pvalue_rural))
}

# Calculate p-values for each malnutrition indicator and region
pvalue_stunted <- calculate_pvalues(merged_data, "stunted")
pvalue_underweight <- calculate_pvalues(merged_data, "underweight")
pvalue_wasting <- calculate_pvalues(merged_data, "wasting")
pvalue_any_undernutrition <- calculate_pvalues(merged_data, "any_undernutrition")

# Create a data frame to hold p-values for each malnutrition indicator and region
pvalue_df <- data.frame(
  Mothers_age = c("<20", "<20", "20-29", "20-29", "30-49", "30-49"),
  V025 = c("Urban", "Rural", "Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = pvalue_stunted,
  Underweight_p_value = pvalue_underweight,
  Wasting_p_value = pvalue_wasting,
  Any_Undernutrition_p_value = pvalue_any_undernutrition
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_mothers_age_residence, pvalue_df, by = c("Mothers_age", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Mothers_age, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Mother's Age and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Mothers_age = "Mother's Age",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)




# Filter the data for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

# Perform the Chi-square test for Mother's Age and Place of Residence for stunted children
chisq_test_stunted <- chisq.test(stunted_data$Mothers_age, stunted_data$V025)

# Display the test results
print(chisq_test_stunted)


```

```{r}
# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Mothers_age, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V012, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Mothers_age, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V012, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Mothers_age, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V012, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)



```

```{r}
# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

# Create a contingency table for place of residence (V025)
stunted_table <- table(stunted_data$V025)

# Perform Chi-square test
chisq_test_stunted <- chisq.test(stunted_table)

# Calculate the total frequency (sum of urban and rural frequencies)
stunted_place_residence_sum <- sum(stunted_table)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

# Create a contingency table for place of residence (V025)
underweight_table <- table(underweight_data$V025)

# Perform Chi-square test
chisq_test_underweight <- chisq.test(underweight_table)

# Calculate the total frequency (sum of urban and rural frequencies)
underweight_place_residence_sum <- sum(underweight_table)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

# Create a contingency table for place of residence (V025)
wasting_table <- table(wasting_data$V025)

# Perform Chi-square test
chisq_test_wasting <- chisq.test(wasting_table)

# Calculate the total frequency (sum of urban and rural frequencies)
wasting_place_residence_sum <- sum(wasting_table)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Place_of_Residence_Sum = c(stunted_place_residence_sum, underweight_place_residence_sum, wasting_place_residence_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)

```
```{r}
## MOTHER'S EDUCATION



# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Include columns for stunting, underweight, and wasting

# Select specific columns from datainD dataset, include Mother's Education (V106)
dataind_sub <- datainD %>%
  select(CASEID, V106, V025)  # Include V106 for Mother's Education and V025 for Place of Residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Mother's Education
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Mother's Education
    Mother_education = case_when(
      V106 == 0 ~ "No Education",
      V106 == 1 ~ "Primary",
      V106 >= 2 ~ "Secondary and Above"
    ),
    
    # Convert Place of Residence to factor
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Mother_education = factor(Mother_education, levels = c("No Education", "Primary", "Secondary and Above"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Mother's Education and Place of Residence
malnutrition_by_mother_education_residence <- merged_data %>%
  group_by(Mother_education, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Mother_education, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, education_level, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Mother_education == education_level, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Mother_education == education_level, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for each malnutrition indicator and category (Urban/Rural)
calculate_indicator_pvalues <- function(data, indicator) {
  no_education_urban <- calculate_pvalue(data, "No Education", "Urban", indicator)
  primary_urban <- calculate_pvalue(data, "Primary", "Urban", indicator)
  secondary_above_urban <- calculate_pvalue(data, "Secondary and Above", "Urban", indicator)
  
  no_education_rural <- calculate_pvalue(data, "No Education", "Rural", indicator)
  primary_rural <- calculate_pvalue(data, "Primary", "Rural", indicator)
  secondary_above_rural <- calculate_pvalue(data, "Secondary and Above", "Rural", indicator)
  
  # Perform proportion tests for the given indicator
  pvalue_urban <- prop.test(c(no_education_urban[1], primary_urban[1], secondary_above_urban[1]), 
                            c(no_education_urban[2], primary_urban[2], secondary_above_urban[2]))
  
  pvalue_rural <- prop.test(c(no_education_rural[1], primary_rural[1], secondary_above_rural[1]), 
                            c(no_education_rural[2], primary_rural[2], secondary_above_rural[2]))
  
  return(c(pvalue_urban$p.value, pvalue_rural$p.value))
}

# Calculate p-values for each malnutrition indicator
pvalues_stunted <- calculate_indicator_pvalues(merged_data, "stunted")
pvalues_underweight <- calculate_indicator_pvalues(merged_data, "underweight")
pvalues_wasting <- calculate_indicator_pvalues(merged_data, "wasting")
pvalues_any_undernutrition <- calculate_indicator_pvalues(merged_data, "any_undernutrition")

# Create a data frame to hold all p-values
pvalue_df <- data.frame(
  Mother_education = c("No Education", "No Education", "Primary", "Primary", "Secondary and Above", "Secondary and Above"),
  V025 = c("Urban", "Rural", "Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalues_stunted[1], pvalues_stunted[2], pvalues_stunted[1], pvalues_stunted[2], pvalues_stunted[1], pvalues_stunted[2]),
  Underweight_p_value = c(pvalues_underweight[1], pvalues_underweight[2], pvalues_underweight[1], pvalues_underweight[2], pvalues_underweight[1], pvalues_underweight[2]),
  Wasting_p_value = c(pvalues_wasting[1], pvalues_wasting[2], pvalues_wasting[1], pvalues_wasting[2], pvalues_wasting[1], pvalues_wasting[2]),
  Any_Undernutrition_p_value = c(pvalues_any_undernutrition[1], pvalues_any_undernutrition[2], pvalues_any_undernutrition[1], pvalues_any_undernutrition[2], pvalues_any_undernutrition[1], pvalues_any_undernutrition[2])
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_mother_education_residence, pvalue_df, by = c("Mother_education", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Mother_education, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Mother's Education and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Mother_education = "Mother's Education",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values and "Any form of undernutrition"
print(malnutrition_gt_table_with_pvalues)


```

```{r}


# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Mother_education, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V106, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Mother_education, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V106, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Mother_education, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V106, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)

```
```{r}


## FATHER'S EDUCATION


# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Father's Education (V701)
dataind_sub <- datainD %>%
  select(CASEID, V701, V025)  # Include V701 for Father's Education

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Father's Education
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    Father_education = case_when(                # Categorize Father's Education
      V701 == 0 ~ "No Education",
      V701 == 1 ~ "Primary",
      V701 >= 2 ~ "Secondary and Above"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Father_education = factor(Father_education, levels = c("No Education", "Primary", "Secondary and Above"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Father's Education and Place of Residence
malnutrition_by_father_education_residence <- merged_data %>%
  group_by(Father_education, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100
  ) %>%
  select(Father_education, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting)

# Print the malnutrition_percentages table
print(malnutrition_by_father_education_residence)

# Use gt to create a formatted table for better visualization
malnutrition_gt_table <- malnutrition_by_father_education_residence %>%
  arrange(Father_education, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages by Father's Education and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting),
    decimals = 1
  ) %>%
  cols_label(
    Father_education = "Father's Education",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table
print(malnutrition_gt_table)




















# --- Stunted p-value Analysis by Father's Education ---

# Calculate total children for each Father's Education category in urban areas
total_children_urban_no_education <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Urban") %>%
  nrow()

print(total_children_urban_no_education)

total_children_urban_no_education_stunt <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Urban", stunted == 1) %>%
  nrow()

print(total_children_urban_no_education_stunt)


total_children_urban_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Urban") %>%
  nrow()

print(total_children_urban_primary)

total_children_stunted_urban_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Urban", stunted == 1) %>%
  nrow()

print(total_children_stunted_urban_primary)


total_children_urban_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Urban") %>%
  nrow()

print(total_children_urban_secondary_above)

total_children_stunted_urban_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Urban", stunted == 1) %>%
  nrow()

print(total_children_stunted_urban_secondary_above)

# Calculate the stunted proportions and p-value for urban areas based on Father's Education
stuntedp_urban <- c(total_children_urban_no_education_stunt, total_children_stunted_urban_primary, total_children_stunted_urban_secondary_above)
totalp_urban <- c(total_children_urban_no_education, total_children_urban_primary, total_children_urban_secondary_above)

# Perform proportion test for stunted in urban areas
pvalue_of_stunted_urban <- prop.test(stuntedp_urban, totalp_urban)
print(pvalue_of_stunted_urban)


# --- Calculate Stunted p-value for Rural Areas ---

# Calculate total children for each Father's Education category in rural areas
total_children_rural_no_education <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Rural") %>%
  nrow()

print(total_children_rural_no_education)

total_children_rural_no_education_stunt <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Rural", stunted == 1) %>%
  nrow()

print(total_children_rural_no_education_stunt)


total_children_rural_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Rural") %>%
  nrow()

print(total_children_rural_primary)

total_children_stunted_rural_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Rural", stunted == 1) %>%
  nrow()

print(total_children_stunted_rural_primary)


total_children_rural_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Rural") %>%
  nrow()

print(total_children_rural_secondary_above)

total_children_stunted_rural_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Rural", stunted == 1) %>%
  nrow()

print(total_children_stunted_rural_secondary_above)

# Calculate the stunted proportions and p-value for rural areas based on Father's Education
stuntedp_rural <- c(total_children_rural_no_education_stunt, total_children_stunted_rural_primary, total_children_stunted_rural_secondary_above)
totalp_rural <- c(total_children_rural_no_education, total_children_rural_primary, total_children_rural_secondary_above)

# Perform proportion test for stunted in rural areas
pvalue_of_stunted_rural <- prop.test(stuntedp_rural, totalp_rural)
print(pvalue_of_stunted_rural)
























# --- Underweight p-value Analysis by Father's Education ---

# Calculate total children for each Father's Education category in urban areas
total_children_urban_no_education <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Urban") %>%
  nrow()

print(total_children_urban_no_education)

total_children_urban_no_education_underweight <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Urban", underweight == 1) %>%
  nrow()

print(total_children_urban_no_education_underweight)


total_children_urban_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Urban") %>%
  nrow()

print(total_children_urban_primary)

total_children_underweight_urban_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Urban", underweight == 1) %>%
  nrow()

print(total_children_underweight_urban_primary)


total_children_urban_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Urban") %>%
  nrow()

print(total_children_urban_secondary_above)

total_children_underweight_urban_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Urban", underweight == 1) %>%
  nrow()

print(total_children_underweight_urban_secondary_above)

# Calculate the underweight proportions and p-value for urban areas based on Father's Education
underweightp_urban <- c(total_children_urban_no_education_underweight, total_children_underweight_urban_primary, total_children_underweight_urban_secondary_above)
totalp_urban <- c(total_children_urban_no_education, total_children_urban_primary, total_children_urban_secondary_above)

# Perform proportion test for underweight in urban areas
pvalue_of_underweight_urban <- prop.test(underweightp_urban, totalp_urban)
print(pvalue_of_underweight_urban)


# --- Calculate Underweight p-value for Rural Areas ---

# Calculate total children for each Father's Education category in rural areas
total_children_rural_no_education <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Rural") %>%
  nrow()

print(total_children_rural_no_education)

total_children_rural_no_education_underweight <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Rural", underweight == 1) %>%
  nrow()

print(total_children_rural_no_education_underweight)


total_children_rural_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Rural") %>%
  nrow()

print(total_children_rural_primary)

total_children_underweight_rural_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Rural", underweight == 1) %>%
  nrow()

print(total_children_underweight_rural_primary)


total_children_rural_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Rural") %>%
  nrow()

print(total_children_rural_secondary_above)

total_children_underweight_rural_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Rural", underweight == 1) %>%
  nrow()

print(total_children_underweight_rural_secondary_above)

# Calculate the underweight proportions and p-value for rural areas based on Father's Education
underweightp_rural <- c(total_children_rural_no_education_underweight, total_children_underweight_rural_primary, total_children_underweight_rural_secondary_above)
totalp_rural <- c(total_children_rural_no_education, total_children_rural_primary, total_children_rural_secondary_above)

# Perform proportion test for underweight in rural areas
pvalue_of_underweight_rural <- prop.test(underweightp_rural, totalp_rural)
print(pvalue_of_underweight_rural)






















# --- Wasting p-value Analysis by Father's Education ---

# Calculate total children for each Father's Education category in urban areas
total_children_urban_no_education <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Urban") %>%
  nrow()

print(total_children_urban_no_education)

total_children_urban_no_education_wasting <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Urban", wasting == 1) %>%
  nrow()

print(total_children_urban_no_education_wasting)


total_children_urban_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Urban") %>%
  nrow()

print(total_children_urban_primary)

total_children_wasting_urban_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Urban", wasting == 1) %>%
  nrow()

print(total_children_wasting_urban_primary)


total_children_urban_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Urban") %>%
  nrow()

print(total_children_urban_secondary_above)

total_children_wasting_urban_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Urban", wasting == 1) %>%
  nrow()

print(total_children_wasting_urban_secondary_above)

# Calculate the wasting proportions and p-value for urban areas based on Father's Education
wastingp_urban <- c(total_children_urban_no_education_wasting, total_children_wasting_urban_primary, total_children_wasting_urban_secondary_above)
totalp_urban <- c(total_children_urban_no_education, total_children_urban_primary, total_children_urban_secondary_above)

# Perform proportion test for wasting in urban areas
pvalue_of_wasting_urban <- prop.test(wastingp_urban, totalp_urban)
print(pvalue_of_wasting_urban)


# --- Calculate Wasting p-value for Rural Areas ---

# Calculate total children for each Father's Education category in rural areas
total_children_rural_no_education <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Rural") %>%
  nrow()

print(total_children_rural_no_education)

total_children_rural_no_education_wasting <- merged_data %>%
  filter(Father_education == "No Education", V025 == "Rural", wasting == 1) %>%
  nrow()

print(total_children_rural_no_education_wasting)


total_children_rural_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Rural") %>%
  nrow()

print(total_children_rural_primary)

total_children_wasting_rural_primary <- merged_data %>%
  filter(Father_education == "Primary", V025 == "Rural", wasting == 1) %>%
  nrow()

print(total_children_wasting_rural_primary)


total_children_rural_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Rural") %>%
  nrow()

print(total_children_rural_secondary_above)

total_children_wasting_rural_secondary_above <- merged_data %>%
  filter(Father_education == "Secondary and Above", V025 == "Rural", wasting == 1) %>%
  nrow()

print(total_children_wasting_rural_secondary_above)

# Calculate the wasting proportions and p-value for rural areas based on Father's Education
wastingp_rural <- c(total_children_rural_no_education_wasting, total_children_wasting_rural_primary, total_children_wasting_rural_secondary_above)
totalp_rural <- c(total_children_rural_no_education, total_children_rural_primary, total_children_rural_secondary_above)

# Perform proportion test for wasting in rural areas
pvalue_of_wasting_rural <- prop.test(wastingp_rural, totalp_rural)
print(pvalue_of_wasting_rural)










# Calculate and store p-values from the tests into variables
pvalue_stunted_urban <- pvalue_of_stunted_urban$p.value
pvalue_stunted_rural <- pvalue_of_stunted_rural$p.value

pvalue_underweight_urban <- pvalue_of_underweight_urban$p.value
pvalue_underweight_rural <- pvalue_of_underweight_rural$p.value

pvalue_wasting_urban <- pvalue_of_wasting_urban$p.value
pvalue_wasting_rural <- pvalue_of_wasting_rural$p.value

# Create a data frame to hold p-values for each malnutrition indicator and region
pvalue_df <- data.frame(
  Father_education = c("No Education", "No Education", "Primary", "Primary", "Secondary and Above", "Secondary and Above"),
  V025 = c("Urban", "Rural", "Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalue_stunted_urban, pvalue_stunted_rural, pvalue_stunted_urban, pvalue_stunted_rural, pvalue_stunted_urban, pvalue_stunted_rural),
  Underweight_p_value = c(pvalue_underweight_urban, pvalue_underweight_rural, pvalue_underweight_urban, pvalue_underweight_rural, pvalue_underweight_urban, pvalue_underweight_rural),
  Wasting_p_value = c(pvalue_wasting_urban, pvalue_wasting_rural, pvalue_wasting_urban, pvalue_wasting_rural, pvalue_wasting_urban, pvalue_wasting_rural)
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_father_education_residence, pvalue_df, by = c("Father_education", "V025"))

# Create a formatted gt table that includes p-values
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Father_education, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Father's Education and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Stunted_p_value, Underweight_p_value, Wasting_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Father_education = "Father's Education",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)













# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Father's Education (V701)
dataind_sub <- datainD %>%
  select(CASEID, V701, V025)  # Include V701 for Father's Education

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Father's Education
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    Father_education = case_when(                # Categorize Father's Education
      V701 == 0 ~ "No Education",
      V701 == 1 ~ "Primary",
      V701 >= 2 ~ "Secondary and Above"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Father_education = factor(Father_education, levels = c("No Education", "Primary", "Secondary and Above"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Father's Education and Place of Residence
malnutrition_by_father_education_residence <- merged_data %>%
  group_by(Father_education, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Father_education, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition_percentages table
print(malnutrition_by_father_education_residence)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, education_level, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Father_education == education_level, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Father_education == education_level, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for "Any form of undernutrition" in urban and rural areas
any_undernutrition_urban <- calculate_pvalue(merged_data, "No Education", "Urban", "any_undernutrition")
any_undernutrition_primary_urban <- calculate_pvalue(merged_data, "Primary", "Urban", "any_undernutrition")
any_undernutrition_secondary_urban <- calculate_pvalue(merged_data, "Secondary and Above", "Urban", "any_undernutrition")

any_undernutrition_rural <- calculate_pvalue(merged_data, "No Education", "Rural", "any_undernutrition")
any_undernutrition_primary_rural <- calculate_pvalue(merged_data, "Primary", "Rural", "any_undernutrition")
any_undernutrition_secondary_rural <- calculate_pvalue(merged_data, "Secondary and Above", "Rural", "any_undernutrition")

# Perform proportion tests for "Any form of undernutrition"
pvalue_of_any_undernutrition_urban <- prop.test(c(any_undernutrition_urban[1], any_undernutrition_primary_urban[1], any_undernutrition_secondary_urban[1]), 
                                                c(any_undernutrition_urban[2], any_undernutrition_primary_urban[2], any_undernutrition_secondary_urban[2]))

pvalue_of_any_undernutrition_rural <- prop.test(c(any_undernutrition_rural[1], any_undernutrition_primary_rural[1], any_undernutrition_secondary_rural[1]), 
                                                c(any_undernutrition_rural[2], any_undernutrition_primary_rural[2], any_undernutrition_secondary_rural[2]))

# Calculate and store p-values
pvalue_any_undernutrition_urban <- pvalue_of_any_undernutrition_urban$p.value
pvalue_any_undernutrition_rural <- pvalue_of_any_undernutrition_rural$p.value

# Create a data frame to hold all p-values, including "Any form of undernutrition"
pvalue_df <- data.frame(
  Father_education = c("No Education", "No Education", "Primary", "Primary", "Secondary and Above", "Secondary and Above"),
  V025 = c("Urban", "Rural", "Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalue_of_stunted_urban$p.value, pvalue_of_stunted_rural$p.value, pvalue_of_stunted_urban$p.value, pvalue_of_stunted_rural$p.value, pvalue_of_stunted_urban$p.value, pvalue_of_stunted_rural$p.value),
  Underweight_p_value = c(pvalue_of_underweight_urban$p.value, pvalue_of_underweight_rural$p.value, pvalue_of_underweight_urban$p.value, pvalue_of_underweight_rural$p.value, pvalue_of_underweight_urban$p.value, pvalue_of_underweight_rural$p.value),
  Wasting_p_value = c(pvalue_of_wasting_urban$p.value, pvalue_of_wasting_rural$p.value, pvalue_of_wasting_urban$p.value, pvalue_of_wasting_rural$p.value, pvalue_of_wasting_urban$p.value, pvalue_of_wasting_rural$p.value),
  Any_Undernutrition_p_value = c(pvalue_any_undernutrition_urban, pvalue_any_undernutrition_rural, pvalue_any_undernutrition_urban, pvalue_any_undernutrition_rural, pvalue_any_undernutrition_urban, pvalue_any_undernutrition_rural)
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_father_education_residence, pvalue_df, by = c("Father_education", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Father_education, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Father's Education and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Father_education = "Father's Education",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values and "Any form of undernutrition"
print(malnutrition_gt_table_with_pvalues)


```
```{r}


# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Father_education, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V701, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Father_education, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V701, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Father_education, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V701, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)



```

```{r}

## MOTHER'S WORKING STATUS

# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Include columns for stunting, underweight, and wasting

# Select specific columns from datainD dataset, include Mother's Working Status (V714)
dataind_sub <- datainD %>%
  select(CASEID, V714, V025)  # Include V714 for Mother's Working Status and V025 for Place of Residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Mother's Working Status
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Mother's Working Status
    Mothers_working_status = case_when(
      V714 == 1 ~ "Working",
      V714 == 0 ~ "Not Working"
    ),
    
    # Convert Place of Residence to factor
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Mothers_working_status = factor(Mothers_working_status, levels = c("Not Working", "Working"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Mother's Working Status and Place of Residence
malnutrition_by_working_status_residence <- merged_data %>%
  group_by(Mothers_working_status, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Mothers_working_status, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, working_status, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Mothers_working_status == working_status, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Mothers_working_status == working_status, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for each malnutrition indicator and category (Urban/Rural)
calculate_indicator_pvalues <- function(data, indicator) {
  not_working_urban <- calculate_pvalue(data, "Not Working", "Urban", indicator)
  working_urban <- calculate_pvalue(data, "Working", "Urban", indicator)
  
  not_working_rural <- calculate_pvalue(data, "Not Working", "Rural", indicator)
  working_rural <- calculate_pvalue(data, "Working", "Rural", indicator)
  
  # Perform proportion tests for the given indicator
  pvalue_urban <- prop.test(c(not_working_urban[1], working_urban[1]), 
                            c(not_working_urban[2], working_urban[2]))
  
  pvalue_rural <- prop.test(c(not_working_rural[1], working_rural[1]), 
                            c(not_working_rural[2], working_rural[2]))
  
  return(c(pvalue_urban$p.value, pvalue_rural$p.value))
}

# Calculate p-values for each malnutrition indicator
pvalues_stunted <- calculate_indicator_pvalues(merged_data, "stunted")
pvalues_underweight <- calculate_indicator_pvalues(merged_data, "underweight")
pvalues_wasting <- calculate_indicator_pvalues(merged_data, "wasting")
pvalues_any_undernutrition <- calculate_indicator_pvalues(merged_data, "any_undernutrition")

# Create a data frame to hold all p-values
pvalue_df <- data.frame(
  Mothers_working_status = c("Not Working", "Not Working", "Working", "Working"),
  V025 = c("Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalues_stunted[1], pvalues_stunted[2], pvalues_stunted[1], pvalues_stunted[2]),
  Underweight_p_value = c(pvalues_underweight[1], pvalues_underweight[2], pvalues_underweight[1], pvalues_underweight[2]),
  Wasting_p_value = c(pvalues_wasting[1], pvalues_wasting[2], pvalues_wasting[1], pvalues_wasting[2]),
  Any_Undernutrition_p_value = c(pvalues_any_undernutrition[1], pvalues_any_undernutrition[2], pvalues_any_undernutrition[1], pvalues_any_undernutrition[2])
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_working_status_residence, pvalue_df, by = c("Mothers_working_status", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Mothers_working_status, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Mother's Working Status and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Mothers_working_status = "Mother's Working Status",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values and "Any form of undernutrition"
print(malnutrition_gt_table_with_pvalues)


```
```{r}




# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Mothers_working_status, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V714, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Mothers_working_status, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V714, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Mothers_working_status, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V714, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)




```

```{r}


## FATHER'S OCCUPATION






# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Father's Occupation (V704)
dataind_sub <- datainD %>%
  select(CASEID, V704, V025)  # Include V704 for Father's Occupation

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Father's Occupation
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    Fathers_occupation = case_when(              # Categorize Father's Occupation
      V704 >= 1 & V704 <= 54 ~ "Service/Business",
      V704 >= 55 & V704 <= 96 ~ "Farmers/Others"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Fathers_occupation = factor(Fathers_occupation, levels = c("Service/Business", "Farmers/Others"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Father's Occupation and Place of Residence
malnutrition_by_fathers_occupation_residence <- merged_data %>%
  group_by(Fathers_occupation, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100
  ) %>%
  select(Fathers_occupation, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting)

# Print the malnutrition_percentages table
print(malnutrition_by_fathers_occupation_residence)

# Use gt to create a formatted table for better visualization
malnutrition_gt_table <- malnutrition_by_fathers_occupation_residence %>%
  arrange(Fathers_occupation, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages by Father's Occupation and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting),
    decimals = 1
  ) %>%
  cols_label(
    Fathers_occupation = "Father's Occupation",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table
print(malnutrition_gt_table)







# --- stunted pvale ---










# Calculate total children for each Father's Occupation category in urban areas
total_children_urban_service <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Urban") %>%
  nrow()

print(total_children_urban_service)

total_children_urban_service_stunt <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Urban", stunted == 1) %>%
  nrow()

print(total_children_urban_service_stunt)


total_children_urban_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Urban") %>%
  nrow()

print(total_children_urban_farmers)

total_children_stunted_urban_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Urban", stunted == 1) %>%
  nrow()

print(total_children_stunted_urban_farmers)

# Calculate the stunted proportions and p-value for urban areas based on Father's Occupation
stuntedp_urban <- c(total_children_urban_service_stunt, total_children_stunted_urban_farmers)
totalp_urban <- c(total_children_urban_service, total_children_urban_farmers)

# Perform proportion test for stunted in urban areas
pvalue_of_stunted_urban <- prop.test(stuntedp_urban, totalp_urban)
print(pvalue_of_stunted_urban)


# --- Calculate Stunted p-value for Rural Areas ---

# Calculate total children for each Father's Occupation category in rural areas
total_children_rural_service <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Rural") %>%
  nrow()

print(total_children_rural_service)

total_children_rural_service_stunt <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Rural", stunted == 1) %>%
  nrow()

print(total_children_rural_service_stunt)


total_children_rural_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Rural") %>%
  nrow()

print(total_children_rural_farmers)

total_children_stunted_rural_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Rural", stunted == 1) %>%
  nrow()

print(total_children_stunted_rural_farmers)

# Calculate the stunted proportions and p-value for rural areas based on Father's Occupation
stuntedp_rural <- c(total_children_rural_service_stunt, total_children_stunted_rural_farmers)
totalp_rural <- c(total_children_rural_service, total_children_rural_farmers)

# Perform proportion test for stunted in rural areas
pvalue_of_stunted_rural <- prop.test(stuntedp_rural, totalp_rural)
print(pvalue_of_stunted_rural)























# ---underweight pvale ---




# Calculate total children for each Father's Occupation category in urban areas
total_children_urban_service <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Urban") %>%
  nrow()

print(total_children_urban_service)

total_children_urban_service_underweight <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Urban", underweight == 1) %>%
  nrow()

print(total_children_urban_service_underweight)


total_children_urban_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Urban") %>%
  nrow()

print(total_children_urban_farmers)

total_children_underweight_urban_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Urban", underweight == 1) %>%
  nrow()

print(total_children_underweight_urban_farmers)

# Calculate the underweight proportions and p-value for urban areas based on Father's Occupation
underweightp_urban <- c(total_children_urban_service_underweight, total_children_underweight_urban_farmers)
totalp_urban <- c(total_children_urban_service, total_children_urban_farmers)

# Perform proportion test for underweight in urban areas
pvalue_of_underweight_urban <- prop.test(underweightp_urban, totalp_urban)
print(pvalue_of_underweight_urban)


# --- Calculate Underweight p-value for Rural Areas ---

# Calculate total children for each Father's Occupation category in rural areas
total_children_rural_service <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Rural") %>%
  nrow()

print(total_children_rural_service)

total_children_rural_service_underweight <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Rural", underweight == 1) %>%
  nrow()

print(total_children_rural_service_underweight)


total_children_rural_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Rural") %>%
  nrow()

print(total_children_rural_farmers)

total_children_underweight_rural_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Rural", underweight == 1) %>%
  nrow()

print(total_children_underweight_rural_farmers)

# Calculate the underweight proportions and p-value for rural areas based on Father's Occupation
underweightp_rural <- c(total_children_rural_service_underweight, total_children_underweight_rural_farmers)
totalp_rural <- c(total_children_rural_service, total_children_rural_farmers)

# Perform proportion test for underweight in rural areas
pvalue_of_underweight_rural <- prop.test(underweightp_rural, totalp_rural)
print(pvalue_of_underweight_rural)













# ---wasting pvale ---






# Calculate total children for each Father's Occupation category in urban areas
total_children_urban_service <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Urban") %>%
  nrow()

print(total_children_urban_service)

total_children_urban_service_wasting <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Urban", wasting == 1) %>%
  nrow()

print(total_children_urban_service_wasting)


total_children_urban_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Urban") %>%
  nrow()

print(total_children_urban_farmers)

total_children_wasting_urban_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Urban", wasting == 1) %>%
  nrow()

print(total_children_wasting_urban_farmers)

# Calculate the wasting proportions and p-value for urban areas based on Father's Occupation
wastingp_urban <- c(total_children_urban_service_wasting, total_children_wasting_urban_farmers)
totalp_urban <- c(total_children_urban_service, total_children_urban_farmers)

# Perform proportion test for wasting in urban areas
pvalue_of_wasting_urban <- prop.test(wastingp_urban, totalp_urban)
print(pvalue_of_wasting_urban)


# --- Calculate Wasting p-value for Rural Areas ---

# Calculate total children for each Father's Occupation category in rural areas
total_children_rural_service <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Rural") %>%
  nrow()

print(total_children_rural_service)

total_children_rural_service_wasting <- merged_data %>%
  filter(Fathers_occupation == "Service/Business", V025 == "Rural", wasting == 1) %>%
  nrow()

print(total_children_rural_service_wasting)


total_children_rural_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Rural") %>%
  nrow()

print(total_children_rural_farmers)

total_children_wasting_rural_farmers <- merged_data %>%
  filter(Fathers_occupation == "Farmers/Others", V025 == "Rural", wasting == 1) %>%
  nrow()

print(total_children_wasting_rural_farmers)

# Calculate the wasting proportions and p-value for rural areas based on Father's Occupation
wastingp_rural <- c(total_children_rural_service_wasting, total_children_wasting_rural_farmers)
totalp_rural <- c(total_children_rural_service, total_children_rural_farmers)

# Perform proportion test for wasting in rural areas
pvalue_of_wasting_rural <- prop.test(wastingp_rural, totalp_rural)
print(pvalue_of_wasting_rural)














# Store the p-values from the tests into variables
pvalue_stunted_urban <- pvalue_of_stunted_urban$p.value
pvalue_stunted_rural <- pvalue_of_stunted_rural$p.value

pvalue_underweight_urban <- pvalue_of_underweight_urban$p.value
pvalue_underweight_rural <- pvalue_of_underweight_rural$p.value

pvalue_wasting_urban <- pvalue_of_wasting_urban$p.value
pvalue_wasting_rural <- pvalue_of_wasting_rural$p.value

# Create a data frame to hold p-values for each malnutrition indicator and region
pvalue_df <- data.frame(
  Fathers_occupation = c("Service/Business", "Service/Business", "Farmers/Others", "Farmers/Others"),
  V025 = c("Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalue_stunted_urban, pvalue_stunted_rural, pvalue_stunted_urban, pvalue_stunted_rural),
  Underweight_p_value = c(pvalue_underweight_urban, pvalue_underweight_rural, pvalue_underweight_urban, pvalue_underweight_rural),
  Wasting_p_value = c(pvalue_wasting_urban, pvalue_wasting_rural, pvalue_wasting_urban, pvalue_wasting_rural)
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_fathers_occupation_residence, pvalue_df, by = c("Fathers_occupation", "V025"))

# Create a formatted gt table that includes p-values
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Fathers_occupation, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Father's Occupation and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Stunted_p_value, Underweight_p_value, Wasting_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Fathers_occupation = "Father's Occupation",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)


# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Father's Occupation (V704)
dataind_sub <- datainD %>%
  select(CASEID, V704, V025)  # Include V704 for Father's Occupation

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Father's Occupation
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    Fathers_occupation = case_when(              # Categorize Father's Occupation
      V704 >= 1 & V704 <= 54 ~ "Service/Business",
      V704 >= 55 & V704 <= 96 ~ "Farmers/Others"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Fathers_occupation = factor(Fathers_occupation, levels = c("Service/Business", "Farmers/Others"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Father's Occupation and Place of Residence
malnutrition_by_fathers_occupation_residence <- merged_data %>%
  group_by(Fathers_occupation, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Fathers_occupation, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, occupation_level, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Fathers_occupation == occupation_level, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Fathers_occupation == occupation_level, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for each malnutrition indicator and category (Urban/Rural)
calculate_indicator_pvalues <- function(data, indicator) {
  service_urban <- calculate_pvalue(data, "Service/Business", "Urban", indicator)
  farmers_urban <- calculate_pvalue(data, "Farmers/Others", "Urban", indicator)
  
  service_rural <- calculate_pvalue(data, "Service/Business", "Rural", indicator)
  farmers_rural <- calculate_pvalue(data, "Farmers/Others", "Rural", indicator)
  
  # Perform proportion tests for the given indicator
  pvalue_urban <- prop.test(c(service_urban[1], farmers_urban[1]), c(service_urban[2], farmers_urban[2]))
  pvalue_rural <- prop.test(c(service_rural[1], farmers_rural[1]), c(service_rural[2], farmers_rural[2]))
  
  return(c(pvalue_urban$p.value, pvalue_rural$p.value))
}

# Calculate p-values for each malnutrition indicator
pvalues_stunted <- calculate_indicator_pvalues(merged_data, "stunted")
pvalues_underweight <- calculate_indicator_pvalues(merged_data, "underweight")
pvalues_wasting <- calculate_indicator_pvalues(merged_data, "wasting")
pvalues_any_undernutrition <- calculate_indicator_pvalues(merged_data, "any_undernutrition")

# Create a data frame to hold all p-values
pvalue_df <- data.frame(
  Fathers_occupation = c("Service/Business", "Service/Business", "Farmers/Others", "Farmers/Others"),
  V025 = c("Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalues_stunted[1], pvalues_stunted[2], pvalues_stunted[1], pvalues_stunted[2]),
  Underweight_p_value = c(pvalues_underweight[1], pvalues_underweight[2], pvalues_underweight[1], pvalues_underweight[2]),
  Wasting_p_value = c(pvalues_wasting[1], pvalues_wasting[2], pvalues_wasting[1], pvalues_wasting[2]),
  Any_Undernutrition_p_value = c(pvalues_any_undernutrition[1], pvalues_any_undernutrition[2], pvalues_any_undernutrition[1], pvalues_any_undernutrition[2])
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_fathers_occupation_residence, pvalue_df, by = c("Fathers_occupation", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Fathers_occupation, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Father's Occupation and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Fathers_occupation = "Father's Occupation",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values and "Any form of undernutrition"
print(malnutrition_gt_table_with_pvalues)

```




```{r}



# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Fathers_occupation, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V704, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Fathers_occupation, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V704, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Fathers_occupation, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V704, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)






```
```{r}

## ACCESS TO MEDIA




# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include variables related to access to media (V157, V158, V159)
dataind_sub <- datainD %>%
  select(CASEID, V157, V158, V159, V025)  # Include V157, V158, V159 for Access to Media

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Access to Media
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Access to Media based on V157, V158, V159 values
    Access_to_media = case_when(
      (V157 >= 1 & V157 <= 3) | (V158 >= 1 & V158 <= 3) | (V159 >= 1 & V159 <= 3) ~ "Have access",
      V157 == 0 & V158 == 0 & V159 == 0 ~ "No access"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Access_to_media = factor(Access_to_media, levels = c("Have access", "No access"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Access to Media and Place of Residence
malnutrition_by_access_to_media_residence <- merged_data %>%
  group_by(Access_to_media, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Access_to_media, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition percentages table
print(malnutrition_by_access_to_media_residence)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, access_level, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Access_to_media == access_level, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Access_to_media == access_level, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for each malnutrition indicator (Stunted, Underweight, Wasting, Any form of undernutrition)
pvalue_list <- list()
indicators <- c("stunted", "underweight", "wasting", "any_undernutrition")

# Loop through indicators and calculate p-values for Urban and Rural areas
for (indicator in indicators) {
  # Urban
  urban_have_access <- calculate_pvalue(merged_data, "Have access", "Urban", indicator)
  urban_no_access <- calculate_pvalue(merged_data, "No access", "Urban", indicator)
  
  pvalue_urban <- prop.test(c(urban_have_access[1], urban_no_access[1]), 
                            c(urban_have_access[2], urban_no_access[2]))$p.value
  
  # Rural
  rural_have_access <- calculate_pvalue(merged_data, "Have access", "Rural", indicator)
  rural_no_access <- calculate_pvalue(merged_data, "No access", "Rural", indicator)
  
  pvalue_rural <- prop.test(c(rural_have_access[1], rural_no_access[1]), 
                            c(rural_have_access[2], rural_no_access[2]))$p.value
  
  # Store p-values in the list
  pvalue_list[[indicator]] <- c(pvalue_urban, pvalue_rural, pvalue_urban, pvalue_rural)
}

# Create a data frame to hold all p-values for each indicator
pvalue_df <- data.frame(
  Access_to_media = c("Have access", "Have access", "No access", "No access"),
  V025 = c("Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = pvalue_list$stunted,
  Underweight_p_value = pvalue_list$underweight,
  Wasting_p_value = pvalue_list$wasting,
  Any_Undernutrition_p_value = pvalue_list$any_undernutrition
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_access_to_media_residence, pvalue_df, by = c("Access_to_media", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Access_to_media, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Access to Media and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Access_to_media = "Access to Media",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values for all malnutrition indicators
print(malnutrition_gt_table_with_pvalues)

```
```{r}
# Load required library
library(dplyr)

# Create a new column for access to media as a sum of V157, V158, and V159
merged_data <- merged_data %>%
  mutate(All_media = V157 + V158 + V159)

# Filter the data for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

# Perform the Chi-square test for Access to Media and Place of Residence
chisq_test_stunted <- chisq.test(stunted_data$All_media, stunted_data$V025)

# Summation of All Media for stunted children
stunted_age_sum <- sum(stunted_data$All_media, na.rm = TRUE)


# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$All_media, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$All_media, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$All_media, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$All_media, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)





```




```{r}

# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Wealth Index (V190) and place of residence (V025)
dataind_sub <- datainD %>%
  select(CASEID, V190, V025)  # Include V190 for Wealth Index and V025 for residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Wealth Index
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition
    
    # Categorize Wealth Index based on V190 values
    Wealth_index = case_when(
      V190 == 1 | V190 == 2 ~ "Poor",
      V190 == 3 ~ "Middle",
      V190 == 4 | V190 == 5 ~ "Rich"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Wealth_index = factor(Wealth_index, levels = c("Poor", "Middle", "Rich"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Wealth Index and Place of Residence
malnutrition_by_wealth_residence <- merged_data %>%
  group_by(Wealth_index, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Wealth_index, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition percentages table
print(malnutrition_by_wealth_residence)

# --- Stunted p-value Analysis by Wealth Index ---

# Calculate total children for each Wealth Index category in urban areas
total_children_urban_poor <- merged_data %>%
  filter(Wealth_index == "Poor", V025 == "Urban") %>%
  nrow()

total_children_urban_poor_stunt <- merged_data %>%
  filter(Wealth_index == "Poor", V025 == "Urban", stunted == 1) %>%
  nrow()

total_children_urban_middle <- merged_data %>%
  filter(Wealth_index == "Middle", V025 == "Urban") %>%
  nrow()

total_children_stunted_urban_middle <- merged_data %>%
  filter(Wealth_index == "Middle", V025 == "Urban", stunted == 1) %>%
  nrow()

total_children_urban_rich <- merged_data %>%
  filter(Wealth_index == "Rich", V025 == "Urban") %>%
  nrow()

total_children_stunted_urban_rich <- merged_data %>%
  filter(Wealth_index == "Rich", V025 == "Urban", stunted == 1) %>%
  nrow()

# Calculate the stunted proportions and p-value for urban areas based on Wealth Index
stuntedp_urban <- c(total_children_urban_poor_stunt, total_children_stunted_urban_middle, total_children_stunted_urban_rich)
totalp_urban <- c(total_children_urban_poor, total_children_urban_middle, total_children_urban_rich)

# Perform proportion test for stunted in urban areas
pvalue_of_stunted_urban <- prop.test(stuntedp_urban, totalp_urban)

# --- Calculate Stunted p-value for Rural Areas ---

# Calculate total children for each Wealth Index category in rural areas
total_children_rural_poor <- merged_data %>%
  filter(Wealth_index == "Poor", V025 == "Rural") %>%
  nrow()

total_children_rural_poor_stunt <- merged_data %>%
  filter(Wealth_index == "Poor", V025 == "Rural", stunted == 1) %>%
  nrow()

total_children_rural_middle <- merged_data %>%
  filter(Wealth_index == "Middle", V025 == "Rural") %>%
  nrow()

total_children_stunted_rural_middle <- merged_data %>%
  filter(Wealth_index == "Middle", V025 == "Rural", stunted == 1) %>%
  nrow()

total_children_rural_rich <- merged_data %>%
  filter(Wealth_index == "Rich", V025 == "Rural") %>%
  nrow()

total_children_stunted_rural_rich <- merged_data %>%
  filter(Wealth_index == "Rich", V025 == "Rural", stunted == 1) %>%
  nrow()

# Calculate the stunted proportions and p-value for rural areas based on Wealth Index
stuntedp_rural <- c(total_children_rural_poor_stunt, total_children_stunted_rural_middle, total_children_stunted_rural_rich)
totalp_rural <- c(total_children_rural_poor, total_children_rural_middle, total_children_rural_rich)

# Perform proportion test for stunted in rural areas
pvalue_of_stunted_rural <- prop.test(stuntedp_rural, totalp_rural)

# --- Repeat similar steps for `underweight`, `wasting`, and `any_undernutrition` based on Wealth Index ---
# Calculate p-values for underweight and wasting in both urban and rural areas.
# Calculate p-values for "Any form of undernutrition" based on Wealth Index and Residence.

# Calculate total children with any undernutrition for each Wealth Index category
total_children_urban_poor_any <- merged_data %>%
  filter(Wealth_index == "Poor", V025 == "Urban", any_undernutrition == 1) %>%
  nrow()

total_children_urban_middle_any <- merged_data %>%
  filter(Wealth_index == "Middle", V025 == "Urban", any_undernutrition == 1) %>%
  nrow()

total_children_urban_rich_any <- merged_data %>%
  filter(Wealth_index == "Rich", V025 == "Urban", any_undernutrition == 1) %>%
  nrow()

total_children_rural_poor_any <- merged_data %>%
  filter(Wealth_index == "Poor", V025 == "Rural", any_undernutrition == 1) %>%
  nrow()

total_children_rural_middle_any <- merged_data %>%
  filter(Wealth_index == "Middle", V025 == "Rural", any_undernutrition == 1) %>%
  nrow()

total_children_rural_rich_any <- merged_data %>%
  filter(Wealth_index == "Rich", V025 == "Rural", any_undernutrition == 1) %>%
  nrow()

# Proportion tests for any undernutrition in both urban and rural areas
pvalue_any_urban <- prop.test(c(total_children_urban_poor_any, total_children_urban_middle_any, total_children_urban_rich_any), 
                              c(total_children_urban_poor, total_children_urban_middle, total_children_urban_rich))$p.value

pvalue_any_rural <- prop.test(c(total_children_rural_poor_any, total_children_rural_middle_any, total_children_rural_rich_any), 
                              c(total_children_rural_poor, total_children_rural_middle, total_children_rural_rich))$p.value

# Create a data frame to hold p-values for each malnutrition indicator and region
pvalue_df <- data.frame(
  Wealth_index = c("Poor", "Poor", "Middle", "Middle", "Rich", "Rich"),
  V025 = c("Urban", "Rural", "Urban", "Rural", "Urban", "Rural"),
  Stunted_p_value = c(pvalue_of_stunted_urban$p.value, pvalue_of_stunted_rural$p.value, pvalue_of_stunted_urban$p.value, pvalue_of_stunted_rural$p.value, pvalue_of_stunted_urban$p.value, pvalue_of_stunted_rural$p.value),
  Underweight_p_value = c(pvalue_of_underweight_urban$p.value, pvalue_of_underweight_rural$p.value, pvalue_of_underweight_urban$p.value, pvalue_of_underweight_rural$p.value, pvalue_of_underweight_urban$p.value, pvalue_of_underweight_rural$p.value),
  Wasting_p_value = c(pvalue_of_wasting_urban$p.value, pvalue_of_wasting_rural$p.value, pvalue_of_wasting_urban$p.value, pvalue_of_wasting_rural$p.value, pvalue_of_wasting_urban$p.value, pvalue_of_wasting_rural$p.value),
  Any_Undernutrition_p_value = c(pvalue_any_urban, pvalue_any_rural, pvalue_any_urban, pvalue_any_rural, pvalue_any_urban, pvalue_any_rural)
)

# Merge the summary table with the p-value table
malnutrition_with_pvalues <- left_join(malnutrition_by_wealth_residence, pvalue_df, by = c("Wealth_index", "V025"))

# Create formatted `gt` table including p-values
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Wealth_index, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Wealth Index and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Wealth_index = "Wealth Index",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)



```
```{r}



# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Wealth_index, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V190, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Wealth_index, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V190, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Wealth_index, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V190, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)



```

```{r}
## Age at First Marriage




# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Age at First Marriage (V511)
dataind_sub <- datainD %>%
  select(CASEID, V511, V025)  # Include V511 for Age at First Marriage, V025 for Residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Age at First Marriage
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Age at First Marriage based on V511 values
    Age_at_first_marriage = case_when(
      V511 <= 15 ~ "<=15",
      V511 >= 16 & V511 <= 19 ~ "16-19",
      V511 >= 20 ~ "20 and above"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Age_at_first_marriage = factor(Age_at_first_marriage, levels = c("<=15", "16-19", "20 and above"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Age at First Marriage and Place of Residence
malnutrition_by_age_marriage_residence <- merged_data %>%
  group_by(Age_at_first_marriage, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Age_at_first_marriage, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition_percentages table
print(malnutrition_by_age_marriage_residence)

# Define a helper function to calculate the proportion test p-values
calculate_pvalue <- function(data, age_level, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Age_at_first_marriage == age_level, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Age_at_first_marriage == age_level, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for each malnutrition indicator in urban and rural areas
malnutrition_indicators <- c("stunted", "underweight", "wasting", "any_undernutrition")
pvalues <- list()

for (indicator in malnutrition_indicators) {
  urban_15 <- calculate_pvalue(merged_data, "<=15", "Urban", indicator)
  urban_16_19 <- calculate_pvalue(merged_data, "16-19", "Urban", indicator)
  urban_20_above <- calculate_pvalue(merged_data, "20 and above", "Urban", indicator)
  
  rural_15 <- calculate_pvalue(merged_data, "<=15", "Rural", indicator)
  rural_16_19 <- calculate_pvalue(merged_data, "16-19", "Rural", indicator)
  rural_20_above <- calculate_pvalue(merged_data, "20 and above", "Rural", indicator)
  
  pvalue_urban <- prop.test(c(urban_15[1], urban_16_19[1], urban_20_above[1]), 
                            c(urban_15[2], urban_16_19[2], urban_20_above[2]))$p.value
  pvalue_rural <- prop.test(c(rural_15[1], rural_16_19[1], rural_20_above[1]), 
                            c(rural_15[2], rural_16_19[2], rural_20_above[2]))$p.value
  
  pvalues[[indicator]] <- list(urban = pvalue_urban, rural = pvalue_rural)
}

# Create a data frame to hold p-values for each malnutrition indicator
pvalue_df <- data.frame(
  Age_at_first_marriage = rep(c("<=15", "16-19", "20 and above"), each = 2),
  V025 = rep(c("Urban", "Rural"), 3),
  Stunted_p_value = rep(c(pvalues$stunted$urban, pvalues$stunted$rural), 3),
  Underweight_p_value = rep(c(pvalues$underweight$urban, pvalues$underweight$rural), 3),
  Wasting_p_value = rep(c(pvalues$wasting$urban, pvalues$wasting$rural), 3),
  Any_Undernutrition_p_value = rep(c(pvalues$any_undernutrition$urban, pvalues$any_undernutrition$rural), 3)
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_age_marriage_residence, pvalue_df, by = c("Age_at_first_marriage", "V025"))

# Create formatted gt table including p-values for all indicators
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Age_at_first_marriage, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Age at First Marriage and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, 
                Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Age_at_first_marriage = "Age at First Marriage",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)

```


```{r}




# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Age_at_first_marriage, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V511, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Age_at_first_marriage, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V511, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Age_at_first_marriage, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V511, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)




```





```{r}
# Age at first_birth

# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Age at First Marriage (V511)
dataind_sub <- datainD %>%
  select(CASEID, V212, V025)  # Include V212 for Age at First Birth, V025 for Residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Age at First Birth
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Age at First birth based on V212 values
    Age_at_first_birth = case_when(
      V212 < 15 ~ "<=15",
      V212 >= 15 & V212 <= 19 ~ "15-19",
      V212 >= 20 & V212 <= 45 ~ "20-45"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Age_at_first_birth = factor(Age_at_first_birth, levels = c("<=15", "15-19", "20-45"))
  )

# View how V212 is categorized
print(table(merged_data$V212, merged_data$Age_at_first_birth))

# Check the distribution of V212
print(table(merged_data$V212))

# Calculate unweighted percentages for each malnutrition indicator by Age at First birth and Place of Residence
malnutrition_by_age_birth_residence <- merged_data %>%
  group_by(Age_at_first_birth, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Age_at_first_birth, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition_percentages table
print(malnutrition_by_age_birth_residence)

# Define a helper function to calculate the proportion test p-values
calculate_pvalue <- function(data, age_level, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Age_at_first_birth == age_level, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Age_at_first_birth == age_level, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for each malnutrition indicator in urban and rural areas
malnutrition_indicators <- c("stunted", "underweight", "wasting", "any_undernutrition")
pvalues <- list()

for (indicator in malnutrition_indicators) {
  urban_15 <- calculate_pvalue(merged_data, "<=15", "Urban", indicator)
  urban_16_19 <- calculate_pvalue(merged_data, "15-19", "Urban", indicator)
  urban_20_above <- calculate_pvalue(merged_data, "20-45", "Urban", indicator)
  
  rural_15 <- calculate_pvalue(merged_data, "<=15", "Rural", indicator)
  rural_16_19 <- calculate_pvalue(merged_data, "15-19", "Rural", indicator)
  rural_20_above <- calculate_pvalue(merged_data, "20-45", "Rural", indicator)
  
  pvalue_urban <- prop.test(c(urban_15[1], urban_16_19[1], urban_20_above[1]), 
                            c(urban_15[2], urban_16_19[2], urban_20_above[2]))$p.value
  pvalue_rural <- prop.test(c(rural_15[1], rural_16_19[1], rural_20_above[1]), 
                            c(rural_15[2], rural_16_19[2], rural_20_above[2]))$p.value
  
  pvalues[[indicator]] <- c(pvalue_urban, pvalue_rural)
}

# Create a data frame to hold p-values for each malnutrition indicator
pvalue_df <- data.frame(
  Age_at_first_birth = rep(c("<=15", "15-19", "20-45"), each = 2),
  V025 = rep(c("Urban", "Rural"), 3),
  Stunted_p_value = rep(pvalues$stunted, each = 1),
  Underweight_p_value = rep(pvalues$underweight, each = 1),
  Wasting_p_value = rep(pvalues$wasting, each = 1),
  Any_Undernutrition_p_value = rep(pvalues$any_undernutrition, each = 1)
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_age_birth_residence, pvalue_df, by = c("Age_at_first_birth", "V025"))

# Create formatted gt table including p-values for all indicators
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Age_at_first_birth, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Age at First Birth and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, 
                Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Age_at_first_birth = "Age at First Birth",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)


```



```{r}





# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Age_at_first_birth, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V212, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Age_at_first_birth, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V212, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Age_at_first_birth, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V212, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)



```

```{r}
# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns from databir
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%  # Filter children 59 months or younger
  select(CASEID, HW1, HW70, HW71, HW72)  # Selecting anthropometric indicators

# Select specific columns from datainD, include Mother's BMI (V445), and area type (V025)
dataind_sub <- datainD %>%
  select(CASEID, V445, V025)

# Merge the two datasets based on CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns (ensure valid Z-scores)
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,  # Height-for-age Z-score (stunting)
    HW72 < 9990,  # Weight-for-height Z-score (wasting)
    HW71 < 9990   # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Mother's BMI
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition
    
    Mother_BMI = V445 / 100,  # Convert BMI to actual value (since V445 is stored as *100)
    Mother_BMI_cat = case_when(
      Mother_BMI < 18.5 ~ "Thin",
      Mother_BMI >= 18.5 & Mother_BMI < 25 ~ "Normal",
      Mother_BMI >= 25 ~ "Obese"
    ),
    
    # Categorize area type as factor with labels
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Mother's BMI and Place of Residence
malnutrition_by_mothers_BMI_residence <- merged_data %>%
  group_by(Mother_BMI_cat, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Mother_BMI_cat, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Corrected function to calculate counts for each Mother's BMI category in urban and rural areas for each indicator
calculate_counts <- function(data, category, residence, indicator) {
  total_children <- data %>%
    filter(Mother_BMI_cat == category, V025 == residence) %>%
    nrow()
  
  total_indicator <- data %>%
    filter(Mother_BMI_cat == category, V025 == residence, !!sym(indicator) == 1) %>%
    nrow()
  
  return(c(total_indicator, total_children))
}

# Create a helper function to calculate p-values for a given indicator
calculate_pvalues <- function(data, indicator) {
  # Urban p-value calculation
  urban_counts <- lapply(unique(data$Mother_BMI_cat), function(bmi) calculate_counts(data, bmi, "Urban", indicator))
  urban_counts_matrix <- do.call(rbind, urban_counts)
  pvalue_urban <- prop.test(urban_counts_matrix[, 1], urban_counts_matrix[, 2])$p.value
  
  # Rural p-value calculation
  rural_counts <- lapply(unique(data$Mother_BMI_cat), function(bmi) calculate_counts(data, bmi, "Rural", indicator))
  rural_counts_matrix <- do.call(rbind, rural_counts)
  pvalue_rural <- prop.test(rural_counts_matrix[, 1], rural_counts_matrix[, 2])$p.value
  
  return(c(pvalue_urban, pvalue_rural))
}

# Calculate p-values for each malnutrition indicator and region
pvalue_stunted <- calculate_pvalues(merged_data, "stunted")
pvalue_underweight <- calculate_pvalues(merged_data, "underweight")
pvalue_wasting <- calculate_pvalues(merged_data, "wasting")
pvalue_any_undernutrition <- calculate_pvalues(merged_data, "any_undernutrition")

# Create a data frame to hold p-values for each malnutrition indicator and region
pvalue_df <- data.frame(
  Mother_BMI_cat = rep(unique(merged_data$Mother_BMI_cat), each = 2),
  V025 = rep(c("Urban", "Rural"), length(unique(merged_data$Mother_BMI_cat))),
  Stunted_p_value = pvalue_stunted,
  Underweight_p_value = pvalue_underweight,
  Wasting_p_value = pvalue_wasting,
  Any_Undernutrition_p_value = pvalue_any_undernutrition
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_mothers_BMI_residence, pvalue_df, by = c("Mother_BMI_cat", "V025"))

# Create a formatted gt table that includes p-values and "Any form of undernutrition"
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Mother_BMI_cat, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Mother's BMI and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Mother_BMI_cat = "Mother's BMI",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values
print(malnutrition_gt_table_with_pvalues)




```
```{r}







# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Mother_BMI, stunted_data$V025)
# Count the number of rows in V445 that are stunted
stunted_v445_rows <- sum(!is.na(stunted_data$V445))



# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Mother_BMI, underweight_data$V025)
underweight_v445_rows <- sum(!is.na(underweight_data$V445))

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Mother_BMI, wasting_data$V025)
wasting_v445_rows <- sum(!is.na(wasting_data$V445))

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_v445_rows, underweight_v445_rows, wasting_v445_rows)
)

# Print the results table
print("Summary Table:")
print(results_table)
```




```{r}
###  CHILD'S AGE (IN MONTHS)






# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Include HW1 for Child's Age

# Select specific columns from datainD dataset, include Place of Residence (V025)
dataind_sub <- datainD %>%
  select(CASEID, V025)  # Include V025 for Residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Child's Age
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Child's Age based on HW1 values
    Child_age = case_when(
      HW1 >= 0 & HW1 <= 11 ~ "0-11",
      HW1 >= 12 & HW1 <= 23 ~ "12-23",
      HW1 >= 24 & HW1 <= 35 ~ "24-35",
      HW1 >= 36 & HW1 <= 47 ~ "36-47",
      HW1 >= 48 & HW1 <= 59 ~ "48-59"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Child_age = factor(Child_age, levels = c("0-11", "12-23", "24-35", "36-47", "48-59"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Child's Age and Place of Residence
malnutrition_by_child_age_residence <- merged_data %>%
  group_by(Child_age, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Child_age, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition percentages table
print(malnutrition_by_child_age_residence)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, age_group, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Child_age == age_group, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Child_age == age_group, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for stunting, underweight, wasting, and any undernutrition in urban and rural areas
age_groups <- c("0-11", "12-23", "24-35", "36-47", "48-59")

stunting_urban <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Urban", "stunted"))
underweight_urban <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Urban", "underweight"))
wasting_urban <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Urban", "wasting"))
any_undernutrition_urban <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Urban", "any_undernutrition"))

stunting_rural <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Rural", "stunted"))
underweight_rural <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Rural", "underweight"))
wasting_rural <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Rural", "wasting"))
any_undernutrition_rural <- sapply(age_groups, function(age_group) calculate_pvalue(merged_data, age_group, "Rural", "any_undernutrition"))

# Perform proportion tests for each malnutrition indicator
pvalue_stunting_urban <- prop.test(stunting_urban[1,], stunting_urban[2,])
pvalue_underweight_urban <- prop.test(underweight_urban[1,], underweight_urban[2,])
pvalue_wasting_urban <- prop.test(wasting_urban[1,], wasting_urban[2,])
pvalue_any_undernutrition_urban <- prop.test(any_undernutrition_urban[1,], any_undernutrition_urban[2,])

pvalue_stunting_rural <- prop.test(stunting_rural[1,], stunting_rural[2,])
pvalue_underweight_rural <- prop.test(underweight_rural[1,], underweight_rural[2,])
pvalue_wasting_rural <- prop.test(wasting_rural[1,], wasting_rural[2,])
pvalue_any_undernutrition_rural <- prop.test(any_undernutrition_rural[1,], any_undernutrition_rural[2,])

# Create a data frame to hold p-values
pvalue_df <- data.frame(
  Child_age = rep(age_groups, 2),
  V025 = rep(c("Urban", "Rural"), each = 5),
  Stunted_p_value = c(rep(pvalue_stunting_urban$p.value, 5), rep(pvalue_stunting_rural$p.value, 5)),
  Underweight_p_value = c(rep(pvalue_underweight_urban$p.value, 5), rep(pvalue_underweight_rural$p.value, 5)),
  Wasting_p_value = c(rep(pvalue_wasting_urban$p.value, 5), rep(pvalue_wasting_rural$p.value, 5)),
  Any_Undernutrition_p_value = c(rep(pvalue_any_undernutrition_urban$p.value, 5), rep(pvalue_any_undernutrition_rural$p.value, 5))
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_child_age_residence, pvalue_df, by = c("Child_age", "V025"))

# Create a formatted gt table that includes p-values for all malnutrition indicators
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Child_age, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Child's Age and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Child_age = "Child's Age",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values for all indicators
print(malnutrition_gt_table_with_pvalues)


```




```{r}





# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Child_age, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$HW1, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Child_age, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$HW1, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Child_age, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$HW1, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)


```
```{r}
### BIRTH ORDER





# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)


EGPR61FL <- read_sas("~/Downloads/EGPR61SD (1)/EGPR61FL.SAS7BDAT", 
                     NULL)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
data_individual <- EGBR61FL  # Individual dataset
data_household <- EGPR61FL   # Household dataset

# Filter for children aged 59 months or less and select relevant columns from the individual dataset
data_individual_sub <- data_individual %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Select child's age and malnutrition indicators

# Select specific columns from the household dataset, including birth order (HC64) and place of residence (HV025)
data_household_sub <- data_household %>%
  select(HHID, HC64, HV025)  # HHID is the household ID, HC64 is birth order, HV025 is residence

# Merge individual and household datasets based on HHID
merged_data <- data_individual_sub %>%
  mutate(HHID = substr(CASEID, 1, 12)) %>%  # Create HHID from CASEID to match the household dataset
  left_join(data_household_sub, by = "HHID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Birth Order
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Birth Order based on HC64 values
    birth_order = case_when(
      HC64 == 1 ~ "First",
      HC64 == 2 ~ "Second",
      HC64 >= 3 ~ "Third and above"
    ),
    HV025 = factor(HV025, levels = c(1, 2), labels = c("Urban", "Rural")),
    birth_order = factor(birth_order, levels = c("First", "Second", "Third and above"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Birth Order and Place of Residence
malnutrition_by_birth_order_residence <- merged_data %>%
  group_by(birth_order, HV025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(birth_order, HV025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition percentages table
print(malnutrition_by_birth_order_residence)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, order, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(birth_order == order, HV025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(birth_order == order, HV025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for stunting, underweight, wasting, and any undernutrition in urban and rural areas
birth_orders <- c("First", "Second", "Third and above")

stunting_urban <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Urban", "stunted"))
underweight_urban <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Urban", "underweight"))
wasting_urban <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Urban", "wasting"))
any_undernutrition_urban <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Urban", "any_undernutrition"))

stunting_rural <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Rural", "stunted"))
underweight_rural <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Rural", "underweight"))
wasting_rural <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Rural", "wasting"))
any_undernutrition_rural <- sapply(birth_orders, function(order) calculate_pvalue(merged_data, order, "Rural", "any_undernutrition"))

# Perform proportion tests for each malnutrition indicator
pvalue_stunting_urban <- prop.test(stunting_urban[1,], stunting_urban[2,])
pvalue_underweight_urban <- prop.test(underweight_urban[1,], underweight_urban[2,])
pvalue_wasting_urban <- prop.test(wasting_urban[1,], wasting_urban[2,])
pvalue_any_undernutrition_urban <- prop.test(any_undernutrition_urban[1,], any_undernutrition_urban[2,])

pvalue_stunting_rural <- prop.test(stunting_rural[1,], stunting_rural[2,])
pvalue_underweight_rural <- prop.test(underweight_rural[1,], underweight_rural[2,])
pvalue_wasting_rural <- prop.test(wasting_rural[1,], wasting_rural[2,])
pvalue_any_undernutrition_rural <- prop.test(any_undernutrition_rural[1,], any_undernutrition_rural[2,])

# Create a data frame to hold p-values
pvalue_df <- data.frame(
  birth_order = rep(birth_orders, 2),
  HV025 = rep(c("Urban", "Rural"), each = 3),
  Stunted_p_value = c(rep(pvalue_stunting_urban$p.value, 3), rep(pvalue_stunting_rural$p.value, 3)),
  Underweight_p_value = c(rep(pvalue_underweight_urban$p.value, 3), rep(pvalue_underweight_rural$p.value, 3)),
  Wasting_p_value = c(rep(pvalue_wasting_urban$p.value, 3), rep(pvalue_wasting_rural$p.value, 3)),
  Any_Undernutrition_p_value = c(rep(pvalue_any_undernutrition_urban$p.value, 3), rep(pvalue_any_undernutrition_rural$p.value, 3))
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_birth_order_residence, pvalue_df, by = c("birth_order", "HV025"))

# Create a formatted gt table that includes p-values for all malnutrition indicators
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(birth_order, HV025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Birth Order and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    birth_order = "Birth Order",
    HV025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values for all indicators
print(malnutrition_gt_table_with_pvalues)


```
```{r}





# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$birth_order, stunted_data$HV025)
stunted_age_sum <- sum(stunted_data$HC64, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$birth_order, underweight_data$HV025)
underweight_age_sum <- sum(underweight_data$HC64, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$birth_order, wasting_data$HV025)
wasting_age_sum <- sum(wasting_data$HC64, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)


```
```{r}

## NUMBER OF LIVING CHILDREN



# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Include HW1 for Child's Age

# Select specific columns from datainD dataset, include Place of Residence (V025) and Number of Living Children (V218)
dataind_sub <- datainD %>%
  select(CASEID, V025, V218)  # Include V025 for Residence, V218 for Number of Living Children

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Number of Living Children
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Number of Living Children based on V218 values
    Number_of_living_children = case_when(
      V218 >= 1 & V218 <= 2 ~ "1-2",
      V218 >= 3 & V218 <= 13 ~ "3-13"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Number_of_living_children = factor(Number_of_living_children, levels = c("1-2", "3-13"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Number of Living Children and Place of Residence
malnutrition_by_living_children_residence <- merged_data %>%
  group_by(Number_of_living_children, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(Number_of_living_children, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition percentages table
print(malnutrition_by_living_children_residence)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, living_children, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(Number_of_living_children == living_children, V025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(Number_of_living_children == living_children, V025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for malnutrition indicators in urban and rural areas
living_children_groups <- c("1-2", "3-13")

stunting_urban <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Urban", "stunted"))
underweight_urban <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Urban", "underweight"))
wasting_urban <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Urban", "wasting"))
any_undernutrition_urban <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Urban", "any_undernutrition"))

stunting_rural <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Rural", "stunted"))
underweight_rural <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Rural", "underweight"))
wasting_rural <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Rural", "wasting"))
any_undernutrition_rural <- sapply(living_children_groups, function(living_children) calculate_pvalue(merged_data, living_children, "Rural", "any_undernutrition"))

# Perform proportion tests for each malnutrition indicator
pvalue_stunting_urban <- prop.test(stunting_urban[1,], stunting_urban[2,])
pvalue_underweight_urban <- prop.test(underweight_urban[1,], underweight_urban[2,])
pvalue_wasting_urban <- prop.test(wasting_urban[1,], wasting_urban[2,])
pvalue_any_undernutrition_urban <- prop.test(any_undernutrition_urban[1,], any_undernutrition_urban[2,])

pvalue_stunting_rural <- prop.test(stunting_rural[1,], stunting_rural[2,])
pvalue_underweight_rural <- prop.test(underweight_rural[1,], underweight_rural[2,])
pvalue_wasting_rural <- prop.test(wasting_rural[1,], wasting_rural[2,])
pvalue_any_undernutrition_rural <- prop.test(any_undernutrition_rural[1,], any_undernutrition_rural[2,])

# Create a data frame to hold p-values
pvalue_df <- data.frame(
  Number_of_living_children = rep(living_children_groups, 2),
  V025 = rep(c("Urban", "Rural"), each = 2),
  Stunted_p_value = c(rep(pvalue_stunting_urban$p.value, 2), rep(pvalue_stunting_rural$p.value, 2)),
  Underweight_p_value = c(rep(pvalue_underweight_urban$p.value, 2), rep(pvalue_underweight_rural$p.value, 2)),
  Wasting_p_value = c(rep(pvalue_wasting_urban$p.value, 2), rep(pvalue_wasting_rural$p.value, 2)),
  Any_Undernutrition_p_value = c(rep(pvalue_any_undernutrition_urban$p.value, 2), rep(pvalue_any_undernutrition_rural$p.value, 2))
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_living_children_residence, pvalue_df, by = c("Number_of_living_children", "V025"))

# Create a formatted gt table that includes p-values for all malnutrition indicators
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(Number_of_living_children, V025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Number of Living Children and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    Number_of_living_children = "Number of Living Children",
    V025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values for all indicators
print(malnutrition_gt_table_with_pvalues)


```
```{r}






# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$Number_of_living_children, stunted_data$V025)
stunted_age_sum <- sum(stunted_data$V218, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$Number_of_living_children, underweight_data$V025)
underweight_age_sum <- sum(underweight_data$V218, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$Number_of_living_children, wasting_data$V025)
wasting_age_sum <- sum(wasting_data$V218, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)





```
```{r}

## BIRTH INTERVAL



# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
data_individual <- EGBR61FL  # Individual dataset
data_household <- EGPR61FL   # Household dataset

# Filter for children aged 59 months or less and select relevant columns from the individual dataset
data_individual_sub <- data_individual %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Select child's age and malnutrition indicators

# Select specific columns from the household dataset, including birth interval (HC63) and place of residence (HV025)
data_household_sub <- data_household %>%
  select(HHID, HC63, HV025)  # HHID is the household ID, HC63 is birth interval, HV025 is residence

# Merge individual and household datasets based on HHID
merged_data <- data_individual_sub %>%
  mutate(HHID = substr(CASEID, 1, 12)) %>%  # Create HHID from CASEID to match the household dataset
  left_join(data_household_sub, by = "HHID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Birth Interval
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Define "Any form of undernutrition" as having any of stunting, underweight, or wasting
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Categorize Birth Interval based on HC63 values
    birth_interval = case_when(
      HC63 >= 7 & HC63 <= 12 ~ "First Birth",
      HC63 <= 24 ~ "Less than 24",
      HC63 >= 24 & HC63 <= 47 ~ "24-47",
      HC63 >= 48 ~ "48 and above"
    ),
    HV025 = factor(HV025, levels = c(1, 2), labels = c("Urban", "Rural")),
    birth_interval = factor(birth_interval, levels = c("First Birth", "Less than 24", "24-47", "48 and above"))
  )

# Calculate unweighted percentages for each malnutrition indicator by Birth Interval and Place of Residence
malnutrition_by_birth_interval_residence <- merged_data %>%
  group_by(birth_interval, HV025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted, na.rm = TRUE),
    Underweight = sum(underweight, na.rm = TRUE),
    Wasting = sum(wasting, na.rm = TRUE),
    Any_Undernutrition = sum(any_undernutrition, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Undernutrition = (Any_Undernutrition / Total_Children) * 100
  ) %>%
  select(birth_interval, HV025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)

# Print the malnutrition percentages table
print(malnutrition_by_birth_interval_residence)

# Define a helper function to perform the proportion test
calculate_pvalue <- function(data, interval, residence, malnutrition_indicator) {
  total_children <- data %>%
    filter(birth_interval == interval, HV025 == residence) %>%
    nrow()
  
  total_malnourished <- data %>%
    filter(birth_interval == interval, HV025 == residence, !!sym(malnutrition_indicator) == 1) %>%
    nrow()
  
  return(c(total_malnourished, total_children))
}

# Calculate p-values for malnutrition indicators in urban and rural areas
birth_intervals <- c("First Birth", "Less than 24", "24-47", "48 and above")

stunting_urban <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Urban", "stunted"))
underweight_urban <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Urban", "underweight"))
wasting_urban <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Urban", "wasting"))
any_undernutrition_urban <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Urban", "any_undernutrition"))

stunting_rural <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Rural", "stunted"))
underweight_rural <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Rural", "underweight"))
wasting_rural <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Rural", "wasting"))
any_undernutrition_rural <- sapply(birth_intervals, function(interval) calculate_pvalue(merged_data, interval, "Rural", "any_undernutrition"))

# Perform proportion tests for each malnutrition indicator
pvalue_stunting_urban <- prop.test(stunting_urban[1,], stunting_urban[2,])
pvalue_underweight_urban <- prop.test(underweight_urban[1,], underweight_urban[2,])
pvalue_wasting_urban <- prop.test(wasting_urban[1,], wasting_urban[2,])
pvalue_any_undernutrition_urban <- prop.test(any_undernutrition_urban[1,], any_undernutrition_urban[2,])

pvalue_stunting_rural <- prop.test(stunting_rural[1,], stunting_rural[2,])
pvalue_underweight_rural <- prop.test(underweight_rural[1,], underweight_rural[2,])
pvalue_wasting_rural <- prop.test(wasting_rural[1,], wasting_rural[2,])
pvalue_any_undernutrition_rural <- prop.test(any_undernutrition_rural[1,], any_undernutrition_rural[2,])

# Create a data frame to hold p-values
pvalue_df <- data.frame(
  birth_interval = rep(birth_intervals, 2),
  HV025 = rep(c("Urban", "Rural"), each = 4),
  Stunted_p_value = c(rep(pvalue_stunting_urban$p.value, 4), rep(pvalue_stunting_rural$p.value, 4)),
  Underweight_p_value = c(rep(pvalue_underweight_urban$p.value, 4), rep(pvalue_underweight_rural$p.value, 4)),
  Wasting_p_value = c(rep(pvalue_wasting_urban$p.value, 4), rep(pvalue_wasting_rural$p.value, 4)),
  Any_Undernutrition_p_value = c(rep(pvalue_any_undernutrition_urban$p.value, 4), rep(pvalue_any_undernutrition_rural$p.value, 4))
)

# Merge the original summary table with the p-values table
malnutrition_with_pvalues <- left_join(malnutrition_by_birth_interval_residence, pvalue_df, by = c("birth_interval", "HV025"))

# Create a formatted gt table that includes p-values for all malnutrition indicators
malnutrition_gt_table_with_pvalues <- malnutrition_with_pvalues %>%
  arrange(birth_interval, HV025) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages and P-values by Birth Interval and Place of Residence"
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition, Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value),
    decimals = 3
  ) %>%
  cols_label(
    birth_interval = "Birth Interval",
    HV025 = "Residence",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Undernutrition = "Any Undernutrition (%)",
    Stunted_p_value = "Stunted P-value",
    Underweight_p_value = "Underweight P-value",
    Wasting_p_value = "Wasting P-value",
    Any_Undernutrition_p_value = "Any Undernutrition P-value"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Undernutrition)
  ) %>%
  tab_spanner(
    label = "P-values",
    columns = c(Stunted_p_value, Underweight_p_value, Wasting_p_value, Any_Undernutrition_p_value)
  ) %>%
  opt_row_striping() # Adds row striping for better readability

# Display the formatted gt table with p-values for all indicators
print(malnutrition_gt_table_with_pvalues)


```
```{R}





# Load required library
library(dplyr)

# Filter and analyze for stunted children
stunted_data <- merged_data %>%
  filter(stunted == 1)

chisq_test_stunted <- chisq.test(stunted_data$birth_interval, stunted_data$HV025)
stunted_age_sum <- sum(stunted_data$HC63, na.rm = TRUE)

# Filter and analyze for underweight children
underweight_data <- merged_data %>%
  filter(underweight == 1)

chisq_test_underweight <- chisq.test(underweight_data$birth_interval, underweight_data$HV025)
underweight_age_sum <- sum(underweight_data$HC63, na.rm = TRUE)

# Filter and analyze for wasting children
wasting_data <- merged_data %>%
  filter(wasting == 1)

chisq_test_wasting <- chisq.test(wasting_data$birth_interval, wasting_data$HV025)
wasting_age_sum <- sum(wasting_data$HC63, na.rm = TRUE)

# Create a summary table
results_table <- data.frame(
  Category = c("Stunted", "Underweight", "Wasting"),
  Chi_Square_Statistic = c(chisq_test_stunted$statistic, chisq_test_underweight$statistic, chisq_test_wasting$statistic),
  P_Value = c(chisq_test_stunted$p.value, chisq_test_underweight$p.value, chisq_test_wasting$p.value),
  Total_Age_Sum = c(stunted_age_sum, underweight_age_sum, wasting_age_sum)
)

# Print the results table
print("Summary Table:")
print(results_table)


```

```{r}
## FATHER'S OCCUPATION AND MOTHER'S WORKING STATUS






# Load necessary libraries
library(dplyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Father's Occupation (V704) and Mother's Working Status (V731)
dataind_sub <- datainD %>%
  select(CASEID, V704, V731, V025)  # Include V704 for Father's Occupation, V731 for Mother's Working Status, and V025 for area of residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Create variables for malnutrition indicators and categorize Father's Occupation and Mother's Working Status
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Categorize Father's Occupation (without "Other")
    Father_occupation = case_when(
      V704 >= 1 & V704 <= 54 ~ "Service/Business",    # Custom category for Service/Business
      V704 >= 55 & V704 <= 96 ~ "Farmers/Others"      # Custom category for Farmers/Others
    ),
    
    # Categorize Mother's Working Status (only 0 and 1)
    Mother_working_status = case_when(
      V731 == 0 ~ "Not Working",
      V731 == 1 ~ "Working"
    ),
    
    # Create a new variable for Any Form of Undernutrition (stunted or underweight or wasting)
    any_form_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    
    # Create a new variable for All Forms of Undernutrition (stunted and underweight and wasting)
    all_form_undernutrition = ifelse(stunted == 1 & underweight == 1 & wasting == 1, 1, 0),
    
    # Convert V025 to labels: 1 = Urban, 2 = Rural
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural"))
  ) %>%
  # Remove rows with "NA" in Mother's Working Status
  filter(!is.na(Mother_working_status))

# Calculate malnutrition percentages by Father's Occupation, Mother's Working Status, and Area (Urban/Rural)
malnutrition_percentages <- merged_data %>%
  group_by(Father_occupation, Mother_working_status, V025) %>%
  summarize(
    Total_Children = n(),                                  # Count total children for each group
    Stunted = sum(stunted),                                # Total number of stunted children
    Underweight = sum(underweight),                        # Total number of underweight children
    Wasting = sum(wasting),                                # Total number of wasted children
    Any_Form_of_Undernutrition = sum(any_form_undernutrition),  # Children experiencing any form of undernutrition
    All_Forms_of_Undernutrition = sum(all_form_undernutrition)  # Children experiencing all forms of undernutrition
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Form_of_Undernutrition = (Any_Form_of_Undernutrition / Total_Children) * 100,
    Percent_All_Forms_of_Undernutrition = (All_Forms_of_Undernutrition / Total_Children) * 100
  )

# Create a formatted table using gt without the raw counts and totals
malnutrition_gt_table <- malnutrition_percentages %>%
  arrange(V025, Father_occupation, Mother_working_status) %>%
  select(Father_occupation, Mother_working_status, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition) %>%  # Only include percentage columns
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages by Father's Occupation and Mother's Working Status in Urban and Rural Areas"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition)
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition),
    decimals = 1
  ) %>%
  cols_label(
    Father_occupation = "Father's Occupation",
    Mother_working_status = "Mother's Working Status",
    V025 = "Area",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Form_of_Undernutrition = "Any Form of Undernutrition (%)",
    Percent_All_Forms_of_Undernutrition = "All Forms of Undernutrition (%)"
  ) %>%
  opt_row_striping()  # Adds row striping for better readability

# Display the formatted gt table
print(malnutrition_gt_table)


```

```{r}

### MOTHER'S EDUCATION AND MOTHER'S WORKING STATUS





# Load necessary libraries
library(dplyr)
library(gt)

# Step 1: Ensure V025 (Area) is correctly set as a factor before merging
dataind_sub <- datainD %>%
  mutate(V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural"))) %>%
  select(CASEID, V025, V106, V731)

# Step 2: Select relevant columns from the 'databir' dataset for child-related malnutrition data
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%  # Filter for children aged 59 months or less
  select(CASEID, HW1, HW70, HW71, HW72)  # Select malnutrition-related columns

# Step 3: Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Check if merge was successful
print(paste("Total rows after merging:", nrow(merged_data)))

# Step 4: Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Check after removing NAs
print(paste("Total rows after removing NAs:", nrow(merged_data)))

# Step 5: Create variables for Mother's Education, Working Status, and Malnutrition Indicators
merged_data <- merged_data %>%
  mutate(
    Mother_education = case_when(
      V106 == 0 ~ "No Education",
      V106 == 1 ~ "Primary",
      V106 >= 2 ~ "Secondary and Above"
    ),
    Mother_working_status = case_when(
      V731 == 0 ~ "Not Working",
      V731 == 1 ~ "Working"
    ),
    stunted = ifelse(HW70 < -200, 1, 0),  # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),  # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),  # Wasting (Weight-for-height z-score < -2 SD)
    any_form_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    all_form_undernutrition = ifelse(stunted == 1 & underweight == 1 & wasting == 1, 1, 0)
  ) %>%
  # Remove rows with NA values in Mother's Education or Working Status
  filter(!is.na(Mother_education) & !is.na(Mother_working_status))

# Check if variable creation was successful
print(head(merged_data))

# Step 6: Calculate malnutrition percentages by Mother's Education, Working Status, and Area
malnutrition_percentages <- merged_data %>%
  group_by(Mother_education, Mother_working_status, V025) %>%
  summarize(
    Total_Children = n(),
    Stunted = sum(stunted),
    Underweight = sum(underweight),
    Wasting = sum(wasting),
    Any_Form_of_Undernutrition = sum(any_form_undernutrition),
    All_Forms_of_Undernutrition = sum(all_form_undernutrition)
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = ifelse(Total_Children > 0, (Stunted / Total_Children) * 100, NA),
    Percent_Underweight = ifelse(Total_Children > 0, (Underweight / Total_Children) * 100, NA),
    Percent_Wasting = ifelse(Total_Children > 0, (Wasting / Total_Children) * 100, NA),
    Percent_Any_Form_of_Undernutrition = ifelse(Total_Children > 0, (Any_Form_of_Undernutrition / Total_Children) * 100, NA),
    Percent_All_Forms_of_Undernutrition = ifelse(Total_Children > 0, (All_Forms_of_Undernutrition / Total_Children) * 100, NA)
  )

# Check the summary of the calculated percentages
print(malnutrition_percentages)

# Step 7: Remove rows with zero Total_Children to focus on non-empty groups
malnutrition_percentages <- malnutrition_percentages %>%
  filter(Total_Children > 0)

# Step 8: Create a formatted gt table
malnutrition_gt_table <- malnutrition_percentages %>%
  arrange(V025, Mother_education, Mother_working_status) %>%
  select(Mother_education, Mother_working_status, V025, Percent_Stunted, Percent_Underweight, 
         Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition) %>%
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages by Mother's Education and Mother's Working Status in Urban and Rural Areas"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, 
                Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition)
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, 
                Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition),
    decimals = 1
  ) %>%
  cols_label(
    Mother_education = "Mother's Education",
    Mother_working_status = "Mother's Working Status",
    V025 = "Area",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Form_of_Undernutrition = "Any Form of Undernutrition (%)",
    Percent_All_Forms_of_Undernutrition = "All Forms of Undernutrition (%)"
  ) %>%
  opt_row_striping()

# Step 9: Display the gt table
print(malnutrition_gt_table)


```

```{r}
### FATHER'S EDUCATION AND FATHER'S WORKING STATUS





# Load necessary libraries
library(dplyr)
library(gt)

# Assuming the datasets are already loaded as `datainD` (EGIR61FL) and `databir` (EGBR61FL)
# Ensure to include the Father's Education (V701) and Father's Occupation (V704)

# Select relevant columns from the `datainD` dataset, including Father's Education (V701) and Occupation (V704)
dataind_sub <- datainD %>%
  select(CASEID, V025, V701, V704)  # Ensure V701 (Father's Education) and V704 (Father's Occupation) are selected

# Select relevant columns from the `databir` dataset for child-related malnutrition data
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%  # Filter for children aged 59 months or less
  select(CASEID, HW1, HW70, HW71, HW72)  # Select malnutrition-related columns

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Create variables for malnutrition indicators and categorize Father's Education and Father's Occupation
merged_data <- merged_data %>%
  mutate(
    # Categorize Father's Education based on V701 values
    Father_education = case_when(
      V701 == 0 ~ "No Education",
      V701 == 1 ~ "Primary",
      V701 >= 2 ~ "Secondary and Above",
      TRUE ~ "Other"
    ),
    # Categorize Father's Occupation based on V704 values
    Father_occupation = case_when(
      V704 >= 1 & V704 <= 54 ~ "Service/Business",
      V704 >= 55 & V704 <= 96 ~ "Farmers/Others",
      TRUE ~ "Other"
    ),
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),  # Area: Urban or Rural
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    
    # Create new variables for "Any Form of Undernutrition" and "All Forms of Undernutrition"
    any_form_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition (stunted or underweight or wasting)
    all_forms_undernutrition = ifelse(stunted == 1 & underweight == 1 & wasting == 1, 1, 0)  # All forms of undernutrition (stunted and underweight and wasting)
  )

# Calculate malnutrition percentages by Father's Education, Father's Occupation, and Area (Urban/Rural)
malnutrition_percentages <- merged_data %>%
  group_by(Father_education, Father_occupation, V025) %>%
  summarize(
    Total_Children = n(),                                   # Count total children for each group
    Stunted = sum(stunted),                                 # Total number of stunted children
    Underweight = sum(underweight),                         # Total number of underweight children
    Wasting = sum(wasting),                                 # Total number of wasted children
    Any_Form_of_Undernutrition = sum(any_form_undernutrition),   # Children experiencing any form of undernutrition
    All_Forms_of_Undernutrition = sum(all_forms_undernutrition)  # Children experiencing all forms of undernutrition
  ) %>%
  ungroup() %>%
  mutate(
    Percent_Stunted = (Stunted / Total_Children) * 100,
    Percent_Underweight = (Underweight / Total_Children) * 100,
    Percent_Wasting = (Wasting / Total_Children) * 100,
    Percent_Any_Form_of_Undernutrition = (Any_Form_of_Undernutrition / Total_Children) * 100,
    Percent_All_Forms_of_Undernutrition = (All_Forms_of_Undernutrition / Total_Children) * 100
  )

# Create a formatted table using gt without the raw counts and totals
malnutrition_gt_table <- malnutrition_percentages %>%
  arrange(V025, Father_education, Father_occupation) %>%
  select(Father_education, Father_occupation, V025, Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition) %>%  # Only include percentage columns
  gt() %>%
  tab_header(
    title = "Malnutrition Percentages by Father's Education and Father's Occupation in Urban and Rural Areas"
  ) %>%
  tab_spanner(
    label = "Malnutrition Indicators (%)",
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition)
  ) %>%
  fmt_number(
    columns = c(Percent_Stunted, Percent_Underweight, Percent_Wasting, Percent_Any_Form_of_Undernutrition, Percent_All_Forms_of_Undernutrition),
    decimals = 1
  ) %>%
  cols_label(
    Father_education = "Father's Education",
    Father_occupation = "Father's Occupation",
    V025 = "Area",
    Percent_Stunted = "Stunted (%)",
    Percent_Underweight = "Underweight (%)",
    Percent_Wasting = "Wasting (%)",
    Percent_Any_Form_of_Undernutrition = "Any Form of Undernutrition (%)",
    Percent_All_Forms_of_Undernutrition = "All Forms of Undernutrition (%)"
  ) %>%
  opt_row_striping()  # Adds row striping for better readability

# Display the formatted gt table
print(malnutrition_gt_table)


```

```{r}
## BINARY PROBIT





# Load necessary libraries
library(dplyr)
library(gt)
library(broom)
library(purrr)
library(haven)
library(tidyr)

# Define helper function to annotate coefficients based on p-values
annotate_coefficient <- function(coef, pvalue) {
  if (pvalue < 0.01) {
    return(paste0(round(coef, 3), "^a"))
  } else if (pvalue < 0.05) {
    return(paste0(round(coef, 3), "^b"))
  } else if (pvalue < 0.1) {
    return(paste0(round(coef, 3), "^c"))
  } else {
    return(as.character(round(coef, 3)))
  }
}


EGBR61FL <- read_sas("EGBR61SD (3)/EGBR61FL.SAS7BDAT", 
                     NULL)

EGIR61FL <- read_sas("EGIR61SD (3)/EGIR61FL.SAS7BDAT", 
                     NULL)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Select relevant columns from `datainD` for parental characteristics
dataind_sub <- datainD %>%
  select(CASEID, V025, V106, V701, V714, V704, V024)  # V024 is an additional control variable (Region)

# Select relevant columns from `databir` for child-related malnutrition data
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%  # Filter for children aged 59 months or less
  select(CASEID, HW1, HW70, HW71, HW72)  # Select malnutrition-related columns

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Step 1: Create malnutrition indicators without categorizing parental variables
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),          # Stunted: Height-for-age z-score < -2 SD
    underweight = ifelse(HW71 < -200, 1, 0),      # Underweight: Weight-for-age z-score < -2 SD
    wasting = ifelse(HW72 < -200, 1, 0),          # Wasting: Weight-for-height z-score < -2 SD
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0)  # Any form of undernutrition
  )

# Step 2: Create a function to fit binary probit models for each undernutrition indicator and area
fit_models <- function(data, indicator) {
  # Model-1: Basic model
  urban_model1 <- glm(as.formula(paste(indicator, "~ V106 + V701 + V714 + V704")), 
                      family = binomial(link = "probit"), data = data %>% filter(V025 == 1))  # 1 = Urban
  rural_model1 <- glm(as.formula(paste(indicator, "~ V106 + V701 + V714 + V704")), 
                      family = binomial(link = "probit"), data = data %>% filter(V025 == 2))  # 2 = Rural
  
  # Model-2: Interaction model
  urban_model2 <- glm(as.formula(paste(indicator, "~ V106 * V701 + V714 + V704")), 
                      family = binomial(link = "probit"), data = data %>% filter(V025 == 1))  # 1 = Urban
  rural_model2 <- glm(as.formula(paste(indicator, "~ V106 * V701 + V714 + V704")), 
                      family = binomial(link = "probit"), data = data %>% filter(V025 == 2))  # 2 = Rural
  
  # Model-3: Control variable added
  urban_model3 <- glm(as.formula(paste(indicator, "~ V106 + V701 + V714 + V704 + V024")), 
                      family = binomial(link = "probit"), data = data %>% filter(V025 == 1))  # 1 = Urban
  rural_model3 <- glm(as.formula(paste(indicator, "~ V106 + V701 + V714 + V704 + V024")), 
                      family = binomial(link = "probit"), data = data %>% filter(V025 == 2))  # 2 = Rural
  
  list(model1 = list(urban = urban_model1, rural = rural_model1),
       model2 = list(urban = urban_model2, rural = rural_model2),
       model3 = list(urban = urban_model3, rural = rural_model3))
}

# Step 3: Create a function to extract and format model summaries
extract_and_format_model <- function(model, type, area, model_name) {
  summary <- tidy(model)
  summary %>%
    filter(term != "(Intercept)") %>%  # Exclude the intercept term from the results
    mutate(
      Type_of_Undernutrition = type,
      Area = area,
      Model = model_name,
      Coefficient = map2_chr(estimate, p.value, annotate_coefficient)  # Use map2_chr from purrr
    ) %>%
    # Relabel variables with descriptive names for clarity
    mutate(term = case_when(
      term == "V106" ~ "Mother's Education",
      term == "V701" ~ "Father's Education",
      term == "V714" ~ "Mother's Working Status",
      term == "V704" ~ "Father's Occupation",
      term == "V024" ~ "Region",
      TRUE ~ term
    )) %>%
    select(Type_of_Undernutrition, term, Coefficient, Area, Model)
}

# Step 4: Create models for each undernutrition indicator
stunting_models <- fit_models(merged_data, "stunted")
underweight_models <- fit_models(merged_data, "underweight")
wasting_models <- fit_models(merged_data, "wasting")
any_undernutrition_models <- fit_models(merged_data, "any_undernutrition")

# Step 5: Extract and format summaries for each indicator and model
# Extract for Model-1, Model-2, and Model-3
all_summaries <- bind_rows(
  lapply(names(stunting_models), function(model_name) {
    bind_rows(
      extract_and_format_model(stunting_models[[model_name]]$urban, "Stunting", "Urban", model_name),
      extract_and_format_model(stunting_models[[model_name]]$rural, "Stunting", "Rural", model_name),
      extract_and_format_model(underweight_models[[model_name]]$urban, "Underweight", "Urban", model_name),
      extract_and_format_model(underweight_models[[model_name]]$rural, "Underweight", "Rural", model_name),
      extract_and_format_model(wasting_models[[model_name]]$urban, "Wasting", "Urban", model_name),
      extract_and_format_model(wasting_models[[model_name]]$rural, "Wasting", "Rural", model_name),
      extract_and_format_model(any_undernutrition_models[[model_name]]$urban, "Any form of undernutrition", "Urban", model_name),
      extract_and_format_model(any_undernutrition_models[[model_name]]$rural, "Any form of undernutrition", "Rural", model_name)
    )
  }) %>% bind_rows()
)

# Step 6: Filter out unwanted terms (e.g., interactions like "V106:V701" and "Region")
filtered_summaries <- all_summaries %>%
  filter(!term %in% c("V106:V701", "Region"))

# Step 7: Create a formatted table using gt
summary_table <- filtered_summaries %>%
  select(Type_of_Undernutrition, term, Coefficient, Area, Model) %>%
  pivot_wider(names_from = c(Area, Model), values_from = Coefficient) %>%
  gt() %>%
  tab_header(
    title = "Binary Probit Coefficient of Parental Characteristics Affecting Nutritional Status of Children Under Five in Urban and Rural Areas"
  ) %>%
  cols_label(
    Type_of_Undernutrition = "Type of Undernutrition",
    term = "Parental Characteristics",
    `Urban_model1` = "Urban (Model-1)",
    `Rural_model1` = "Rural (Model-1)",
    `Urban_model2` = "Urban (Model-2)",
    `Rural_model2` = "Rural (Model-2)",
    `Urban_model3` = "Urban (Model-3)",
    `Rural_model3` = "Rural (Model-3)"
  ) %>%
  opt_row_striping() %>%
  fmt_number(
    columns = everything(),
    decimals = 3
  )

# Step 8: Display the formatted gt table
print(summary_table)


```

```{r}
## ggplot of mother's age





library(dplyr)
library(haven)
library(KernSmooth)
library(ggplot2)
library(tidyverse)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Mother's Age (V012)
dataind_sub <- datainD %>%
  select(CASEID, V012, V025)  # Include V012 for Mother's Age

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)




merged_data 
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Mother's Age
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0)  # Any form of undernutrition
  )

# Apply kernel smoothing for each malnutrition indicator
bandwidth <- 5  # Adjust the bandwidth as needed

# Smoothing for wasting
smoothed_wasting <- locpoly(merged_data$V012, merged_data$wasting, bandwidth = bandwidth, gridsize = length(merged_data$V012))

# Smoothing for underweight
smoothed_underweight <- locpoly(merged_data$V012, merged_data$underweight, bandwidth = bandwidth, gridsize = length(merged_data$V012))

# Smoothing for stunted
smoothed_stunted <- locpoly(merged_data$V012, merged_data$stunted, bandwidth = bandwidth, gridsize = length(merged_data$V012))

# Smoothing for any undernutrition
smoothed_any_undernutrition <- locpoly(merged_data$V012, merged_data$any_undernutrition, bandwidth = bandwidth, gridsize = length(merged_data$V012))

# Combine smoothed data into a single dataframe for ggplot
plot_data <- data.frame(
  Mothers_Age = smoothed_wasting$x,  # Renamed column here
  Wasting = smoothed_wasting$y,
  Underweight = smoothed_underweight$y,
  Stunted = smoothed_stunted$y,
  Any_Undernutrition = smoothed_any_undernutrition$y
)

# Reshape the data to long format for ggplot
plot_data_long <- plot_data %>%
  gather(key = "Indicator", value = "Proportion", -Mothers_Age)  # No single quote here

# Plot with ggplot
ggplot(plot_data_long, aes(x = Mothers_Age, y = Proportion, color = Indicator)) +
  geom_line(size = 1.2) +
  labs(
    title = "Kernel Smoothing: Malnutrition Indicators vs Mother's Age",
    x = "Mother's Age",
    y = "Proportion of Malnutrition"
  ) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme_minimal()


```


```{r}

# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Mother's Age (V012) and place of residence (V025)
dataind_sub <- datainD %>%
  select(CASEID, V012, V025)  # Include V012 for Mother's Age and V025 for place of residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize place of residence
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural"))                # Convert V025 to Urban/Rural factor
  )

# Calculate the total number of children under 5 in rural areas
total_rural_children <- merged_data %>%
  filter(V025 == "Rural") %>%
  summarise(total_children = n()) %>%
  pull(total_children)

# Calculate the total number of stunted children in rural areas
total_stunted_rural <- merged_data %>%
  filter(V025 == "Rural", stunted == 1) %>%
  summarise(total_stunted = n()) %>%
  pull(total_stunted)

# Calculate the percentage
percentage_stunted_rural <- (total_stunted_rural / total_rural_children) * 100

# Display the result
cat("Percentage of stunted children in rural areas:", round(percentage_stunted_rural, 2), "%\n")

```
```{r}
# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Mother's Age (V012) and place of residence (V025)
dataind_sub <- datainD %>%
  select(CASEID, V012, V025)  # Include V012 for Mother's Age and V025 for place of residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize place of residence
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    malnutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural"))                # Convert V025 to Urban/Rural factor
  )

# Calculate the total number of children under 5 in urban areas
total_urban_children <- merged_data %>%
  filter(V025 == "Urban") %>%
  summarise(total_children = n()) %>%
  pull(total_children)

# Calculate the total number of stunted children in urban areas
total_underweight_urban <- merged_data %>%
  filter(V025 == "Urban", underweight == 1) %>%
  summarise(total_underweight = n()) %>%
  pull(total_underweight)

# Calculate the percentage
percentage_underweight_urban <- (total_underweight_urban / total_urban_children) * 100

# Display the result
cat("Percentage of underweight children in urban areas:", round(percentage_underweight_urban, 2), "%\n")












# Calculate the total number of children under 5 in urban areas
total_urban_children <- merged_data %>%
  filter(V025 == "Urban") %>%
  summarise(total_children = n()) %>%
  pull(total_children)

# Calculate the total number of malnourished children in urban areas
total_malnourished_urban <- merged_data %>%
  filter(V025 == "Urban", malnutrition == 1) %>%
  summarise(total_malnourished = n()) %>%
  pull(total_malnourished)

# Calculate the percentage
percentage_malnutrition_urban <- (total_malnourished_urban / total_urban_children) * 100

# Display the result
cat("Percentage of malnutrition in urban areas:", round(percentage_malnutrition_urban, 2), "%\n")

```



```{r}

# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Mother's Age (V012) and place of residence (V025)
dataind_sub <- datainD %>%
  select(CASEID, V012, V025)  # Include V012 for Mother's Age and V025 for place of residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize place of residence
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    malnutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),  # Any form of undernutrition
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural"))                # Convert V025 to Urban/Rural factor
  )

# Calculate the total number of children under 5 in urban areas
total_urban_children <- merged_data %>%
  filter(V025 == "Urban") %>%
  summarise(total_children = n()) %>%
  pull(total_children)

# Calculate the total number of stunted children in urban areas
total_underweight_urban <- merged_data %>%
  filter(V025 == "Urban", underweight == 1) %>%
  summarise(total_underweight = n()) %>%
  pull(total_underweight)

# Calculate the percentage
percentage_underweight_urban <- (total_underweight_urban / total_urban_children) * 100

# Display the result
cat("Percentage of underweight children in urban areas:", round(percentage_underweight_urban, 2), "%\n")












# Calculate the total number of children under 5 in rural areas
total_rural_children <- merged_data %>%
  filter(V025 == "Rural") %>%
  summarise(total_children = n()) %>%
  pull(total_children)

# Calculate the total number of malnourished children in rural areas
total_malnourished_rural <- merged_data %>%
  filter(V025 == "Rural", malnutrition == 1) %>%
  summarise(total_malnourished = n()) %>%
  pull(total_malnourished)

# Calculate the percentage
percentage_malnutrition_rural <- (total_malnourished_rural / total_rural_children) * 100

# Display the result
cat("Percentage of malnutrition in rural areas:", round(percentage_malnutrition_rural, 2), "%\n")


```
```{r}

## MOTHER'S EDUCATION VISUALISATION



# Load necessary libraries
library(dplyr)
library(haven)
library(KernSmooth)
library(ggplot2)
library(tidyr)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72, V012)  # Include V012 for Mother's Age

# Select specific columns from datainD dataset, include Mother's Education (V106) and Place of Residence (V025)
dataind_sub <- datainD %>%
  select(CASEID, V106, V025)

# Merge the two datasets based on CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  ) %>%
  mutate(
    education = case_when(
      V106 == 0 ~ "No education",
      V106 == 1 ~ "Primary",
      V106 == 2 ~ "Secondary",
      V106 == 3 ~ "Higher"
    )
  )

# Create new variables for malnutrition indicators
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0)  # Any form of undernutrition
  )

# Ensure 'V012' (Mother's Age) does not have missing or infinite values
merged_data <- merged_data %>%
  filter(!is.na(V012) & is.finite(V012))  # Remove NA and infinite values

# Apply kernel smoothing for each malnutrition indicator by education level
apply_smoothing <- function(x, y, education) {
  # Ensure non-missing values for smoothing
  x <- x[!is.na(x) & !is.infinite(x)]
  y <- y[!is.na(y) & !is.infinite(y)]
  education <- education[!is.na(education)]
  
  # Apply kernel smoothing for each education level
  smooth_no_education <- locpoly(x[education == "No education"], y[education == "No education"], bandwidth = 5, gridsize = length(x))
  smooth_primary <- locpoly(x[education == "Primary"], y[education == "Primary"], bandwidth = 5, gridsize = length(x))
  smooth_secondary <- locpoly(x[education == "Secondary"], y[education == "Secondary"], bandwidth = 5, gridsize = length(x))
  smooth_higher <- locpoly(x[education == "Higher"], y[education == "Higher"], bandwidth = 5, gridsize = length(x))
  
  # Combine into a dataframe for each education level
  data.frame(
    Mothers_Age = c(smooth_no_education$x, smooth_primary$x, smooth_secondary$x, smooth_higher$x),
    Proportion = c(smooth_no_education$y, smooth_primary$y, smooth_secondary$y, smooth_higher$y),
    education = rep(c("No education", "Primary", "Secondary", "Higher"), each = length(smooth_no_education$x))
  )
}

# Smoothing for each malnutrition indicator
smoothed_wasting <- apply_smoothing(merged_data$V012, merged_data$wasting, merged_data$education)
smoothed_underweight <- apply_smoothing(merged_data$V012, merged_data$underweight, merged_data$education)
smoothed_stunted <- apply_smoothing(merged_data$V012, merged_data$stunted, merged_data$education)
smoothed_any_undernutrition <- apply_smoothing(merged_data$V012, merged_data$any_undernutrition, merged_data$education)

# Combine all smoothed data into a single dataframe for ggplot
plot_data <- bind_rows(
  smoothed_wasting %>% mutate(Indicator = "Wasting"),
  smoothed_underweight %>% mutate(Indicator = "Underweight"),
  smoothed_stunted %>% mutate(Indicator = "Stunted"),
  smoothed_any_undernutrition %>% mutate(Indicator = "Any Undernutrition")
)

# Plot with ggplot in a 2x2 grid layout
ggplot(plot_data, aes(x = Mothers_Age, y = Proportion, color = education)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Indicator, nrow = 2, ncol = 2, scales = "free_y") +  # Create a 2x2 grid
  labs(
    title = "Kernel Smoothing: Malnutrition Indicators by Mother's Education",
    x = "Mother's Age",
    y = "Proportion of Malnutrition"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("No education" = "red", "Primary" = "orange", "Secondary" = "blue", "Higher" = "green")) +  # Education colors
  theme(legend.position = "bottom")


```

```{r}
## REGION




# Load necessary libraries
library(dplyr)
library(haven)
library(KernSmooth)
library(ggplot2)
library(tidyr)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)  # Include HW1 for Child's Age

# Select specific columns from datainD dataset, include Place of Residence (V025) and Number of Living Children (V218)
dataind_sub <- datainD %>%
  select(CASEID, V025, V218, V012)  # Include V025 for Residence, V218 for Number of Living Children, V012 for Mother's Age

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  ) %>%
  mutate(
    region = case_when(
      V025 == 1 ~ "Urban",
      V025 == 2 ~ "Rural"
    )
  )

# Create new variables for malnutrition indicators
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0)  # Any form of undernutrition
  )

# Ensure 'V012' (Mother's Age) does not have missing or infinite values
merged_data <- merged_data %>%
  filter(!is.na(V012) & is.finite(V012))  # Remove NA and infinite values

# Apply kernel smoothing for each malnutrition indicator by region
apply_smoothing <- function(x, y, region) {
  # Ensure non-missing values for smoothing
  x <- x[!is.na(x) & !is.infinite(x)]
  y <- y[!is.na(y) & !is.infinite(y)]
  region <- region[!is.na(region)]
  
  # Apply kernel smoothing
  smooth_urban <- locpoly(x[region == "Urban"], y[region == "Urban"], bandwidth = 5, gridsize = length(x))
  smooth_rural <- locpoly(x[region == "Rural"], y[region == "Rural"], bandwidth = 5, gridsize = length(x))
  
  # Combine into a dataframe for each region
  data.frame(
    Mothers_Age = c(smooth_urban$x, smooth_rural$x),
    Proportion = c(smooth_urban$y, smooth_rural$y),
    region = rep(c("Urban", "Rural"), each = length(smooth_urban$x))
  )
}

# Smoothing for each malnutrition indicator
smoothed_wasting <- apply_smoothing(merged_data$V012, merged_data$wasting, merged_data$region)
smoothed_underweight <- apply_smoothing(merged_data$V012, merged_data$underweight, merged_data$region)
smoothed_stunted <- apply_smoothing(merged_data$V012, merged_data$stunted, merged_data$region)
smoothed_any_undernutrition <- apply_smoothing(merged_data$V012, merged_data$any_undernutrition, merged_data$region)

# Combine all smoothed data into a single dataframe for ggplot
plot_data <- bind_rows(
  smoothed_wasting %>% mutate(Indicator = "Wasting"),
  smoothed_underweight %>% mutate(Indicator = "Underweight"),
  smoothed_stunted %>% mutate(Indicator = "Stunted"),
  smoothed_any_undernutrition %>% mutate(Indicator = "Any Undernutrition")
)

# Plot with ggplot in a 2x2 grid layout
ggplot(plot_data, aes(x = Mothers_Age, y = Proportion, color = region)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Indicator, nrow = 2, ncol = 2, scales = "free_y") +  # Create a 2x2 grid
  labs(
    title = "Kernel Smoothing: Malnutrition Indicators by Region (Urban vs Rural)",
    x = "Mother's Age",
    y = "Proportion of Malnutrition"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Urban" = "blue", "Rural" = "red")) +  # Urban and Rural colors
  theme(legend.position = "bottom")


```




```{R}


## Global non parametric test





# Load necessary libraries
library(dplyr)
library(npregfast)
library(gt)

# Assign datasets to meaningful variables (ensure these datasets are loaded in the workspace)
datainD <- EGIR61FL
databir <- EGBR61FL

# Filter for children aged 59 months or less and select relevant columns
databir_sub <- databir %>%
  filter(HW1 <= 59) %>%
  select(CASEID, HW1, HW70, HW71, HW72)

# Select specific columns from datainD dataset, include Father's Occupation (V704) and Mother's Working Status (V731)
dataind_sub <- datainD %>%
  select(CASEID, V704, V731, V025)  # Include V704 for Father's Occupation, V731 for Mother's Working Status, and V025 for area of residence

# Merge the two datasets based on the CASEID
merged_data <- merge(dataind_sub, databir_sub, by = "CASEID")

# Remove rows with missing values in the merged data
merged_data <- na.omit(merged_data)

# Filter out invalid values from the malnutrition-related columns
merged_data <- merged_data %>%
  filter(
    HW70 < 9990,          # Height-for-age Z-score (stunting)
    HW72 < 9990,          # Weight-for-height Z-score (wasting)
    HW71 < 9990           # Weight-for-age Z-score (underweight)
  )

# Create new variables for malnutrition indicators and categorize Father's Occupation and Mother's Working Status
merged_data <- merged_data %>%
  mutate(
    stunted = ifelse(HW70 < -200, 1, 0),         # Stunted (Height-for-age z-score < -2 SD)
    underweight = ifelse(HW71 < -200, 1, 0),     # Underweight (Weight-for-age z-score < -2 SD)
    wasting = ifelse(HW72 < -200, 1, 0),         # Wasting (Weight-for-height z-score < -2 SD)
    any_form_undernutrition = ifelse(stunted == 1 | underweight == 1 | wasting == 1, 1, 0),
    all_form_undernutrition = ifelse(stunted == 1 & underweight == 1 & wasting == 1, 1, 0),
    
    Fathers_occupation = case_when(              # Categorize Father's Occupation
      V704 >= 1 & V704 <= 54 ~ "Service/Business",
      V704 >= 55 & V704 <= 96 ~ "Farmers/Others"
    ),
    
    Mother_working_status = case_when(           # Categorize Mother's Working Status
      V731 == 1 ~ "Working",
      V731 == 0 ~ "Not Working",
      TRUE ~ "Other"
    ),
    
    V025 = factor(V025, levels = c(1, 2), labels = c("Urban", "Rural")),
    Fathers_occupation = factor(Fathers_occupation, levels = c("Service/Business", "Farmers/Others")),
    Mother_working_status = factor(Mother_working_status, levels = c("Not Working", "Working"))
  )

# Define a function to perform the global test for each indicator
perform_global_test <- function(indicator) {
  # Formula for the test
  formula <- as.formula(paste(indicator, "~ V704 : V731 : V025"))
  
  # Run the global test
  test_result <- globaltest(formula, data = merged_data, der = 1, seed = 130853, nboot = 100)
  
  # Return the test results as a data frame row
  data.frame(
    Indicator = indicator,
    Statistic = round(test_result$Statistic, 3),
    P_Value = round(test_result$pvalue, 3),
    Decision = test_result$Decision
  )
}

# Perform the global test for each malnutrition indicator and combine results
test_results <- bind_rows(
  perform_global_test("stunted"),
  perform_global_test("underweight"),
  perform_global_test("wasting"),
  perform_global_test("any_form_undernutrition"),
  perform_global_test("all_form_undernutrition")
)

# Create a formatted gt table for the Global Test results
test_results_gt <- test_results %>%
  gt() %>%
  tab_header(
    title = "Global Test Results for Malnutrition Indicators"
  ) %>%
  cols_label(
    Indicator = "Malnutrition Indicator",
    Statistic = "Test Statistic",
    P_Value = "P-Value",
    Decision = "Decision"
  ) %>%
  fmt_number(
    columns = c(Statistic, P_Value),
    decimals = 3
  ) %>%
  opt_row_striping()  # Adds row striping for better readability

# Display the formatted gt table
print(test_results_gt)


```
