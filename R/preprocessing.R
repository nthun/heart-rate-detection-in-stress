# Preprocessing script ----------------------------------------------

# Install packages if necessary
# install.packages(c("tidyverse", "here", "naniar", "skmir", "janitor", "gt))

# Project setup 
library(tidyverse)
library(googlesheets4)
library(stringi)
library(gt)
library(naniar)
library(skimr)

# Set parameters
outlier_treshold_sd = 3 # Threshold for outliers in SD

renamed_markers <-
  c("baseline" = "Baseline",
    "physical" = "Handgrip",
    "cognitive" = "N-back",
    "mental" = "N-back")

cardioceptive_names <-
  c(ca = "Heartbeat perception",
    hrp = "Heart rate perception")

phys_metrics <- 
  tribble(~name, ~metric,
          "meanhr", "Mean HR",
          "lg_hf_percent", "HF% (log)",
          "lg_rmssd", "RMSSD (log)",
          "lg_eda_avg", "Mean EDA (log)",
          "hf.percent", "HF%",
          "rmssd", "RMSSD",
          "eda.avg", "Mean EDA") %>% 
  mutate(metric = fct_inorder(metric))

skin_avg <- c("lg_eda_avg", "lg_scl_avg", "lg_scr_avg")
heart_metrics <- c("lg_rmssd", "lg_hf_percent", "lg_hf_n_u", "meanhr")

react_vars <-
  c(
    "meanhr_N-back",
    "meanhr_Handgrip",
    "hf.percent_N-back",
    "hf.percent_Handgrip",
    "rmssd_N-back",
    "rmssd_Handgrip",
    "eda.avg_N-back",
    "eda.avg_Handgrip",
    "stai_N-back",
    "stai_Handgrip"
  )


# Data read --------------------------------------------------------------------
# Read raw experiment data from google drive. Need authorization.
experiment_data_raw <- 
  read_sheet("https://docs.google.com/spreadsheets/d/19AcUrnOCxCH_Huu1yQ4JjEuJWljFRzhFQPYIGU1nhj0/edit#gid=57027312") %>% 
  janitor::clean_names() %>% 
  mutate(code = as.character(code))

phys_data <- 
  read_csv(here::here("data/08_summarised/phys_long_merged.csv")) %>% 
  mutate(marker = recode(marker, !!!renamed_markers))

# Experiment data processing
## Calculate cardioreceptive metrics

# Cardioreceptive accuracy, bias, and heart rate perception were calculated using the formulas:  
# - ca_ = 1 - |(hr_XXobj - hr_XXszubj)/ hr_XXobj| 
# - hrp = resting_hr_estimated / resting_hr 

# XX stands for 25,35,50
# With the average calculated for each person. 


# Calculate cardioceptive metrics ----------------------------------------------
cardio <-
  experiment_data_raw %>%
  transmute(id = stri_trans_general(code, "Latin-ASCII") %>%
                 str_to_lower(),
            ca_25 = 1 - abs((hr_25obj - hr_25szubj) / hr_25obj),
            ca_35 = 1 - abs((hr_35obj - hr_35szubj) / hr_35obj),
            ca_50 = 1 - abs((hr_50obj - hr_50szubj) / hr_50obj),
            ca = (ca_25 + ca_35 + ca_50)/3,
            hrp = resting_hr_estimated / resting_hr,
            hrp_group = cut_number(hrp, 
                                   n = 3, 
                                   labels = c("Low HRP",
                                              "Medium HRP",
                                              "High HRP"),
                                   ordered_result = TRUE),
            hbp_group = cut_number(ca, 
                                   n = 3, 
                                   labels = c("Low HBP",
                                              "Medium HBP",
                                              "High HBP"),
                                   ordered_result = TRUE)
  )

# Calculate cronnbach alphas for ca
ca_alpha <-
  cardio %>% 
  select(starts_with("ca_")) %>% 
  psych::alpha() %>% 
  .[["total"]] %>% 
  .[["std.alpha"]]


# Process experiment data ------------------------------------------------------
experiment <-
  experiment_data_raw %>%
  rename(id = code, task_order = mental_order_1_2) %>%
  mutate(
    id = stri_trans_general(id, "Latin-ASCII") %>%
      str_to_lower(),
    sex = recode(sex, `1` = "Male", `2` = "Female"),
    bodyweight = as.numeric(bodyweight),
    bmi = bodyweight / (bodyheight / 100) ^ 2,
    task_order = if_else(task_order == 1,
                         "Cognitive first",
                         "Physical first") %>%
      as.factor()) %>%
  select(-starts_with("marker_")) %>%
  left_join(cardio, by = "id")


# Calculate STAI-S reactivity --------------------------------------------------
stai_reactivity <-
  experiment %>% 
  select(id, stai_s_mental_t0_sum,  stai_s_mental_t1_sum, stai_s_physical_t0_sum, stai_s_physical_t1_sum) %>% 
  transmute(id,
            `stai_react_N-back` = stai_s_mental_t1_sum - stai_s_mental_t0_sum,
            stai_react_Handgrip = stai_s_physical_t1_sum - stai_s_physical_t0_sum)


stai <-
  experiment %>% 
  select(id, ends_with("_sum"), task_order) %>% 
  pivot_longer(cols = -c(id, stai_s_t0_sum, task_order),
               names_to = c("task", "time"),
               names_pattern = "stai_s_(.*)_(.*)_sum") %>% 
  mutate(task = recode(task, !!!renamed_markers))

stai_sum <-
  stai %>% 
  group_by(task, time) %>% 
  summarise(stai_avg = mean(value, na.rm = TRUE),
            stai_sd = sd(value, na.rm = TRUE),
            stai_se = stai_sd/sqrt(n())) %>% 
  mutate(stai_prop = stai_avg/first(stai_avg)) %>% 
  ungroup()


# STAI reliability
stai_wide <-
  experiment %>% 
  select(id, matches("^stai_s_(mental|physical)")) %>% 
  select(-matches("sum$|mental$|physical$"),
         -one_of(c("stai_s_mental_t0", 
                   "stai_s_mental_t1", 
                   "stai_s_physical_t0", 
                   "stai_s_physical_t1")))

stai_wide %>% 
  pivot_longer(-id) %>% 
  extract(name, 
          regex = "stai_s_(.*)_t(.)_(.*)$", 
          into = c("task", "time", "item")) %>% 
  pivot_wider(names_from = c("task", "time"),
              values_from = "value") %>% 
  select(mental_0:physical_1) %>% 
  psych::alpha()


# Psychophysiological data processing ------------------------------------------
# Outlier detection

# We first flag outliers (calculated as outside of `r outlier_treshold_sd` SDs), and check how many can be found in each metric.

outlier_flagged <- 
  phys_data %>% 
  pivot_longer(eda_avg:hf_abs) %>% 
  group_by(name) %>% 
  mutate(outlier = if_else(abs(scale(value)) > outlier_treshold_sd, TRUE, FALSE),
         .before = value) %>% 
  ungroup()

outlier_flagged %>% 
  group_by(name) %>% 
  summarise( n_outlier = sum(outlier, na.rm = TRUE),
             perc_outlier = mean(outlier, na.rm = TRUE)) %>% 
  arrange(-perc_outlier) %>% 
  gt() %>% 
  fmt_percent(perc_outlier) %>% 
  cols_label(name = "Name",
             n_outlier = "N outlier",
             perc_outlier = "% outlier") %>% 
  tab_options(column_labels.font.weight = "bold",
              column_labels.background.color = "lightgrey")

outlier_flagged %>% 
  ggplot() +
  aes(x = value, fill = outlier) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  scale_fill_manual(values = c("lightblue", "red")) +
  labs(x = NULL, y = NULL,
       title = "Histogram of the physiological variables",
       subtitle = str_glue("Outliers (red) are identified as values outside of {outlier_treshold_sd} SD"),
       fill = "Outlier")

## Data cleaning and processing

# We remove outliers, and apply natural log+1 transformation on the values. Values show that the EDA and SCR increase variables should be analyzed using a GLM with exponential distribution.

phys_trans <-
  outlier_flagged %>% 
  # Remove outliers
  filter(!outlier) %>% 
  # Create log transformed values
  mutate(lg = log(value + 1)) %>% 
  pivot_wider(names_from = "name",
              values_from = c(value, lg)) %>% 
  set_names(str_remove(names(.), "value_")) %>% 
  # Join participant data
  left_join(select(experiment, 
                   id, sex, age, bodyfat_percent, bmi, task_order,
                   ca, hrp), 
            by = "id")

phys_trans %>% 
  select(starts_with("lg_")) %>% 
  pivot_longer(everything()) %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(x = NULL, y = NULL,
       title = "Histogram of the log+1 transformed physiological variables",
       subtitle = "Outliers have been removed")


# Calculate physiological reactivity metrics -----------------------------------
# Reactivity values were calculated for HR, HF%, RMSSD, and EDA, by subtracting the baseline (t0) from the post-stress measurement (t1) for each task.


#  Calculate reactivity statistics
reactivity <-
  phys_trans %>% 
  select(id, marker, meanhr, hf_percent, rmssd, eda_avg) %>% 
  pivot_wider(names_from = marker,
              values_from = c("meanhr", "hf_percent", "rmssd","eda_avg")) %>% 
  transmute(id,
            #HR
            `meanhr_react_N-back` = `meanhr_N-back` - meanhr_Baseline,
            meanhr_react_Handgrip = meanhr_Handgrip - meanhr_Baseline,
            # HF
            `hf.percent_react_N-back` = `hf_percent_N-back` - hf_percent_Baseline,
            hf.percent_react_Handgrip = hf_percent_Handgrip - hf_percent_Baseline,
            # RMSSD
            `rmssd_react_N-back` = `rmssd_N-back` - rmssd_Baseline,
            rmssd_react_Handgrip = rmssd_Handgrip - rmssd_Baseline,
            #EDA
            `eda.avg_react_N-back` = `eda_avg_N-back` - eda_avg_Baseline,
            eda.avg_react_Handgrip = eda_avg_Handgrip - eda_avg_Baseline) %>% 
  left_join(stai_reactivity, by = "id")

reactivity %>% 
  pivot_longer(-id) %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram() +
  facet_wrap(~name, scales = "free", nrow = 4) +
  labs(title = "Reactivity histograms of key metrics")



## Missing data analysis

phys_data %>% 
  select(eda_avg:hf_abs) %>% 
  group_by() %>% 
  miss_var_summary()

# Save processed data to files -------------------------------------------------

dir.create("data/processed")

# Physiology
phys_clean <- 
  phys_trans %>% 
  select(-contains("increase")) %>% 
  relocate(sex:hrp, .after = id)
  
write_csv(phys_clean, "data/processed/physiology_clean.csv")
# Create codebook
write_tsv(tibble(variables = names(phys_clean)), 
          "data/processed/physiology_clean_codebook.txt", 
          col_names = FALSE)

# Reactivity measures
write_csv(reactivity, "data/processed/reactivity_clean.csv")
write_tsv(tibble(variables = names(reactivity)), 
          "data/processed/reactivity_codebook.txt", 
          col_names = FALSE)

experiment_clean <-
  experiment %>%
  select(id, sex, age, bodyweight:bodyfat_percent, bmi, max_grip_strength,
         task_order,
         physical_expected_stress:physical_experienced_hr_change,
         mental_expected_stress:mental_experienced_hr_change,
         resting_hr, resting_hr_estimated,
         ca:hbp_group,
         ends_with("sum")
         )

write_csv(experiment_clean, "data/processed/survey_clean.csv")
# Create codebook
write_tsv(tibble(variables = names(experiment_clean)), 
          "data/processed/survey_clean_codebook.txt", 
          col_names = FALSE)
