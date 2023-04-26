# Create missing descriptive table
# STAI, várt és észlelt HR, várt stressz - erre kell egy táblázat, meg egy másik korrelációs táblázat
library(tidyverse)
library(gt)
library(correlation)

metric_order <- c("STAI-S Before", "STAI-S After", "Expected stress", "Perceived stress", "Expected HR", "Perceived HR")

stai_sum_wide <-
  experiment %>% 
  select(id, 
         `STAI-S Before_N-back` = stai_s_mental_t0_sum,  
         `STAI-S After_N-back` = stai_s_mental_t1_sum, 
         `STAI-S Before_Handgrip` = stai_s_physical_t0_sum, 
         `STAI-S After_Handgrip` = stai_s_physical_t1_sum)

subj_wide <-
  experiment %>% 
  select(id, 
         `Expected stress_change_N-back` = mental_expected_stress,
         `Expected stress_change_Handgrip`= physical_expected_stress,
         `Perceived stress_change_N-back` = mental_experienced_stress,
         `Perceived stress_change_Handgrip` = physical_experienced_stress,
         `Expected HR_change_N-back` = mental_expected_hr_change,
         `Expected HR_change_Handgrip` = physical_expected_hr_change,
         `Perceived HR_change_N-back` = mental_experienced_hr_change,
         `Perceived HR_change_Handgrip` = physical_experienced_hr_change) %>% 
  left_join(stai_sum_wide, by = "id")

subj_long <-
  subj_wide %>% 
  pivot_longer(cols = -c("id"),
               names_to = c("metric", NA,"task"),
               names_pattern = "^(.*)_(.*)_(.*)",
               values_to = "value")

  # Third, we hypothesized that expected stress, physiological reaction, and cardioceptive accuracy would predict perceived stress

# Correlations of subj metrics

handgrip_cor <-
  subj_wide %>% 
  select(ends_with("_Handgrip")) %>%
  rename_with(~str_remove(., "_change")) %>% 
  correlation(redundant = TRUE) %>% 
  summary(redundant = TRUE) %>% 
  as_tibble() %>% 
  separate(Parameter, into = c("metric", "task"), sep = "_") %>% 
  rename_with(~str_remove(., "_Handgrip"))

nback_cor <-
  subj_wide %>% 
  select(ends_with("_N-Back")) %>%
  rename_with(~str_remove(., "_change")) %>% 
  correlation(redundant = TRUE) %>% 
  summary(redundant = TRUE) %>% 
  as_tibble() %>% 
  separate(Parameter, into = c("metric", "task"), sep = "_") %>% 
  rename_with(~str_remove(., "_N-back"))

all_cor <-
  bind_rows(handgrip_cor, nback_cor)

subj_long %>% 
  group_by(task, metric) %>% 
  summarise(Mean = mean(value, na.rm = TRUE),
            SD = sd(value, na.rm = TRUE), .groups = "drop") %>% 
  bind_rows(stai_df) %>% 
  left_join(all_cor, by = c("task", "metric")) %>% 
  mutate(metric = fct_relevel(metric, !!!metric_order)) %>% 
  arrange(task, metric) %>% 
  select(task, metric, Mean, SD, all_of(metric_order)) %>% 
  gt() %>% 
  fmt_number(c("Mean", "SD"), decimals = 1) %>% 
  fmt_number(`STAI-S Before`:`Perceived HR`, decimals = 2)
  




