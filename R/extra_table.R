# Create missing descriptive table
# STAI, várt és észlelt HR, várt stressz - erre kell egy táblázat, meg egy másik korrelációs táblázat
library(gt)

subj <-
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
  pivot_longer(cols = -c("id"),
               names_to = c("metric", NA,"task"),
               names_pattern = "^(.*)_(.*)_(.*)",
               values_to = "value")

stai_df <-
  stai_sum %>% 
  transmute(task,
            metric = "STAI-S",
            measurement = if_else(time == "t0", "Before", "After"),
            Mean = stai_avg,
            SD = stai_sd) %>% 
  unite(metric, c("metric", "measurement"), sep = " ") %>% 
  pivot_wider(names_from = "task", values_from = c("Mean", "SD"))

subj %>% 
  group_by(task, metric) %>% 
  summarise(Mean = mean(value, na.rm = TRUE),
            SD = sd(value, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = "task", values_from = c("Mean", "SD")) %>% 
  bind_rows(stai_df, .) %>% 
  gt() %>% 
  fmt_number(c(Mean_Handgrip, `Mean_N-back`, SD_Handgrip, `SD_N-back`), decimals = 1) %>%
  cols_merge(columns = c("Mean_Handgrip", "SD_Handgrip"), pattern = "{1} ({2})") %>%
  cols_merge(columns = c("Mean_N-back", "SD_N-back"), pattern = "{1} ({2})") %>%
  cols_label("metric" = "",
             "Mean_Handgrip" = "Handgrip	",
             "Mean_N-back" = "N-back")




# Third, we hypothesized that expected stress, physiological reaction, and cardioceptive accuracy would predict perceived stress







  