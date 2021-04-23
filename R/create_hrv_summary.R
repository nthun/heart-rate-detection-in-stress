# Merge HRV data

library(tidyverse)
library(vroom)
library(janitor)
library(fs)

theme_set(theme_light())

# Read raw data
by_marker_raw <- 
    vroom(dir_ls("data/07_hrv_marker/"),
      id = "file",
      col_names = c("variable", "value")) %>% 
    select(-X3) 


# Process data ----------------------------------------------------------------------

hrv_summary <-
  by_marker_raw %>%
  # Create id variables from filename
  extract(
    file,
    into = c("id", "marker"),
    regex = "data/07_hrv_marker/(.*)-(.*)_hrvResults.txt",
    convert = TRUE) %>%
  # Clean the hrv variable names
  mutate(variable = janitor::make_clean_names(string = variable) %>%
         str_remove("_\\d+$")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(id, marker)


# Save final hrv file ---------------------------------------------------------------

write_excel_csv(hrv_summary, "data/08_summarised/hrd_hrv_long.csv")

hrv <- read_csv("data/08_summarised/hrd_hrv_long.csv")

hrv_summary %>% 
  ggplot() +
  aes(x = rmssd, y = hf_abs) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

hrv_summary %>% 
  select(id, session, rmssd, hf_abs, hf_percent) %>% 
  pivot_longer(cols = c(rmssd, hf_abs, hf_percent),
               names_to = "hrv",
               values_to = "value") %>% 
  ggplot() +
  aes(y = value, x = hrv) +
  geom_boxplot(outlier.alpha = .2) +
  facet_wrap(~hrv, scales = "free")

hrv_summary %>% 
  filter(scale(rmssd) < 3) %>% 
  select(id, marker, rmssd, hf_abs, lf_abs, hf_percent, meanhr, sdnn, recordingtime_s) %>% 
  pivot_longer(cols = -c(id, marker),
               names_to = "hrv",
               values_to = "value") %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram() +
  facet_wrap(~hrv, scales = "free")

hrv_summary %>% 
  ggplot() +
  aes(x = marker, y = hf_percent) +
  geom_boxplot()

library(lmerTest)
library(sjPlot)

hrv

mod <- lmer(log(hf_n_u) ~ marker + (1|id), data = hrv)
tab_model(mod)
  