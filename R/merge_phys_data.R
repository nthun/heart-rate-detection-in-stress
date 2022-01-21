# Merge all physiological data

library(tidyverse)

# 2 cognitive sessions separately
# hrv_sum <- read_csv("data/08_summarised/hrd_hrv_long.csv")
# eda_sum <- read_csv("data/08_summarised/eda_long.csv")

# Single cognitive session
hrv_sum <- read_csv("data/08_summarised/hrv_merged_long.csv")
eda_sum <- read_csv("data/08_summarised/eda_long_merged.csv")

phys_data <- 
  left_join(eda_sum, hrv_sum, by = c("id", "marker")) %>% 
  select(id, marker, start, recordingtime_s, everything())

write_csv(phys_data, "data/08_summarised/phys_long_merged.csv")

# Verify if all participants have all markers
eda_sum %>% 
  count(id) %>% 
  count(n)
