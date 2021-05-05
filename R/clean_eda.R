# Clean EDA

library(tidyverse)
library(stringi)
library(vroom)
library(hms)
library(fs)
library(lubridate)
library(pracma)
source("R/change.R")

theme_set(theme_light())

# File properties
raw_eda = "data/11_raw_eda"
trimmed_eda = "data/12_trimmed_eda"
dissected_eda = "data/13_eda_marker"

sample_rate = 32L
scl_window = 20

# Create dirs when initializing the project
# dir_create(raw_eda)
# dir_create(trimmed_eda)
# dir_create(dissected_eda)

header_length = 14L
footer_length = 3L
pattern = ".txt"

# Read markers
markers <-
  read_csv("data/00_meta/markers.csv")
  
# Read raw data
raw_df <-
  vroom(dir_ls(raw_eda, regexp = pattern),
        id = "file",
        delim = "\t",
        skip = header_length, 
        col_names = c("sample", "eda", "X1", "events", "segments"),
        col_types = "in___")


# Process data before summarize ------------------------------------------------

trimmed_df <-
  raw_df %>% 
  # Prettify the names
  extract(file, 
          into = "id", 
          regex = paste0(raw_eda, "/(.*).txt$"),
          remove = FALSE) %>% 
  mutate(id = stri_trans_general(id, "Latin-ASCII") %>% 
           str_to_lower()) %>% 
  group_by(id) %>% 
  # Add a time column so we can join the markers
  mutate(time = as_hms(row_number() / sample_rate)) %>% 
  left_join(markers, by = c("id", "time")) %>% 
  fill(marker, last_marker, .direction = "down") %>%
  # Drop first part without any tasks
  drop_na(marker) %>%
  # Drop the last part from witch phys data are not needed
  filter(marker != last_marker,
         # drop all data that is irrelevant (comment this out to get full recording)
         str_detect(marker, "_start")) %>% 
  group_by(id, marker) %>% 
  # Add moving average and remove it from the signal
  mutate(scl = movavg(x = eda, n = sample_rate*scl_window, type = "e"),
         # scl = movingaves(x = eda, window = sample_rate*scl_window),
         scr = eda - scl) %>% 
  ungroup() %>% 
  mutate(marker = str_remove(marker, "_start")) %>% 
  select(file, marker, id, time, eda, scr, scl)
  
# Create marker-wise summaries -------------------------------------------------

eda_sum <-
  trimmed_df %>% 
  group_by(id, marker) %>% 
  mutate(sample = row_number()) %>% 
  nest() %>% 
  transmute(
             eda_avg = map_dbl(data, ~mean(.$eda)),
             eda_increase = map_dbl(data, ~change(.$eda)),
             scr_avg = map_dbl(data, ~mean(.$scr)),
             scr_increase = map_dbl(data, ~change(.$scr)),
             scl_avg = map_dbl(data, ~mean(.$scl)),
             scl_increase = map_dbl(data, ~change(.$scl)),
             start = map(data, ~first(.$time)) %>% 
                     unlist(start) %>% 
                     as_hms()) %>% 
  ungroup()

write_csv(eda_sum, "data/08_summarised/eda_long.csv")

# Merge all physiological data
hrv_sum <- read_csv("data/08_summarised/hrd_hrv_long.csv")
phys_data <- left_join(eda_sum, hrv_sum, by = c("id", "marker"))
write_csv(phys_data, "data/08_summarised/phys_long.csv")

# Verify if all participants have all markers (yes)
eda_sum %>% 
  count(id) %>% count(n)


# Sandbox ----------------------------------------------------------------------
library(lmerTest)

lmer(log(eda_avg+1) ~ marker + (1|id), data = eda_sum) %>% 
  summary()

lmer(scl_avg ~ marker + (1|id), data = eda_sum) %>% 
  summary()

lmer(log(eda_increase+1) ~ marker + (1|id), data = eda_sum) %>% 
  summary()

lmer(log(eda_slope+1) ~ marker + (1|id), data = eda_sum) %>% 
  summary()

eda_sum %>% 
  mutate(eda_inc_std = scale(eda_increase) %>% as.numeric()) %>% 
  filter(eda_inc_std > 3 | eda_inc_std < -3)

eda_sum %>% 
  pivot_longer(eda_avg:scl_avg) %>% 
  ggplot() +
  aes(x = log(value+1)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")


