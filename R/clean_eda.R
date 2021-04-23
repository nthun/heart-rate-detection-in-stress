# Clean EDA

library(tidyverse)
library(stringi)
library(vroom)
library(hms)
library(fs)
library(lubridate)
source("R/change.R")
source("R/calculate_auc.R")

# File properties
raw_eda = "data/10_raw_eda"
trimmed_eda = "data/11_trimmed_eda"
dissected_eda = "data/12_eda_marker"

sample_rate = 32L

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

# trimmed_df <-
  raw_df %>% 
  extract(file, 
          into = "id", 
          regex = paste0(raw_eda, "/(.*)_EDA.txt$"),
          remove = FALSE) %>% 
  mutate(id = stri_trans_general(id, "Latin-ASCII") %>% 
           str_to_lower()) %>% 
  group_by(file) %>% 
  # Add a time column so we can join the markers
  mutate(time = as_hms(sample / sample_rate)) %>% 
  left_join(markers, by = c("id", "time")) %>% 
  fill(marker, last_marker, .direction = "down") %>%
  # drop first part without any tasks
  drop_na(marker) %>% 
  # drop the last part from witch phys data are not needed
  filter(marker != last_marker,
         # drop all data that is irrelevant
         str_detect(marker, "_start")) %>% 
  ungroup() %>% 
  mutate(file = paste0(trimmed_eda, "/", id, ".txt"),
         marker = str_remove(marker, "_start")) %>% 
  select(file, marker, time, eda) %>% 
  group_by(file, marker) %>% 
  summarise(eda_auc = 
            eda_increase = change(eda), .groups = "drop")

trimmed_df




