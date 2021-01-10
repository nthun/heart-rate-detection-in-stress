# Process the markers 
library(tidyverse)
library(hms)
library(googlesheets4)

# Read experiment file
experiment_data_raw <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/19AcUrnOCxCH_Huu1yQ4JjEuJWljFRzhFQPYIGU1nhj0/edit#gid=57027312") %>% 
  janitor::clean_names() %>% 
  mutate(code = as.character(code)) %>% 
  drop_na(code)

# Clean markers and put into long format
markers <-
  experiment_data_raw %>%
  mutate(across(starts_with("marker_"), as_hms)) %>% 
  select(id = code, 
         # datafiles, 
         starts_with("marker_")) %>% 
  mutate(id = stri_trans_general(id, "Latin-ASCII") %>% 
           str_to_lower()) %>% 
  pivot_longer(cols = starts_with("marker_"), 
               names_to = "marker",
               names_pattern = "marker_(.*)",
               values_to = "time", 
               values_drop_na = TRUE) %>% 
  arrange(id, time) %>% 
  # Extract the last marker, so we will know the point from 
  # which we can discard the data
  group_by(id) %>% 
  mutate(last_marker = last(marker)) %>% 
  ungroup()

write_csv(markers, "data/00_meta/markers.csv")

