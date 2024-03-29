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


# Relative markers -------------------------------------------------------------

rel_markers <-
  markers %>% 
  separate(marker, c("task", "event")) %>% 
  group_by(id, task) %>% 
  mutate(length = as.numeric(time - first(time))) %>% 
  # group_by(id) %>% 
  mutate(start_time = cumsum(as.numeric(time))) %>% 
  ungroup() %>% 
  filter(!str_detect(event, "stop")) %>% 
  select(-event, -time, -last_marker)

write_csv(rel_markers, "data/00_meta/relative_markers.csv")

# Verify markers ---------------------------------------------------------------

# All id-s have 8 markers (good) there are 65 participants
markers %>% 
  count(id) %>% 
  count(n)

# Check the length of markers
# All times seem to be legit, no serious outliers (good)

markers %>% 
  separate(marker, into = c("task", "event")) %>% 
  arrange(id, task, event) %>% 
  group_by(id, task) %>% 
  summarise(task_length = as_hms(time - lag(time)), .groups = "drop") %>% 
  drop_na(task_length) %>% 
  mutate(task = fct_reorder(task, task_length)) %>% 
  ggplot() +
  aes(y = task, x = task_length, fill = task) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_time(name = NULL, 
               limits = c(0, NA), 
               labels = scales::time_format("%M:%S"), 
               breaks = scales::date_breaks("30 sec"))
