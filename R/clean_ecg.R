library(tidyverse)
library(vroom)
library(hms)
library(fs)
library(lubridate)
source("R/downsample.R")

# Setup parallell processing
theme_set(theme_light())

# File properties
raw_dir = "data/00_raw"
downsampled_dir = "data/01_downsampled_ecg/"
driftless_dir = "data/02_driftless_ecg/"

header_length = 14L
footer_length = 3L
pattern = ".txt"

start_rate = 1024L
end_rate = 128L


# Downsample raw data from 1024 Hz to 256 Hz ----------------------------------------

# Read raw data
raw_df <-
  vroom(dir_ls(raw_dir, regexp = pattern, recurse = TRUE), 
        id = "file",
        delim = "\t",
        skip = header_length, 
        # col_names = FALSE,
        col_names = c("sample", "ecg", "ecg2", "ecg3","rsp", "hr"),
        col_types = "in__n_"
        ) %>%  
        # The original time column should be changed
        # group_by(file) %>% 
        # mutate(time = row_number()) %>% 
        ungroup()


# Sandbox ---------------------------------------------------------------------------

df <-
  raw_df %>% 
  gather(var, value, -file, -sample)

df %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

df %>% 
  filter(var == "X5" & sample %in% 15000:35000) %>% 
  ggplot() +
  aes(x = sample, y = value) +
  geom_point()


# Downsampling ----------------------------------------------------------------------
# No need to repeat this, just ise the files from the downsampled library

library(furrr)
plan(multisession(workers = availableCores()))

downsampled_df <-
  raw_df %>%
  group_nest(file) %>%
  mutate(
         ds_file = str_replace(file, raw_dir, downsampled_dir) %>%
                   str_remove("ecg_rest/|ecg_task/"),
         ecg_ds = future_map(data,
                             .progress = TRUE,
                             ~downsample(.x,
                                         variable = "ecg",
                                         from = start_rate,
                                         to = end_rate))
  )

# # Save the files separately to a new library

# dir.create(downsampled_dir)

walk2(downsampled_df$ecg_ds,
      downsampled_df$ds_file,
      ~vroom_write(x = .x,
                   path = .y,
                   na = "",
                   col_names = FALSE))


# Correct movement drift ------------------------------------------------------------

# No need to repeat this, just use the files in the driftless library
# Read the downsampled data
downsampled_df <- vroom(dir_ls(downsampled_dir, regexp = pattern, recurse = TRUE),
                        id = "file",
                        delim = "\t",
                        col_names = c("time", "ecg"))


# Removing movement drift from the data (using 1s window rolling mean)

driftless_df <-
  downsampled_df %>%
  group_by(file) %>%
  transmute(
            # time,
            # Keep only the corrected value as the only variable
            corrected = ecg - caTools::runmean(ecg, end_rate, align = "center")) %>%
  nest() %>%
  mutate(driftless_file = str_replace(file, downsampled_dir, driftless_dir)) %>% 
  ungroup()

dir.create(driftless_dir)

walk2(driftless_df$data,
      driftless_df$driftless_file,
      ~vroom_write(x = .x,
                   path = .y,
                   delim = "/t",
                   na = "",
                   col_names = FALSE))

# Add time to be able to plot this
driftless_df %>% 
  slice(1) %>% 
  unnest(data) %>% 
  mutate(time = row_number()) %>% 
  ggplot() +
  aes(x = time, y = corrected) +
  geom_line() +
  coord_cartesian(xlim = c(0, 5000), ylim = c(-1000, 2000))

# Process the markers ---------------------------------------------------------------

markers <- 
  raw_df %>% 
  filter(str_detect(event, "Light Trigger"))

invalid_markers <-
  raw_df %>% 
  filter(str_detect(event, "Invalid Trigger"))


invalid_markers %>% 
  count(file) %>% 
  mutate(file = str_remove(file, "^.*(?=(/))")) %>% 
  write_tsv("invalid_markers.txt")

markers %>% 
  count(file) %>% 
  right_join(distinct(raw_df, file), by = "file") %>% 
  transmute(file = str_remove(file, "^.*(?=(/))"),
            valid_trigger = if_else(is.na(n), 0L, n)) %>% 
  arrange(file) %>% 
  write_tsv("valid_markers.txt")


markers %>% 
  mutate(time_ds = time/start_rate*end_rate) %>% 
  # count(file) %>% 
  print(n = 100)

markers %>% 
  count(file) %>% 
  count(n)
  ggplot() +
  aes(x = n) +
  geom_histogram() +
  coord_cartesian(xlim = c(0, 150))
  
all_markers <-  
  raw_df %>% 
  drop_na(event)
  
