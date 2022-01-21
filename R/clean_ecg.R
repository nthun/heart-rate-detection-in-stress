# TODO: make the file name cleaning for the downsampling too

# Setup ------------------------------------------------------------------------
library(stringi)
library(tidyverse)
library(vroom)
library(hms)
library(fs)
library(lubridate)
library(furrr)
source("R/downsample.R")

# Setup parallell processing
plan(multisession(workers = availableCores()))
theme_set(theme_light())

# File properties
raw_dir = "data/00_raw"
downsampled_dir = "data/01_downsampled_ecg"
trimmed_dir = "data/02_trimmed_ecg"
driftless_dir = "data/03_driftless_ecg"
dissected_dir = "data/061_ibi_marker_merged"

header_length = 14L
footer_length = 3L
pattern = ".txt"

start_rate = 1024L
end_rate = 128L

# Read markers
# markers <- 
#   read_csv("data/00_meta/markers.csv")
markers <- 
  read_csv("data/00_meta/relative_markers_merged.csv")


# 1. Downsample raw data from 1024 Hz to 128 Hz -------------------------------------
# No need to repeat this, just use the files from the downsampled library
raw_df <-
  vroom(dir_ls(raw_dir, regexp = pattern), 
        id = "file",
        delim = "\t",
        skip = header_length, 
        # col_names = FALSE,
        col_names = c("sample", "ecg", "ecg2", "ecg3", "rsp", "hr", "events", "segments"),
        col_types = "in__n____"
        ) %>%  
        # The original time column should be changed
        # group_by(file) %>% 
        # mutate(time = row_number()) %>% 
        ungroup()


# Temporary area ---------------------------------------------------------------

id_split <- c("magnezium|gyongyharmat|mehpempo|toll|0914")


raw_df <-
  tibble(file = dir_ls("data/00_raw/", type = "file")) %>%
  mutate(clean_filename = stringi::stri_trans_general(file, "Latin-ASCII") %>% 
           str_to_lower()) %>% 
  filter(str_detect(clean_filename, id_split)) %>% 
  mutate(id = str_remove_all(clean_filename, "data/00_raw/|.txt") %>% 
              str_extract("^[a-z]+|^\\d+"),
         data = map(file, ~vroom(.x, 
                                 delim = "\t",
                                 skip = header_length, 
                                 col_names = c("sample", "ecg", "ecg2", "ecg3","rsp", "hr"),
                                 col_types = "in__n_"))) %>% 
  unnest(data)


downsampled_df <-
  raw_df %>%
  select(-rsp) %>%
  group_nest(file) %>%
  mutate(
         ds_file = str_replace(file, raw_dir, downsampled_dir),
         ecg_ds = future_map(data,
                             .progress = TRUE,
                             ~downsample(.x,
                                         variable = "ecg",
                                         from = start_rate,
                                         to = end_rate))
  )

# Save the files separately to a new library
dir.create(downsampled_dir)

walk2(downsampled_df$ecg_ds,
      downsampled_df$ds_file,
      ~vroom_write(x = .x,
                   path = .y,
                   na = "",
                   col_names = FALSE))


# 2. Cut the unused parts of the driftless data to speed up data ECG veri --------

# Read the downsampled data
downsampled_df <- vroom(dir_ls(downsampled_dir, regexp = pattern, recurse = TRUE),
                        id = "file",
                        delim = "\t",
                        col_names = c("time", "ecg"))
# downsampled_df <-
#   downsampled_df %>%
#   select(file = ds_file, ecg_ds) %>%
#   unnest(ecg_ds) %>%
#   rename(ecg = value)

trimmed_df <-
  downsampled_df %>% 
  extract(file, 
          into = "id", 
          regex = str_glue("{downsampled_dir}/(.*)\\.txt"),
          remove = FALSE) %>% 
  # Remove accented characters from names, so markers can be matched
  mutate(id = stri_trans_general(id, "Latin-ASCII") %>% 
              str_to_lower()) %>% 
  group_by(file) %>% 
  # Add a time column so we can join the markers
  mutate(time = as_hms(row_number() / end_rate)) %>% 
  left_join(markers, by = c("id", "time")) %>% 
  fill(marker, last_marker, .direction = "down") %>% 
  # drop first part without any tasks
  drop_na(marker) %>% 
  # drop the last part from witch phys data are not needed
  filter(marker != last_marker,
  # drop all data that is irrelevant
         str_detect(marker, "_start")) %>% 
  ungroup() %>% 
  mutate(file = str_glue("{trimmed_dir}/{id}.txt"),
         marker = str_remove(marker, "_start")) %>% 
  select(file, marker, time, ecg) %>% 
  nest_by(file)

trimmed_df %>% 
  slice(1) %>% 
  unnest(data)

# Save the trimmed ECG data to the trimmed folder
dir_create(trimmed_dir)

walk2(trimmed_df$data,
      trimmed_df$file,
      ~write_tsv(x = .x,
                 file = .y,
                 na = ""))

# 3. Correct movement drift ------------------------------------------------------
# No need to repeat this, just use the files in the driftless library
trimmed_df <- vroom(dir_ls(trimmed_dir, regexp = pattern),
                    id = "file",
                    delim = "\t")

# trimmed_df <-
#   trimmed_df %>%
#   unnest(data)

# Removing movement drift from the data (using 1s window rolling mean)
driftless_df <-
  trimmed_df %>%
  group_by(file, marker) %>%
  # Keep the corrected value as the only variable
  transmute(corrected = ecg - caTools::runmean(ecg, end_rate, align = "center")) %>%
  group_by(file) %>% 
  select(-marker) %>% 
  nest() %>%
  mutate(driftless_file = str_replace(file, trimmed_dir, driftless_dir)) %>% 
  ungroup()

driftless_df %>% 
  slice(1) %>% 
  unnest(data)

dir.create(driftless_dir)

walk2(driftless_df$data,
      driftless_df$driftless_file,
      ~vroom_write(x = .x,
                   path = .y,
                   delim = "/t",
                   na = "",
                   col_names = FALSE))

# Verify if data still has PQRST complex
driftless_df %>% 
  slice(1) %>% 
  unnest(data) %>% 
  mutate(time = row_number()) %>% 
  ggplot() +
  aes(x = time, y = corrected) +
  geom_line() +
  coord_cartesian(xlim = c(0, 5000), ylim = c(-1000, 2000))

# 4. MANUAL STEP: DO THE PEAK RECOGNITION USING ARTIIFACT ----------------------
# Manual verification and correction if needed

# 5. BATCH PROCESING IN ARTIIFACT: Artifact detection and removal --------------
# Parameters
# Detection: Berntson algorithm
# Interpolation: Cubic spline

# 6. Cut the files into 4 using the markers ------------------------------------

rel_markers <- 
  read_csv("data/00_meta/relative_markers_merged.csv")

ibi_raw <-
  vroom(dir_ls(path = "data/05_ibi_artifacts",
               regex = "IBI_artifacts_IBIs_from_.*"),
        delim = ",",
        id = "file",
        skip = 1,
        col_names = c("ibi", "artifact"))

ibi_corrected <-
  vroom(dir_ls(path = "data/05_ibi_artifacts",
               regex = "IBI_artifactCorrected_IBIs_from_.*"),
        delim = ",",
        id = "file",
        skip = 1,
        col_names = "ibi_corrected")

# TODO: Extract information about artifacts corrected

ibi <-
  ibi_raw %>% 
  bind_cols(select(ibi_corrected, ibi_corrected)) %>% 
  extract(file, 
          into = "id", 
          regex = "^.*IBI_artifacts_IBIs_from_(.*).txt.csv") %>% 
  group_by(id) %>% 
  mutate(time = round(cumsum(ibi)/1000)) %>% 
  complete(time = full_seq(c(0, time), period = 1, tol = 1)) %>% 
  left_join(rel_markers, by = c("id", "time" = "start_time")) %>% 
  fill(task) %>% 
  arrange(id, time) %>% 
  drop_na(ibi) %>% 
  ungroup()
  
dissected_df <-
  ibi %>% 
  select(-artifact, -time, -ibi) %>% 
  nest_by(id, task) %>% 
  mutate(file = here::here(dissected_dir) %>% 
                str_glue("/{id}-{task}.txt")) %>% 
  ungroup()

dissected_df %>% slice(1) %>% unnest(data)

walk2(dissected_df$data,
      dissected_df$file,
      ~write_tsv(x = .x,
                 file = .y,
                 na = "", 
                 col_names = FALSE))


# 7. BATCH PROCESSING: Calculate HRV properties -------------------------------------


