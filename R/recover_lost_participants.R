# TODO

# 1. Check the id-s of participants without hrv data
# Altogether, se should have 65 participants!

library(tidyverse)
library(fs)
library(vroom)

id_in_hrv <- 
  read_csv("data/08_summarised/hrd_hrv_long.csv") %>% 
  distinct(id) %>% 
  pull()

# There are 57 ids in the file, while there should be 60 (without 5 cut files).
# Thus we are missing 2 participants.

id_in_marker <- read_csv("data/00_meta/markers.csv") %>% 
  distinct(id) %>% 
  pull()

setdiff(id_in_marker, id_in_hrv)

# Missing: 
id_missing <- c("labdarugas", "tizenegyes", "melegvagyokmentskiinnen", "enigma")

# 2. Find out why they don't have data, make them have data

# The following files should have be reexported:
# labdarugas, tizenegyes, enigma: too short recording. 

# It seems like the recording was not exported correctly.
# This is missing from the file end: <end of exported RAW data>

# melegvagyokmentskiinnen: the ECG recording is fubar, can't be saved

# 3. Merge data of participants with multiple files
# 3.1 Physically merge the files to new files 

# Let's process these separately
id_split <- c("magnezium|gyongyharmat|mehpempo|toll|0914")


split_files <-
  tibble(file = dir_ls("data/00_raw/")) %>%
  mutate(clean_filename = stringi::stri_trans_general(file, "Latin-ASCII") %>% 
                          str_to_lower()) %>% 
  filter(str_detect(clean_filename, id_split)) %>% 
  mutate(id = str_remove_all(clean_filename, "data/00_raw/|.txt") %>% 
              str_extract("^[a-z]+|^\\d+"),
         data = map(file, ~vroom(.x, 
                                 delim = "\t",
                                 skip = header_length, 
                                 col_names = c("sample", "ecg", "ecg2", "ecg3","rsp", "hr"),
                                 col_types = "in__n_")))


# 3.2 Recalculate the marker times
DONE
# 4. Calculate the HRV values for those participants
# 5. Calculate skin conductance or EDA+change for all participants


