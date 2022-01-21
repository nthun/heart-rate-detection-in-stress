# Cut the files into pieces
library(tidyverse)
library(vroom)

rel_markers <- read_csv("data/00_meta/relative_markers.csv")

raw_ibi <-
    vroom(list.files("data/6_ibi_raw/", full.names = TRUE),
          delim = ",",
          id = "filename",
          col_names = "ibi")
    
ibi <-
    raw_ibi %>% 
    extract(filename, 
            into = c("id", "session"), 
            regex = "^.*IBIs_from_(\\d+.)_(\\w+)_BVP.txt") %>% 
    group_by(id, session) %>% 
    transmute(time = round(cumsum(ibi)/1000),
              ibi) %>% 
    ungroup() %>% 
    # Fill in the missing seconds so it is possible to join
    # This is only relevant for everal very long IBIs (>1 sec) after each other
    # full_join(tibble(time = full_seq(x = .$time, 1)), by = "time") %>% 
    # fill(id, session) %>% 
    arrange(id, session, time)

by_marker <-
    ibi %>%
    left_join(markers, by = c("id", "session", "time")) %>% 
    fill(marker_name, marker, .direction = "down") %>% 
    filter(str_detect(marker_name, "start") & !is.na(ibi)) %>%
    mutate(marker_name = str_remove(marker_name, " start")) %>% 
    rowwise() %>% 
    mutate(filename = str_glue("ibi_{id}_{session}_{marker}.txt")) %>% 
    ungroup() %>% 
    group_nest(filename)
    

path <- "data/7_ibi_by_marker/"

walk2(by_marker$filename, by_marker$data,
      ~write_csv(x = select(.y, ibi), 
                 path = str_glue("{path}/{.x}"),
                 col_names = FALSE))


