## Original test dataset was provided by Sarthak Nautiyal at https://www.kaggle.com/sarthaknautiyal/whatsappsample. Some modification was carried out first and the codes to do so are written below.

library(tidyverse)
library(lubridate)
library(glue)

wachats_kaggle_raw <- 
  read_lines("data-raw/datasets_814_1503__chat.txt")

wachats_kaggle <- 
  wachats_kaggle_raw %>% 
  str_replace(pattern = "(?<=A|P)M:", "M -") %>% 
  str_replace("image omitted", "Media omitted") %>% 
  str_replace("video omitted", "Media omitted") %>% 
  enframe(name = NULL, value = "content") %>% 
  separate(
    content,
    into = c("datetime", "content"),
  sep = "(?<=\\d{1,2}/\\d{1,2}/\\d{2}, \\d{1,2}:\\d{2}:\\d{2} [AP]M) - ",
  fill = "left"
) %>% 
  mutate(
    chatid = case_when(
      !is.na(datetime) ~ row_number(),
      TRUE ~ NA_integer_
    )
  ) %>% 
  fill(chatid, datetime) %>%
  group_by(chatid) %>% 
  summarise(
    datetime = unique(datetime),
    content = paste0(content, collapse = "\n")
  ) %>% 
  ungroup() %>% 
  mutate(
    datetime = dmy_hms(datetime),
    datetime = format(datetime, format = "%d/%m/%Y, %H:%M")
  ) %>% 
  transmute(
    content = glue("{datetime} - {content}", .na = NULL)
  ) %>% 
  deframe()
  
# wachats_kaggle <- 
  # wachats_kaggle_raw %>% 
  # str_replace(pattern = "(?<=A|P)M:", "M -") %>% 
  # str_replace("image omitted", "Media omitted") %>% 
  # str_replace("video omitted", "Media omitted") %>% 
  # enframe(name = NULL, value = "content") %>% 
  # filter(nchar(content) != 0) %>% 
  # separate(
  #   content,
  #   into = c("datetime", "content"),
  #   sep = " - ",
  #   fill = "left"
  # ) %>% 
  # mutate(
  #   chatid = case_when(
  #     !is.na(datetime) ~ row_number(),
  #     TRUE ~ NA_integer_
  #   )
  # ) %>% 
  # fill(chatid, datetime) %>% 
  # group_by(chatid) %>% 
  # summarise(
  #   datetime = unique(datetime),
  #   content = paste0(content, collapse = "\n")
  # ) %>% 
  # ungroup() %>% 
  # mutate(
  #   datetime = dmy_hms(datetime),
  #   datetime = format(datetime, format = "%d/%m/%Y, %H:%M")
  # ) %>% 
  # drop_na(datetime) %>% 
  # transmute(
  #   content = glue("{datetime} - {content}", .na = NULL)
  # ) %>% 
  # deframe()

write_lines(wachats_kaggle, "data-raw/wachats_kaggle.txt")

usethis::use_data(wachats_kaggle, overwrite = TRUE, internal = TRUE)
