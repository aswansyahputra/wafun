# Load packages -----------------------------------------------------------

library(readr)     # CRAN v1.3.1
library(dplyr)     # CRAN v1.0.0
library(tibble)    # CRAN v3.0.3
library(tidyr)     # CRAN v1.1.0
library(stringr)   # CRAN v1.4.0
library(lubridate) # CRAN v1.7.9
library(emo)       # [github::hadley/emo] v0.0.0.9000
devtools::load_all()

# Load WhatsApp chat data -------------------------------------------------

wachats_raw <-
  read_lines("data-raw/wachats_kaggle.txt")

# Inspect the raw data ----------------------------------------------------

glimpse(wachats_raw)

# Turn raw data into appropriate format -----------------------------------

wachats <-
  wachats_raw %>%
  enframe(name = NULL, value = "content") %>%
  separate(
    content,
    into = c("datetime", "content"),
    sep = "(?<=\\d{2}/\\d{2}/\\d{4}, \\d{2}:\\d{2}) - ",
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
    content = paste0(content, collapse = "\n"),
    n_lines = n()
  ) %>%
  ungroup() %>%
  filter(str_detect(content, ":")) %>%
  separate(
    content,
    into = c("author", "text"),
    sep = ": ",
    extra = "merge"
  ) %>%
  mutate(
    datetime = dmy_hm(datetime)
  )

glimpse(wachats)

# Glean some features from text -------------------------------------------

wachats_features <-
  wachats %>%
  mutate(
    hour = hour(datetime),
    day = wday(datetime, week_start = 1),
    any_media = str_detect(text, "<Media omitted>"),
    any_emoji = emo::ji_detect(text),
    emoji = emo::ji_extract_all(text),
    n_emojis = emo::ji_count(text),
    n_chars = nchar(text),
    n_words = n_words(text),
    n_nonasciis = n_nonasciis(text),
    n_digits = n_digits(text),
    n_hashtags = n_hashtags(text),
    n_mentions = n_mentions(text),
    n_commas = n_commas(text),
    n_periods = n_periods(text),
    n_exclaims = n_exclaims(text),
    n_caps = n_caps(text),
    n_lowers = n_lowers(text),
    n_urls = n_urls(text),
    n_puncts = n_puncts(text)
  ) %>%
  relocate(n_lines, .before = n_emojis) %>%
  mutate(
    across(
      starts_with("n_"),
      ~ if_else(text == "<Media omitted>", NA_integer_, .x)
    )
  )

glimpse(wachats_features)
