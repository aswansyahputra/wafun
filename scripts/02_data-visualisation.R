# Load packages -----------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggalt)
library(hrbrthemes)
library(wordcloud2)

# Set up plotting themes --------------------------------------------------

theme_set(
  theme_ft_rc(
    base_family = font_tw,
    grid = FALSE,
    ticks = TRUE
  ) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
)

# Daily number of chats ---------------------------------------------------

wachats_features %>%
  count(
    date = as.Date(datetime)
  ) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line()

wachats_features %>%
  ggplot(aes(x = as.Date(datetime))) +
  geom_line(stat = "count", colour = ft_cols$red) +
  labs(
    x = NULL,
    y = "# chats",
    title = "How many chats are sent per day?",
    caption = "Viz: Muhammad Aswan Syahputra"
  )

# Most active people ------------------------------------------------------

mostactive <-
  wachats_features %>%
  count(
    author,
    sort = TRUE
  ) %>%
  slice_max(n = 4, order_by = n)

wachats_features %>%
  semi_join(mostactive) %>%
  count(
    author,
    hour
  ) %>%
  mutate(
    author = fct_reorder(author, n, sum, na.rm = TRUE)
  ) %>%
  ggplot(aes(hour, author, colour = n)) +
  geom_point(size = 8, alpha = 0.7, show.legend = FALSE) +
  scale_x_continuous(breaks = 0:23) +
  scale_colour_viridis_c(option = "inferno", trans = "log2") +
  labs(
    x = NULL,
    y = NULL,
    title = "At what time the most active user are online?",
    subtitle = "Lighter colour denotes high number of chats",
    caption = "Viz: Muhammad Aswan Syahputra"
  )

# My friends and I --------------------------------------------------------

friends <- c(
  "Shahain",
  "Pankaj Sinha",
  "Sahil Phatania"
)

wachats_features %>%
  filter(author %in% friends) %>%
  mutate(
    week = week(datetime),
    wday = wday(datetime, label = TRUE)
  ) %>%
  group_by(
    wday,
    author
  ) %>%
  summarise(
    n = n() / n_distinct(week)
  ) %>%
  ggplot(aes(wday, n, fill = author)) +
  geom_col(colour = NA, position = "dodge") +
  scale_fill_ft() +
  labs(
    x = NULL,
    y = "# chats/week",
    fill = NULL,
    title = "How often my friends and I send chats to our group?",
    caption = "Viz: Muhammad Aswan Syahputra"
  ) +
  coord_polar()

# Emoji power users -------------------------------------------------------

wachats_emoji_stats <-
  wachats_features %>%
  group_by(author) %>%
  summarise(
    emoji_yes = mean(any_emoji, na.rm = TRUE),
    n_emoji_yes = sum(any_emoji == TRUE),
    emoji_no = 1 - emoji_yes,
    n_emoji_no = sum(any_emoji == FALSE),
    n_chats = n()
  )

wachats_emoji_stats %>%
  filter(emoji_yes == 1)

wachats_emoji_stats %>%
  filter(emoji_no == 1)

wachats_emoji_stats %>%
  filter(emoji_no == 0.5)

wachats_emoji_stats %>%
  filter(
    emoji_yes != 1,
    emoji_no != 1,
    emoji_no != 0.5
  ) %>%
  mutate(
    log_ratio = log2(emoji_yes / emoji_no)
  ) %>%
  group_by(
    sign = sign(log_ratio)
  ) %>%
  slice_max(
    order_by = abs(log_ratio),
    n = 5,
    with.ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    author = fct_reorder(author, log_ratio),
    sign = as.character(sign)
  ) %>%
  ggplot(aes(log_ratio, author, colour = sign)) +
  geom_lollipop(horizontal = TRUE, point.size = 5, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ft_cols$slate) +
  geom_point(x = 0, size = 7, fill = ft_cols$white, pch = 21, show.legend = FALSE) +
  geom_text(aes(label = n_chats), x = 0, family = "Roboto Condensed", size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = -2:2) +
  scale_colour_manual(values = c(
    "-1" = ft_cols$red,
    "1" = ft_cols$green, ft_cols$green
  )) +
  labs(
    x = "Log2 ratio of using emoji in chats",
    y = NULL,
    title = "How often ones using or not using emoji in their chats?",
    subtitle = "One positive point means that emojis are used twice as much",
    caption = "Viz: Muhammad Aswan Syahputra"
  )

# Last but not the least --------------------------------------------------

wachats_features %>%
  filter(any_emoji == TRUE) %>%
  unnest_longer(emoji) %>%
  count(emoji, sort = TRUE) %>%
  wordcloud2(
    fontFamily = "Roboto Condensed",
    backgroundColor = "#1e1e1e"
  )
