# Load packages -----------------------------------------------------------

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(hrbrthemes)
library(FactoMineR)
library(janitor)

# Adjust data format for modeling -----------------------------------------

wachats_prep <-
  wachats_features %>%
  group_by(author) %>%
  summarise(
    n_chats = n(),
    across(
      c(hour:any_emoji, n_emojis:n_puncts),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  column_to_rownames("author")

glimpse(wachats_prep)
rownames(wachats_prep)

# Run PCA -----------------------------------------------------------------

wachats_pca <-
  PCA(wachats_prep, graph = FALSE)

plot.PCA(wachats_pca, choix = "ind")
plot.PCA(wachats_pca, choix = "var")

# Run HCPC ----------------------------------------------------------------

wachats_cluster <-
  HCPC(wachats_pca, nb.clust = -1, graph = FALSE)

plot.HCPC(wachats_cluster, choice = "tree")
plot.HCPC(wachats_cluster, choice = "3D.map")
plot.HCPC(wachats_cluster, choice = "map", draw.tree = FALSE)

# Inspect cluster descriptors ---------------------------------------------

wachats_cluster_descriptors <-
  wachats_cluster %>%
  pluck("desc.var", "quanti") %>%
  map_dfr(
    ~ .x %>%
      as_tibble(rownames = "descriptor") %>%
      clean_names(),
    .id = "cluster"
  ) %>%
  mutate(cluster = paste("Cluster", cluster))

ggplot(
  wachats_cluster_descriptors,
  aes(cluster, descriptor, fill = v_test)
) +
  geom_tile() +
  scale_fill_distiller(
    palette = "Spectral",
    direction = 1,
    breaks = -3:3,
    guide = guide_colorbar(barwidth = 20, barheight = 0.8)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "V-test",
    title = "What features that describe a cluster?",
    subtitle = "Positive v-test value signifies that the average value is higher than in the overall data"
  ) +
  theme_ft_rc(
    base_family = font_tw,
    grid = FALSE,
    ticks = TRUE
  ) +
  theme(
    plot.title.position = "plot",
    legend.position = "bottom"
  )
