library(fs)
library(dplyr)
library(tidyr)
library(ggplot2)

noisefiles <- dir_ls(path("data", "noisefiles"))

cols_raw <- readLines(noisefiles[1], 1)
cols_raw2 <- strsplit(trimws(cols_raw), " +")[[1]]
cols <- gsub("#", "", cols_raw2)

noisedata <- lapply(noisefiles, read.table, col.names = cols) %>%
  bind_rows(.id = "file") %>%
  as_tibble()

noisedata %>%
  pivot_longer(a:rmse) %>%
  ggplot() +
  aes(x = wvl, y = value, color = path_file(file)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("figures/noise.png")
