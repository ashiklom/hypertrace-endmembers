library(fs)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)

figdir <- dir_create("figures/endmembers-2")

statfiles <- here("data", "endmembers_v2") %>%
  list.files(pattern = "statistics.csv", recursive = TRUE,
             full.names = TRUE)
names(statfiles) <- statfiles

statcols <- cols(
  inversion = "c",
  atm = "n", az = "n", zen = "n", vzen = "n",
  snr = "c", bias = "n", rmse = "n", crmsd = "n",
  r = "n", std = "n"
)

dat_raw <- map_dfr(statfiles, read_csv, .id = "file", col_types = statcols)

dat <- dat_raw %>%
  extract(file, "endmember", ".*_rfl_v2r2_(.*)_endmembers_.*") %>%
  mutate(snr = str_extract(snr, "cbe[[:digit:]]+"))

outliers <- dat %>%
  filter(r < 0.75)

labels <- outliers %>%
  mutate(label = glue::glue(
    "{substr(inversion, 0, 1)} az{az} zen{zen} vzen{vzen}"
  ))

dat_long <- dat %>%
  left_join(labels) %>%
  pivot_longer(
    bias:std,
    names_to = "variable",
    values_to = "value"
  )

plt <- ggplot(dat_long) +
  aes(x = snr, y = value, fill = endmember) +
  geom_boxplot(outlier.shape = "x") +
  ## ggrepel::geom_text_repel(aes(label = label), size = 2) +
  ## ggbeeswarm::geom_quasirandom(
  ##   cex = 1,
  ##   dodge.width = 0.8
  ## ) +
  facet_grid(vars(variable), scales = "free_y")

ggsave(
  path(figdir, "endmembers.png"),
  plt,
  width = 10, height = 11, dpi = 300
)

plt2 <- ggplot(anti_join(dat_long, outliers)) +
  aes(x = snr, y = value, fill = endmember) +
  geom_boxplot(outlier.shape = NA) +
  ggbeeswarm::geom_quasirandom(
    alpha = 0.5, cex = 0.2,
    dodge.width = 0.8
  ) +
  facet_grid(vars(variable), scales = "free_y")
ggsave(
  path(figdir, "endmembers-nooutliers.png"),
  plt2,
  width = 10, height = 11, dpi = 300
)

byatm <- dat_long %>%
  ## filter(atm == 3) %>%
  anti_join(outliers) %>%
  ggplot() +
  aes(x = snr, y = value, fill = endmember) +
  geom_boxplot(outlier.shape = NA) +
  ## ggbeeswarm::geom_quasirandom(
  ##   cex = 0.2,
  ##   dodge.width = 0.8
  ## ) +
  facet_grid(vars(variable), vars(atm), scales = "free_y")

ggsave(
  path(figdir, "endmembers-nooutliers-byatm.png"),
  byatm,
  width = 21, height = 11.5, dpi = 200
)

figdir_many <- dir_create(path(figdir, "for-bp-many"))

atm_fig <- function(dat, atm, stat) {
  statsym <- rlang::sym(stat)
  title <- paste0("Atmosphere ", atm)
  fname <- path(figdir_many, sprintf("%s_atm%d.png", stat, atm))
  dat_sub <- dat %>%
    mutate(snr = str_remove(snr, "cbe"),
           atm = factor(atm)) %>%
    filter(vzen == 0, atm == {{atm}})
  stopifnot(nrow(dat_sub) > 0)
  plt <- ggplot(dat_sub) +
    aes(x = snr, y = {{statsym}}) +
    geom_point(aes(shape = inversion)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Instrument model", title = title) +
    facet_grid(
      vars(endmember), vars(zen, az),
      scales = "free_y",
      labeller = labeller(
        endmember = label_value,
        .default = label_both
      )
    ) +
    theme_bw()
  ggsave(fname, plt,
         width = 14, height = 9.8, dpi = 300)
  invisible(fname)
}

for (stat in c("bias", "rmse", "crmsd", "r", "std")) {
  for (atm in 0:9) {
    atm_fig(dat, atm, stat)
  }
}

##################################################
# Do some other summary stats.
if (FALSE){

    ggplot() +
    aes(x = snr, y = bias) +
    geom_point(aes(shape = inversion)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(vars(endmember), vars(zen, az), scales = "free_y") +
    theme_bw()

  ggsave("~/Pictures/atm0-basemap.png")

  ggplot(dat) +
    aes(x = zen, y = az, color = bias) +
    geom_jitter(width = 5, height = 5) +
    scale_color_gradient(low = "green4", high = "red2")

}
