library(fs)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

datadir <- dir_create("data")

## # Refresh the endmember file
## url <- paste0(
##   "https://files.osf.io/v1/resources/kp82c/providers/googledrive/",
##   "SSerbin_hypertrace_results/instrument_models/kougarok_ang20180815t203458_rfl_v2r2/",
##   "?zip="
## )
## outfile <- path(datadir, "endmembers.zip")
## download.file(url, outfile)
## unzip(outfile, exdir = datadir)

statfiles <- dir_ls("data", type = "directory") %>%
  dir_ls(glob = "*statistics.csv")

statcols <- cols(
  inversion = "c",
  atm = "n", az = "n", zen = "n", vzen = "n",
  snr = "c", bias = "n", rmse = "n", crmsd = "n",
  r = "n", std = "n"
)

dat_raw <- map_dfr(statfiles, read_csv, .id = "file", col_types = statcols)

dat <- dat_raw %>%
  mutate(parentdir = path_file(path_dir(file))) %>%
  extract(parentdir, "endmember", "^(.*)_endmembers_.*")

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
figdir <- dir_create("figures")
ggsave(
  path(figdir, "endmembers.png"),
  plt,
  width = 10, height = 11, dpi = 300
)

plt2 <- ggplot(anti_join(dat_long, outliers)) +
  aes(x = snr, y = value, fill = endmember) +
  geom_boxplot(outlier.shape = NA) +
  ggbeeswarm::geom_quasirandom(
    cex = 0.2,
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
  "figures/endmembers-nooutliers-byatm.png",
  byatm,
  width = 21, height = 11.5, dpi = 200
)

##################################################
# Do some other summary stats.
if (FALSE){

  plt

  ggplot(dat) +
    aes(x = zen, y = az, color = bias) +
    geom_jitter(width = 5, height = 5) +
    scale_color_gradient(low = "green4", high = "red2")

}
