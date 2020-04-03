library(fs)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(ggplot2)

datadir <- dir_create("data")

# Refresh the endmember file
url <- paste0(
  "https://files.osf.io/v1/resources/kp82c/providers/googledrive/",
  "SSerbin_hypertrace_results/instrument_models/kougarok_ang20180815t203458_rfl_v2r2/",
  "?zip="
)
outfile <- path(datadir, "endmembers.zip")
download.file(url, outfile)
unzip(outfile, exdir = datadir)

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
  extract(parentdir, c("endmember", "atm"), "^(.*)_endmembers_(.*)")

dat_long <- dat %>%
  pivot_longer(
    bias:std,
    names_to = "variable",
    values_to = "value"
  )

plt <- ggplot(dat_long) +
  aes(x = snr, y = value, fill = endmember) +
  geom_boxplot() +
  ## ggbeeswarm::geom_quasirandom(
  ##   cex = 1,
  ##   dodge.width = 0.8
  ## ) +
  facet_grid(vars(variable), scales = "free_y")
if (interactive()) plt

figdir <- dir_create("figures")
ggsave(
  path(figdir, "endmembers.png"),
  width = 10, height = 11, dpi = 300
)
