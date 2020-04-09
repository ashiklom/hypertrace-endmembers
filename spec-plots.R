library(here)
library(fst)
library(fs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(progress)

datadir <- here("data", "endmembers_v2")
figdir <- dir_create(here("figures", "endmembers-2", "spectra"))

ht_raw <- read_fst(path(datadir, "hypertrace.fst")) %>%
  as_tibble() %>%
  mutate(id = cumsum(wavelength == 380))
orig_raw <- read_fst(path(datadir, "original.fst")) %>%
  as_tibble()

badwave <- function(wavelength) {
  (wavelength > 1300 & wavelength < 1450) |
    (wavelength > 1780 & wavelength < 1950) |
    wavelength > 2450
}

ht_refl <- ht_raw %>%
  pivot_longer(
    sample1:sample5,
    names_to = "sample",
    values_to = "reflectance",
    names_prefix = "sample"
  ) %>%
  mutate(reflectance = if_else(badwave(wavelength), NA_real_, reflectance)) %>%
  mutate_at(vars(atm:vzen), as.numeric) %>%
  select(-source)
orig_refl <- orig_raw %>%
  pivot_longer(
    sample1:sample5,
    names_to = "sample",
    values_to = "reflectance",
    names_prefix = "sample"
  ) %>%
  mutate(reflectance = if_else(badwave(wavelength), NA_real_, reflectance))

plot_endmember <- function(endmember, atm, az, zen, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  title <- sprintf("Endmember: %s, Atmosphere: %d, Azimuth: %.1f, Zenith = %.1f",
                   endmember, atm, az, zen)
  fname <- path(figdir, sprintf(
    "%s_atm%d_az%.1f_zen%.1f.png",
    endmember,
    atm, az, zen
  ))
  ht_sub <- ht_refl %>%
    filter(
      endmember == {{endmember}},
      az == {{az}}, zen == {{zen}}, atm == {{atm}}
    )
  orig_sub <- orig_refl %>%
    filter(endmember == {{endmember}})
  plt <- ggplot() +
    aes(x = wavelength, y = reflectance) +
    geom_line(data = orig_sub, color = "black", size = 1.2) +
    geom_line(aes(group = id, color = inversion), data = ht_sub) +
    facet_grid(
      vars(sample), vars(inst),
      scales = "fixed",
      labeller = labeller(.default = label_value,
                          sample = label_both)
    ) +
    labs(x = "Wavelength (nm)", y = "Reflectance") +
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave(fname, plt, width = 19, height = 9.7, dpi = 200)
  invisible(fname)
}

plot_df <- ht_refl %>%
  distinct(endmember, atm, az, zen) %>%
  arrange(az, zen, atm, endmember)
pb <- progress_bar$new(total = nrow(plot_df))
pmap(plot_df, plot_endmember, .pb = pb)
