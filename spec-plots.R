library(here)
library(fst)
library(fs)
library(dplyr)
library(ggplot2)

library(tidyr)

badwave <- function(wavelength) {
  (wavelength > 1300 & wavelength < 1500) |
    (wavelength > 1750 & wavelength < 1950) |
    wavelength > 2400
}

datadir <- here("data", "endmembers_v2")
ht_refl <- read_fst(path(datadir, "hypertrace.fst")) %>%
  as_tibble() %>%
  mutate(id = cumsum(wavelength == 380)) %>%
  mutate(
    sample1 = if_else(badwave(wavelength), NA_real_, sample1),
    sample2 = if_else(badwave(wavelength), NA_real_, sample2),
    sample3 = if_else(badwave(wavelength), NA_real_, sample3),
    sample4 = if_else(badwave(wavelength), NA_real_, sample4),
    sample5 = if_else(badwave(wavelength), NA_real_, sample5)
    ) %>%
  mutate_at(vars(atm:vzen), as.numeric)
  ## group_by_at(vars(atm:inversion)) %>%
  ## mutate(id = group_indices()) %>%
  ## ungroup()
orig_refl <- read_fst(path(datadir, "original.fst")) %>%
  as_tibble() %>%
  mutate(
    sample1 = if_else(badwave(wavelength), NA_real_, sample1),
    sample2 = if_else(badwave(wavelength), NA_real_, sample2),
    sample3 = if_else(badwave(wavelength), NA_real_, sample3),
    sample4 = if_else(badwave(wavelength), NA_real_, sample4),
    sample5 = if_else(badwave(wavelength), NA_real_, sample5)
  )

endmember <- "coastalwater"
ht_sub <- ht_refl %>%
  filter(endmember == {{endmember}},
         az == 180, zen == 0)
orig_sub <- orig_refl %>%
  filter(endmember == {{endmember}})
ggplot() +
  aes(x = wavelength, y = sample1) +
  geom_line(aes(group = id, color = inversion), data = ht_sub) +
  geom_line(data = orig_sub, color = "black", size = 1.2) +
  facet_wrap(vars(inst)) +
  ylim(0, 1)
ggsave("figures/coastal-spectra-az180-zen0.png")
