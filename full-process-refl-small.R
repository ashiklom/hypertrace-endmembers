library(fs)
library(here)
library(raster, exclude = "extract")
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(progress)
library(fst)
library(ggplot2)

datadir <- here("data", "reference_small", "reference_small",
                "data", "output")
reflectance_files <- dir_ls(
  datadir,
  regexp = "_state-[[:alnum:]]+$",
  recurse = TRUE
)

reflectance_files <- str_subset(reflectance_files, "az_90") %>%
  str_subset("zen_0")

read_refl_file <- function(fname, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  fname_file <- path_file(fname)
  fname_parsed <- str_match(fname_file, paste0(
    "atm_([[:digit:].]+)_az_([[:digit:].]+)_zen_([[:digit:].]+)_",
    "vzen_([[:digit:].]+)_",
    "snr_noise_coeff_sbg_([[:alnum:]]+)\\.txt_[[:alpha:]]+-(.*)$"
  ))
  r <- raster::brick(fname)
  bandnames <- names(r)
  wavestr <- str_match(bandnames, "([[:digit:]]+\\.[[:digit:]]+)\\.nm\\.$")[,2]
  waves <- as.numeric(wavestr)
  dat_raw <- raster::getValues(r)
  dat <- t(dat_raw) %>%
    `colnames<-`(paste0("sample", seq_len(nrow(dat_raw)))) %>%
    as_tibble(.name_repair = "check_unique") %>%
    mutate(
      wavelength = waves,
      atm = as.numeric(fname_parsed[,2]) ,
      az = as.numeric(fname_parsed[,3]),
      zen = as.numeric(fname_parsed[,4]),
      vzen = as.numeric(fname_parsed[,5]),
      snr = fname_parsed[,6],
      inversion = fname_parsed[,7]
    ) %>%
    pivot_longer(
      starts_with("sample"),
      names_to = "sample",
      values_to = "reflectance",
      names_prefix = "sample",
      names_ptypes = list(sample = numeric())
    ) %>%
    select(sample, wavelength, everything())
  dat
}

pb <- progress_bar$new(total = length(reflectance_files))
refldat <- map_dfr(reflectance_files, read_refl_file, .id = "file",
                   .pb = pb)
write_fst(refldat, path(datadir, "simple-reflectance-state.fst"))

refl_sub <- refldat %>%
  mutate(
    reflectance = if_else(wavelength > 1300 & wavelength < 1500,
                          NA_real_, reflectance),
    reflectance = if_else(wavelength > 1750 & wavelength < 2000,
                          NA_real_, reflectance),
    reflectance = if_else(wavelength > 2450,
                          NA_real_, reflectance)
  )

refl_sub %>%
  filter(az == 90, zen == 0, inversion == "simple") %>%
  ggplot() +
  aes(x = wavelength, y = reflectance,
      group = interaction(file, sample),
      color = snr) +
  geom_line() +
  facet_grid(vars(sample), vars(atm))

ggsave("~/Downloads/spectra-simple.png")
