library(fs)
library(here)
library(raster, exclude = "extract")
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(progress)

datadir <- here("data")

# DL link: https://files.osf.io/v1/resources/kp82c/providers/osfstorage/5e878f1ef135350018d52ac7/?zip=

em_dir <- path(datadir, "initial_endmembers")
endmember_dirs <- dir_ls(em_dir)

reflectance_files <- dir_ls(
  endmember_dirs[4],
  regexp = "_reflectance-[[:alnum:]]+$",
  recurse = TRUE
)

read_refl_file <- function(fname, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  fname_file <- path_file(fname)
  fname_parsed <- str_match(fname_file, paste0(
    "atm_([[:digit:].]+)_az_([[:digit:].]+)_zen_([[:digit:].]+)_",
    "vzen_([[:digit:].]+)_",
    "snr_noise_coeff_sbg_([[:alnum:]]+)\\.txt_reflectance-(.*)$"
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

library(ggplot2)

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
  filter(az == 90, zen == 0, sample == 1) %>%
  ggplot() +
  aes(x = wavelength, y = reflectance,
      group = interaction(file, sample),
      color = snr) +
  geom_line() +
  facet_grid(vars(inversion), vars(atm))
