library(fs)
library(here)
library(raster, exclude = "extract")
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(progress)
library(fst)
library(furrr, exclude = "values")

datadir <- here("data", "endmembers_v2")

tidy_refl <- function(fname, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  r <- raster::brick(fname)
  bandnames <- names(r)
  wavestr <- str_match(bandnames, "([[:digit:]]+\\.[[:digit:]]+)")[,2]
  waves <- as.numeric(wavestr)
  dat_raw <- raster::getValues(r)
  t(dat_raw) %>%
    `colnames<-`(paste0("sample", seq_len(nrow(dat_raw)))) %>%
    as_tibble(.name_repair = "check_unique") %>%
    mutate(wavelength = waves)
}

orig_reflectance_files <- dir_ls(
  datadir,
  regexp = "reference_reflectance$",
  recurse = TRUE
)

orig_refl_raw <- map_dfr(orig_reflectance_files, tidy_refl, .id = "file")
orig_refl <- orig_refl_raw %>%
  mutate(source = "reference") %>%
  extract(file, "endmember", "v2r2_([[:alnum:]]+)_endmembers") %>%
  select(source, endmember, wavelength, everything())

write_fst(orig_refl, path(datadir, "original.fst"))

reflectance_files <- dir_ls(
  datadir,
  regexp = "_reflectance-[[:alnum:]]+$",
  recurse = TRUE
)

plan("multicore")
## pb <- progress_bar$new(total = length(reflectance_files))
ht_refl_raw <- future_map_dfr(reflectance_files, tidy_refl, .id = "file", .progress = TRUE)
write_fst(ht_refl_raw, path(datadir, "hypertrace-raw.fst"))

ht_refl <- ht_refl_raw %>%
  mutate(source = "hypertrace") %>%
  extract(
    file,
    c("endmember", "atm", "az", "zen", "vzen", "inst", "inversion"),
    paste0(
      "v2r2_([[:alnum:]]+)_endmembers.*",
      "atm_([[:digit:].]+)_",
      "az_([[:digit:].]+)_",
      "zen_([[:digit:].]+)_",
      "vzen_([[:digit:].]+)_",
      "snr_noise_coeff_sbg_([[:alnum:]]+)\\.txt_",
      "reflectance-([[:alnum:]]+)"
    )
  )
write_fst(ht_refl, path(datadir, "hypertrace.fst"))
