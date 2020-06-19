#' Mass spectrometry data size reduction
#'
#' @param fat.mass A dataframe with mz, and int columns.
#' @param base.start A numeric value.
#' @param base.end A numeric value, larger than \code{base.start}.
#' @param range.start A numeric value.
#' @param range.end A numeric value, larger than \code{range.start}.
#' @param baseline.int A numeric value.
#' @return Size-reduced MS data.
#' @examples
#' mass.diet(fat.mass = my_MS_data, base.start = 1750, base.end = 2000,
#' range.start = 1000, range.end = 2500, baseline.int = 2)

mass.diet <- function(fat.mass, base.start, base.end, range.start, range.end, baseline.int){

  library(tidyverse)

  #m/z range filtering----
  losing.mass <- fat.mass %>%
    filter(mz > min(range.start)) %>%
    filter(mz < max(range.end))

  #intensity filtering----
  #intensity threshold determination
  if (baseline.int > 0) { #filters by intensity if the coefficient is not 0
    baseline.filter <- losing.mass %>%
      group_by(oligo, buffer.id, tune, rep) %>% #grouping by individual spectra
      filter(mz < base.end) %>% #selection of baseline range
      filter(mz > base.start) %>%
      summarise(basemean = mean(int)*baseline.int) #intensity threshold (mean noise times the multiplier)

    #removal of noise
    fit.mass <- losing.mass %>% #joins threshold to m/z filtered data
      left_join(baseline.filter, by = c("oligo", "buffer.id", "tune", 'rep')) %>%
      group_by(oligo, buffer.id, tune, rep) %>% #group by spectrum
      filter(int > basemean) %>% #filters
      select(-c(basemean)) #removes threshold column

  } else {
    #does nothing if coefficient at 0
    fit.mass <- losing.mass

  }

  return(fit.mass)
}

