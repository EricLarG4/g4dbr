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

  #data reduction - m/z
  losing.mass <- fat.mass %>%
    filter(mz > min(range.start)) %>%
    filter(mz < max(range.end))

  if (baseline.int > 0) { #filters by intensity if switch is ON
    baseline.filter <- losing.mass %>%
      group_by(oligo, buffer.id, tune, rep) %>%
      filter(mz < base.end) %>%
      filter(mz > base.start) %>%
      summarise(basemean = mean(int)*baseline.int) #intensity threshold (mean times the multiplier)

    fit.mass <- losing.mass %>%
      left_join(baseline.filter, by = c("oligo", "buffer.id", "tune", 'rep')) %>%
      group_by(oligo, buffer.id, tune, rep) %>%
      filter(int > basemean) %>%
      select(-c(basemean))

  } else {

    fit.mass <- losing.mass

  }

  return(fit.mass)
}

