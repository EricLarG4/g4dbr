#' Calculates extinction coefficient from oligonucleotide sequence
#'
#' @param sequence A string.
#' @return The extinction coefficient of \code{sequence}.
#' @examples
#' epsilon.calculator("GGGTTAGGGTTAGGGTTAGGG")

epsilon.calculator <- function(sequence = NULL){

  library(stringr)
  library(tidyverse)

  #extinction coefficient database
  epsilondb <- epsilondb

  #initialization of result data frame
  epsilon.calc <- data.frame()
  buffer <- data.frame()
  result <- data.frame()

  for (i in 1:str_length(sequence)) {
    buffer <- data.frame(position = i,
                         nucleo = substr(sequence, i, i),
                         nn = substr(sequence, i+1, i+1)
    )
    epsilon.calc <- rbind(epsilon.calc, buffer)
  }

  epsilon.calc <- epsilon.calc %>%
    mutate(
      indiv.base = case_when(
        nucleo == 'G' ~ epsilondb$epsilon[epsilondb$base == 'G'],
        nucleo == 'C' ~ epsilondb$epsilon[epsilondb$base == 'C'],
        nucleo == 'T' ~ epsilondb$epsilon[epsilondb$base == 'T'],
        nucleo == 'A' ~ epsilondb$epsilon[epsilondb$base == 'A']
      ),
      nn = case_when(
        nucleo == 'G' ~ case_when(
          nn == 'G' ~ epsilondb$Gcorr[epsilondb$base == 'G'],
          nn == 'C' ~ epsilondb$Ccorr[epsilondb$base == 'G'],
          nn == 'T' ~ epsilondb$Tcorr[epsilondb$base == 'G'],
          nn == 'A' ~ epsilondb$Acorr[epsilondb$base == 'G']
        ),
        nucleo == 'C' ~ case_when(
          nn == 'G' ~ epsilondb$Gcorr[epsilondb$base == 'C'],
          nn == 'C' ~ epsilondb$Ccorr[epsilondb$base == 'C'],
          nn == 'T' ~ epsilondb$Tcorr[epsilondb$base == 'C'],
          nn == 'A' ~ epsilondb$Acorr[epsilondb$base == 'C']
        ),
        nucleo == 'T' ~ case_when(
          nn == 'G' ~ epsilondb$Gcorr[epsilondb$base == 'T'],
          nn == 'C' ~ epsilondb$Ccorr[epsilondb$base == 'T'],
          nn == 'T' ~ epsilondb$Tcorr[epsilondb$base == 'T'],
          nn == 'A' ~ epsilondb$Acorr[epsilondb$base == 'T']
        ),
        nucleo == 'A' ~ case_when(
          nn == 'G' ~ epsilondb$Gcorr[epsilondb$base == 'A'],
          nn == 'C' ~ epsilondb$Ccorr[epsilondb$base == 'A'],
          nn == 'T' ~ epsilondb$Tcorr[epsilondb$base == 'A'],
          nn == 'A' ~ epsilondb$Acorr[epsilondb$base == 'A']
        )
      )
    )

  epsilon.calc$indiv.base[1] = 0
  epsilon.calc$indiv.base[str_length(sequence)] = 0

  result <- sum(epsilon.calc$nn, na.rm = T) - sum(epsilon.calc$indiv.base, na.rm = T)

  return(result)
}
