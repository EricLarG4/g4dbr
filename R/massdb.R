#' Isotopic masses of atoms typically found in nucleic acids.
#'
#' A dataset containing the isotopic masses of H, C, N, O, P, and K.
#'
#' @format A data frame with 6 rows and 7 variables:
#' \describe{
#'   \item{atom}{element name}
#'   \item{ab1}{abundance of the first isotope, as a frequency}
#'   \item{ab2}{abundance of the second isotope, as a frequency}
#'   \item{ab3}{abundance of the third isotope, as a frequency}
#'   \item{mass1}{atomic mass of the first isotope, in ma/Da}
#'   \item{mass2}{atomic mass of the second isotope, in ma/Da}
#'   \item{mass3}{atomic mass of the third isotope, in ma/Da}
#' }
#' @source \url{https://www.ciaaw.org/isotopic-abundances.htm/}
#' @source \url{https://www.ciaaw.org/atomic-masses.htm}
"massdb"
