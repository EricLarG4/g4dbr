% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epsilondb.R
\docType{data}
\name{epsilondb}
\alias{epsilondb}
\title{Molar extinction coefficients and their neighbouring effects of nucleobases.}
\format{
A data frame with 4 rows and 6 variables:
\describe{
  \item{base}{Base name}
  \item{epsilon}{Molar extinction coefficient at 260 nm, in M-1cm-1}
  \item{Acorr}{Correction factors to the extinction coefficient when the nucleotide in 5' is an A, in M-1cm-1}
  \item{Ccorr}{Correction factors to the extinction coefficient when the nucleotide in 5' is an C, in M-1cm-1}
  \item{Gcorr}{Correction factors to the extinction coefficient when the nucleotide in 5' is an G, in M-1cm-1}
  \item{Tcorr}{Correction factors to the extinction coefficient when the nucleotide in 5' is an T, in M-1cm-1}
}
}
\source{
\url{https://doi.org/10.1016/j.bpc.2007.12.004}
}
\usage{
epsilondb
}
\description{
A dataset containing the molar extinction coefficient of A, C, G, and T, and the correction factors
associated to neighbouring effects, at 260 nm.
}
\keyword{datasets}
