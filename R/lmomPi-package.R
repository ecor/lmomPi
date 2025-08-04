#' lmomPi: (Precipitation) Frequency Analysis and Variability with L-Moments from lmom
#'
#' @description
#' This package contains wrapper functions from the 'lmom' package:
#' \code{\link{cdf}}: generic distribution function;  
#' \code{\link{qua}}: generic quantile function;  
#' \code{\link{pel}}: fitting of probability distribution functions using L-moments.
#'
#' Example functions use CHIRPS rainfall meteorological data:
#' Funk et al. (2015), "The climate hazards infrared precipitation with stations", Scientific Data 2, 150066.  
#' \url{https://chc.ucsb.edu/data/chirps}
#'
#' The package-provided datasets are intended for demonstration purposes only.
#'
#' @details
#' The package also includes functions to calculate variability indices such as the Standardized Precipitation Index (SPI).  
#' See \url{https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi} and \url{http://spei.csic.es}.
#'
#' The function \code{\link{spi.cdf}} is compared against \code{SPEI::spi()} from the SPEI package  
#' (\url{https://cran.r-project.org/package=SPEI}). Differences in SPI estimation are typically on the order of 10^{-8},  
#' due to different fitting methods used in the two packages.
#'
#' This package was developed with support from the ACEWATER2 and "Water for Growth and Poverty Reduction in the Mekrou"  
#' projects of the Joint Research Centre of the European Commission (\url{https://aquaknow.jrc.ec.europa.eu}).
#'
#' @keywords internal "_PACKAGE"
NULL


