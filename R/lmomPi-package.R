
#' lmomPi: (Precipitation) Frequency Analysis and Variability with L-Moments from lmom
#' 
#'   
#' This packages contains wrapper functions of 'lmom' packages :  
#' {\code{\link{cdf}}}{: generic distribution function;  }  
#' {\code{\link{qua}}}{: generic quantile function;  } 
#' {\code{\link{pel}}}{: fitting of probability distribution function through L-moments. } 
#' 
#' The example functions are illustrated making use of CHIRPS rainfall meteorological data taken: 
#'  
#' Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, 
#' Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, 
#' Andrew Hoell and Joel Michaelsen.
#' "The climate hazards infrared precipitation with stations - a new environmental 
#' record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015 , 
#' \url{https://chc.ucsb.edu/data/chirps }.
#' 
#' The package-provided datasets shall be only used as example datasets. 
#' 
#' The package also contains wrapped functions to  calculate several  indices based on variability (e.g. 'SPI' , Standardized
#' Precipitation Index, see \url{https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi} and \url{http://spei.csic.es}) for multiple time series or spatio-temporal gridded values. 
#' The function  \code{\link{spi.cdf}()} is compared against \code{SPEI::spi()} in SPEI package (\url{https://cran.r-project.org/package=SPEI}). The differences in SPI extimation have order of magnitude averagely about 10^{-8},  due to  the different fitting methods implemented in the two packages. 
#' (see \code{SPEI::spi()} and \code{\link{spi.cdf}()} for more details). 
#' 
#' The development of this package has been sponsored by ACEWATER2 and "Water  for  Growth  and  Poverty Reduction  in  the  Mekrou" projects of the Joint Research Centre of the Europan Commission (\url{https://aquaknow.jrc.ec.europa.eu}).
#' 
#' 
#'
#' @docType package
#' @name lmomPi-Package
#' 
#'
#' 
#' 
NULL


