# TODO: Add comment
# 
# Author: cordaem
###############################################################################

NULL
#'
#' Generic function for \code{\link{pel...}}: probability distribution fitting with L-Moments
#' 
#' @name pel
#' @title Generic function for \code{\link{pel...}}
#' @param distrib character string indicating the probability distribution to fit 
#' @param lmom,... L-moments and further parameters for \code{\link{pel...}}
#' @param probability_distribution_attrname attribute name for probability distribution
#' @param nmom,sort.data,ratios,trim arguments for \code{\link{samlmu}} (\code{nmom=5} by default). Thay are utilized if argument \code{lmom} is \code{NULL}. 
#' @param x vector containg sample. It is utiled to calculete L-moments in case \code{lmom} is set equal to \code{NULL}.
#' @param indices optional index or tag character vector of the same length of \code{x} used as INDEX for \code{\link{tapply}}. It is used to fit different probability distribution in one sample time series (e. g. months in an year). 
#' @param spi.scale integer value or \code{NA}. If it is greater than 1, \code{x} is filtered with the sum of a generic element of \code{x} and the previous \code{spi.scale-1} ones (e.g. SPI-3,SPI-6, etc. ). Default is \code{NA} (no filtering) which is equivalent to \code{spi.scale=1}.
#' @param correction numeric value correction for the 3rd (and higher) L-moment estimation. Default is \code{NULL} , generally it is not used. It is used and suggested to be \code{10^(-10)} in case of a massive function use with \code{lmom=NULL} (e.g. raster cell or zonal statistics). 
#' @export
#' @rdname pel 
#' 
#' 
#' @return A numeric vector containing the  parameters of the selected probability distribution. It is a list in case of selection of several probability distributions (i.e. \code{length(distrib)>1}). 
#'
#'
#' @seealso \code{\link{pel...}},\code{\link{pelexp}},\code{\link{pelgam}},\code{\link{pelgev}},\code{\link{pelglo}},\code{\link{pelgpa}},
#' 
#' \code{\link{pelgno}},\code{\link{pelgum}},\code{\link{pelkap}},\code{\link{pelln3}},\code{\link{pelnor}},
#' 
#' \code{\link{pelpe3}},\code{\link{pelwak}},\code{\link{pelwei}},
#' 
#' \code{\link{cdf}},\code{\link{qua}}
#' 
#' 
#' 
#' @details \code{pel_x} and \code{pel_lmom} are wrapper functions of \code{\link{pel}} whose first argument is \code{x} or \code{lmom} respectively.
#' 
#' @importFrom lmom  samlmu pelgam pelgev pelglo pelgpa pelgno pelgum pelkap pelln3 pelnor pelpe3 pelwak pelwei 
#' @importFrom stringr str_sub 'str_sub<-'
#' @importFrom stats filter runif
#' @examples
#' 
#'  # Sample L-moments of Ozone from the airquality data
#'  data(airquality)
#'  lmom <- samlmu(airquality$Ozone,nmom=6)
#'  distrib <- "gev"    
#'  # Fit a GEV distribution
#'  out_gev <- pel(distrib=distrib,lmom=lmom)
#' 
#'  distrib <- c("exp","gam","gev","glo","gpa","gno","gum","kap","ln3",
#'  "nor","pe3","wak","wei")
#' 
#'  out_list <- pel(distrib=distrib,lmom=lmom)
#'
#' 
#cdfgam 	Gamma distribution
#cdfgev 	Generalized extreme-value distribution
#cdfglo 	Generalized logistic distribution
#cdfgno 	Generalized normal distribution
#cdfgpa 	Generalized Pareto distribution
#cdfgum 	Gumbel (extreme-value type I) distribution
#cdfkap 	Kappa distribution
#cdfln3 	Three-parameter lognormal distribution
#cdfnor 	Normal distribution
#cdfpe3 	Pearson type III distribution
#cdfwak 	Wakeby distribution
#cdfwei 	Weibull distribution

pel <- function(distrib=c("exp","gam","gev","glo","gpa","gno","gum","kap","ln3","nor","pe3","wak","wei")
,lmom=NULL,probability_distribution_attrname="probability_distrib",x=NULL,nmom=5, sort.data=TRUE, ratios=sort.data, trim=0,indices=NULL,spi.scale=NA,correction=NULL,...) {
  
  ##correction_global <<- correction
	out <- try(stop("Generic Error!!"),silent=TRUE)
	call <- sys.call()
	if (is.null(correction)) correction <- NA
	if (class(x) %in% c("numeric","integer")){
		
		## ADDED BY ECOR ON 2017-06-16
		if (length(spi.scale)<1) spi.scale <- as.numeric(NA)
		if ((class(spi.scale) %in% c("numeric","integer")) & (!is.na(spi.scale))) {
		
			x <- as.vector(stats::filter(x,rep(1,spi.scale[1]),sides=1))
		}
		
		## END ADDED BY ECOR ON 2017-06-16
		

		if (length(indices)==length(x)) {
			
			
				pelxx <- function(x,indices=NULL,...) {
				
				pel(x=x,indices=NULL,...) ## EC 20221016 pel(x=x,indices=NULL,...)
			}
				
				
			out <- tapply(X=x,FUN=pelxx,INDEX=indices,distrib=distrib
					,lmom=lmom,probability_distribution_attrname=probability_distribution_attrname,nmom=nmom, sort.data=sort.data, ratios=sort.data, trim=trim,indices=NULL,correction=correction,...) 
				
			return(out)
				
				
		} else if (!is.null(indices)){
			
			stop("Discrepancy between x and indices")
		}	
				
			
	}
	
	
	if (is.null(lmom)) {
		
	  
		lmom <- lmom::samlmu(x,nmom=nmom, sort.data=sort.data, ratios=ratios, trim=trim)
		
	}
	
	
	if (is.null(distrib)) distrib <- NA
	if (is.na(distrib[1]))  {
		
		stop("Wrong distribution name!!!")
		
	}

	if (length(distrib)!=1) {
		
		names(distrib) <- distrib
		out <- lapply(X=distrib,FUN=pel,lmom=lmom,probability_distribution_attrname=probability_distribution_attrname,...)
		
		return(out)
	} else {
		
		
		distrib <- distrib[1]
		
		
		
	
		if (str_sub(distrib, start = 1, end =3)=="pel") str_sub(distrib, start = 1, end =3) <- ""
		
		fundist <- get(paste0("pel",distrib))
		
		
		
		
		if (!is.na(correction) & ratios) {
		  if (correction<=0.1) correction <- 1-correction  
		  lmom[-c(1,2)][lmom[-c(1,2)]>correction] <- correction 
		}
		##lmom22 <<- lmom
		if (is.na(lmom[2])) lmom[2] <- 0
		## END EC 20221025
		if (lmom[2]==0) {
			
			xxqq <- get(paste0("qua",distrib))(runif(10))
			
			lmom <- samlmu(xxqq,nmom=nmom, sort.data=sort.data, ratios=ratios, trim=trim)
			cond_no_out <- TRUE 
			
		} else {
			
			cond_no_out <- FALSE
		}
		## end EC 20170725
	
		##lmom11 <<- lmom
		
		### https://search.r-project.org/CRAN/refmans/lmomco/html/are.lmom.valid.html
		
		
		out <- fundist(lmom,...)
		
		if (cond_no_out==TRUE) out[] <- NA 
	
		attr(out,probability_distribution_attrname) <- distrib
		
	}


	
	
	return(out)
	
}



NULL
#'
#' @name pel
#' @export
#' @rdname pel
#' @aliases pel_x  
#' 
#' 
pel_x <- function(x,...) {
	
	out <- pel(...,x=x)
	return(out)
}


#'
#' @name pel
#' @export
#' @rdname pel
#' @aliases pel_lmom  
#' 
#' 
pel_lmom <- function(lmom,...) {
	
	out <- pel(...,lmom=lmom)
	return(out)
}


