NULL
#' Generic function for \code{cdf...}: probability distribution fitting with L-Moments. 
#' 
#' 
#' @name cdf 
#' @description These functions compute value(s) of cumulated probability or SPI-like (normal standardize) index from a sample or time series of \code{x}. 
#' 
#' 
#' @param x,para,... L-moments and further parameters for \code{cdf...} and \code{\link{cdf}}
#' @param probability_distribution_attrname attribute name for probability distribution
#' @param indices vector of string working as factors or indices, e g. the month names or similar. It must be of the same length of \code{x} or the length equal to 1 other \code{NULL}, if not used. If used, it computes \code{cdf} for each factor. 
#' @param return.as.spi logical parameter. Default is \code{FALSE}. If it is \code{TRUE} probability value is transformed to a normalized random variable through standard \code{\link{qnorm}}, as for Standard Precipitation Index (SPI) (\url{https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi}).
#' @param spi.scale integer value or \code{NA}. If it greater than 1 \code{x} is filtered with the sum of a generic element of \code{x} and the previous \code{spi.scale-1} ones (e.g. SPI-3,SPI-6, etc. ). Default is \code{NA} (no filtering) which is equivalent to \code{spi.scale=1}.
#' @export
#' 
#' @return A vector of cumulated probability value(s) or SPI-like Gaussianized values. It is a list of vectors in case of several probability parametric distribution functions (i.e. \code{para} is a list and \code{length(para)>1}). 
#'
#'
#' @rdname cdf
#' @seealso \code{\link{pel}},\code{\link{cdfexp}},\code{\link{cdfgam}},\code{\link{cdfgev}},\code{\link{cdfglo}},
#' 
#' \code{\link{cdfgpa}},\code{\link{cdfgno}},\code{\link{cdfgum}},\code{\link{cdfkap}},\code{\link{cdfln3}},\code{\link{cdfnor}},\code{\link{cdfpe3}},\code{\link{cdfwak}},\code{\link{cdfwei}} 	
#' 
#' @importFrom lmom  cdfgam cdfgev cdfglo cdfgpa cdfgno cdfgum cdfkap cdfln3 cdfnor cdfpe3 cdfwak cdfwei 
#' @importFrom stats qnorm 
# @importFrom stringr str_sub str_sub<-
#' @examples
#' 
#' # Sample L-moments of Ozone from the airquality data
#' data(airquality)
#' lmom <- samlmu(airquality$Ozone,nmom=6)
#' 
#' distrib <- c("exp","gam","gev","glo","gpa","gno","gum","kap",
#' "ln3","nor","pe3","wak","wei")
#' 
#' para_list <- pel(distrib=distrib,lmom=lmom)
#' cdf_list <- cdf(para=para_list,x=airquality$Ozone)
#' 
#'  \donttest{
#'  library(rasterList)
#'  precff <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
#' 	precs <- stack(precff)
#'  prec_point <- precs[143][,]
#' 
#'  ## month index 
#'  month <- as.character(as.Date(names(prec_point),format="X%Y.%m.%d"),format="M%m")
#'  prec_point[(prec_point<1) & (month=="M01")] <- 0
#' 
#'  distrib_prec2 <- c("gam")
#'  para_vvv <- pel(x=prec_point,indices=month,distrib=distrib_prec2)
#'  cdf_      <- cdf(x=prec_point,indices=month,para=para_vvv)      
#' 	spi_      <- spi.cdf(x=prec_point,indices=month,para=para_vvv)  
#' 
#'  ####  
#'
#'  para_raster <- rasterList(precs,FUN=pel_x,indices=month,distrib=distrib_prec2)
#'  spi1_raster <- stack(RasterListApply(x=rasterList(precs),para=para_raster,
#'                 indices=list(month),FUN=spi.cdf))
#'
#'   para_raster_spi3 <- rasterList(precs,FUN=pel_x,indices=month,distrib=distrib_prec2,
#' 						spi.scale=3)
#'   spi3_raster <- stack(RasterListApply(x=rasterList(precs),para=para_raster_spi3,
#'                 indices=list(month),spi.scale=3,FUN=spi.cdf))
#' }
#' 
#' 
#' ## Comparison with the SPI/SPEI algorithms:  'SPEI::spi' ('SPEI' package)
#' library(SPEI)
#' \donttest{
#' data(wichita)
#' 
#' distrib_wichita <- 'pe3'
#' spi.scale <- 1
#' 
#' month_wichita <- sprintf("M%02d",wichita$MONTH)
#' para_whichita  <- pel(x=wichita$PRCP,indices=month_wichita,distrib=distrib_wichita,
#' 						spi.scale=spi.scale)
#' spi_wichita   <- spi.cdf(x=wichita$PRCP,indices=month_wichita,para=para_whichita,
#' 						spi.scale=spi.scale)
#' spi_wichita_speipkg   <- spi(data=wichita$PRCP,distrib='PearsonIII',scale=spi.scale)
#' difference <- spi_wichita-spi_wichita_speipkg$fitted
#' 
#' }



cdf <- function(para,x,probability_distribution_attrname="probability_distrib",indices=NULL,return.as.spi=FALSE,spi.scale=NA,...) {
	
	
	
	
	out <- try(stop("Generic Error!!"),silent=TRUE)
	
	
	## ADDED BY ECOR ON 2017-06-16
	if (length(spi.scale)<1) spi.scale <- as.numeric(NA)
	if ((class(spi.scale) %in% c("numeric","integer")) & !(is.na(spi.scale))) {
		
		x <- as.vector(stats::filter(x,rep(1,spi.scale[1]),sides=1))
	}
	## END ADDED BY ECOR ON 2017-06-16	
	

	
	
	if (is.null(para)) para <- NA

	if (is.list(para)) {
		ccc <- all(names(para) %in% unique(indices))	
		
		####names(distrib) <- distrib
		if (all(names(para) %in% unique(indices))) {
			if (length(x)==length(indices)) {
				
				iid <- tapply(X=1:length(x),INDEX=indices,simplify=FALSE,FUN=function(t){t})
				
			} else if (length(indices)<=1) {
				
				iid <- list(1:length(x))
				names(iid) <- "no_index"
				
			}	else { 
			
				stop("Wrong length for indices!!!")
			}
				
				
		} else {
			
			iid <- list(1:length(x))
			names(iid) <- "no_index"
			
			para <- list(no_index=para)
			
			
			
		}
		
		out <- list()
	
		for (iin in names(iid)) {
			wi <- iid[[iin]]
			if (!is.list(para[[iin]])) {
				### para[[iin]] must be a list!!!
				distrib <- attr(para[[iin]],probability_distribution_attrname) 
				para[[iin]] <- list(val=para[[iin]])
				names(para[[iin]]) <- distrib
				
			}
		
			out[[iin]] <- lapply(X=para[[iin]],FUN=cdf,x=x[wi],probability_distribution_attrname=probability_distribution_attrname,indices=NULL,return.as.spi=return.as.spi,...)
			attr(out[[iin]],"indices") <- wi
		}	
		
		outnn <- lapply(X=out[[1]],FUN=function(tt,x){x},x=x)
	
		
		for (iin in names(out)) {
	
			indicc <- (attr(out[[iin]],"indices"))
			outnn <- mapply(noo=outnn,oo=out[[iin]],indiccc=list(indicc),FUN=function(noo,oo,indiccc){
					
						wwi <- indiccc
						noo[wwi] <- oo
						return(noo)
					},USE.NAMES=TRUE,SIMPLIFY=FALSE)
			
		}
		out <- outnn
		
		if (length(out)==1) out <- out[[1]]
		
	} else if (is.na(para[1]))  {
		
		out <- array(as.numeric(NA),length(x))
		
	} else {
		
		distrib <- attr(para,probability_distribution_attrname) 	
		fundist <- get(paste0("cdf",distrib))
			
		out <- fundist(x=x,para=para,...)
		if (return.as.spi==TRUE) out <- qnorm(out)
			
	}
	
	
	
	
	return(out)
	
}

NULL
#'
#' @export
#' @rdname cdf
#' @aliases spi.cdf  
#' 
#' 
spi.cdf <- function(x,para,...) {
	
	out <- cdf(x=x,para=para,return.as.spi=TRUE,...)
	return(out)
}

NULL
#'
#' @export
#' @rdname cdf
#' @aliases cdf.spi 
#' 
#' 
cdf.spi <- function(x,para,...) {
	
	
	
	out <- cdf(x=x,para=para,return.as.spi=TRUE,...)
	return(out)
	
}

