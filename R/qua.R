# TODO: Add comment
# 
# Author: cordaem
###############################################################################

NULL
#' Generic function for \code{qua...}: probabilily distribution fitting with L-Moments
#' 
#' 
# 
#' @param f,para,... L-moments and further parameters for \code{qua...}
#' @param probability_distribution_attrname attribute name for probability distribution
#' 
#' @export
#' 
#' @return A vector of quantiles. It is a list of vectors of quantiles in case of several probability parametric distribution functions (i.e. \code{para} is a list and \code{length(para)>1}). 
#' 
#' @seealso \code{\link{pel}},\code{\link{quaexp}},\code{\link{quagam}},\code{\link{quagev}},\code{\link{quaglo}},\code{\link{quagpa}},\code{\link{quagno}},\code{\link{quagum}},
#' 
#' \code{\link{quakap}},\code{\link{qualn3}},\code{\link{quanor}},\code{\link{quape3}},
#' \code{\link{quawak}},\code{\link{quawei}} 	
#' 
#' @importFrom lmom  quagam quagev quaglo quagpa quagno quagum quakap qualn3 quanor quape3 quawak quawei 
# @importFrom stringr str_sub str_sub<-
#' @examples
#' 
#' # Sample L-moments of Ozone from the airquality data
#' data(airquality)
#' lmom <- samlmu(airquality$Ozone,nmom=6)
#' 
#' distrib <- c("exp","gam","gev","glo","gpa","gno","gum","kap","ln3","nor",
#' 	"pe3","wak","wei")
#' 
#' para_list <- pel(distrib=distrib,lmom=lmom)
#' f <- (1:10)/10
#' qua_list <- qua(para=para_list,f=f)
#' 
#' 


qua <- function(para,f,probability_distribution_attrname="probability_distrib",...) {
	
	out <- try(stop("Generic Error!!"),silent=TRUE)
		
	if (is.null(para)) para <- NA
	if (is.list(para)) {
			
		####names(distrib) <- distrib
		out <- lapply(X=para,FUN=qua,f=f,probability_distribution_attrname=probability_distribution_attrname,...)
			
		
	} else if (is.na(para[1]))  {
		
		out <- array(as.numeric(NA),length(f))
		
	} else {
		
		distrib <- attr(para,probability_distribution_attrname) 	
		fundist <- get(paste0("qua",distrib))		
		out <- fundist(f=f,para=para,...)
		
			
	}
	
	
	
	
	return(out)
	
}




