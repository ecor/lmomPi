NULL
#' Validate L-moment Constraints
#'
#' This function checks whether a given set of L-moments satisfies theoretical and practical constraints
#' commonly used in statistical modeling. These constraints ensure the validity of L-moment ratios and
#' help detect potential issues in the data or its transformation (e.g., log-transformed data with non-positive values).
#'

#' @param lmom vector of L-moments 
#' @param clean logical if it is \code{TRUE} (default) clean \code{lmom}'s names 
#' @param return_numeric logical if it is \code{TRUE} L-moments corrected are returned instead of a boolean value.
#' @param condt3t4 logical if \code{TRUE} condition on \code{t_3} and \code{t_4} is applied.
#' @param condtt3 logical if \code{TRUE} condition on \code{t} and \code{t_3} is applied (only for a distribution that takes positive values, p.24 ref).
#' 
#' 
#' 
#' @export
#' 
#' @return logical value indicating if all of these conditions are verified:
#' \itemize{
#'   \item \eqn{\lambda_2 > 0} (L-scale must be positive)
#'   \item \eqn{|\tau_3| < 1} (L-skewness must lie within \eqn{(-1, 1)})
#'   \item \eqn{|\tau_i| < 1} (i-th L-moment ratio must lie within \eqn{(-1, 1)} for \code{i>3})
#'   \item \eqn{\lambda_1} and \eqn{\lambda_2} must be finite
#'   \item \eqn{\frac{5\tau_3^2 - 1 }{4} \leq \tau_4 < 1} is verified for theoretical consistency
#'   \item \eqn{2\tau-1 \leq \tau_3 < 1} is verified for theoretical consistency (only for a distribution that takes positive values, p.24 ref)
#' }
#'
#'
#' @importFrom stringr str_replace 
#' 
#' 
#' @references
#' Hosking, J.R.M. and Wallis, J.R. (1997). \emph{Regional Frequency Analysis: An Approach Based on L-Moments}. Cambridge University Press. \doi{10.1017/CBO9780511529443}
#'
#' 
#' 
#' @examples 
#'
#' library(lmomPi)
#' 
#' data(airquality)
#' lmom <- samlmu(airquality$Ozone, 6)
#' are.lmoms.valid(lmom = lmom)
#' 
#' are.lmoms.valid(lmom = lmom,return_numeric=TRUE)
#' 
#' lmoml <- samlmu(airquality$Ozone, 6,ratio=FALSE)
#' 
#' are.lmoms.valid(lmom = lmoml)
#' are.lmoms.valid(lmom = lmoml,condtt3=TRUE)
#' are.lmoms.valid(lmom = lmoml,return_numeric=TRUE)
#' are.lmoms.valid(lmom = lmoml,return_numeric=TRUE,condtt3=TRUE)
#' 







are.lmoms.valid <- function (lmom,clean=TRUE,return_numeric=FALSE,condt3t4=TRUE,condtt3=FALSE) {
  #####names_lmom=c("l_1","l_2","t_3","t_4")
  
  out <- FALSE
  if (return_numeric) out <- NULL
  
  if (is.null(lmom)) {
    return(out)
  } else if (length(lmom) <3) {
    return(out)
  } else if (any(is.na(lmom))) {
    return(out)
  }
  
  
  
  lmom <- unlist(lmom)
 
  if (clean) {
    
    names(lmom) <- gsub("\\([^)]*\\)", "", names(lmom))
    
  }
  
  if (!("l_1" %in% names(lmom)))  return(out)
    
 
  if (!("l_2" %in% names(lmom))) {
    i <- which(names(lmom) %in% c("t_2","l_cv")) 
    if (length(i)>0) {
      names(lmom)[i[1]] <- "t"
      
    }
    lmom[["l_2"]] <- lmom[["l_1"]]*lmom[["t"]]
    
  }  
  iii <- (1:length(lmom))[-c(1,2)]
  lnn <- sprintf("l_%d",iii)
  ##lnn <- lnn[lnn]
  iic <- which(names(lmom) %in% lnn)
  if (length(iic)>0) {
    lmom[iic] <- lmom[iic]/lmom[["l_2"]]
    names(lmom)[iic] <- names(lmom)[iic] |> str_replace("l_","t_")
  } 
  ###
  nnn <- c(sprintf("l_%d",1:2),sprintf("t_%d",3:length(lmom)))
  ###
  
  
  
  lmom <- lmom[which(names(lmom) %in% nnn)] 
  
  lmom <- lmom[sort(names(lmom))]
  if (!is.finite(lmom[["l_1"]])) return(out)

  
  if (!is.finite(lmom[["l_2"]])) return(out)
  if (!(lmom[["l_2"]]>=0)) return(out)
  
  for (it in sprintf("t_%d",3:length(lmom))) {
      if (!(abs(lmom[[it]])<1)) return(out)  
  }  
  if (all(c("t_4","t_3") %in% names(lmom)) & condt3t4)  {
    condt3t4 <- ((5*lmom[["t_3"]]^2-1)/4)<=lmom[["t_4"]]  | !condt3t4
    if (!condt3t4) return(out)
    
  } 
  if (all(c("l_1","l_2","t_3") %in% names(lmom)) & condtt3){
    condtt3 <- 2*lmom[["l_2"]]/lmom[["l_1"]]-1<=lmom[["t_3"]]
    if (condtt3) return(out)
    
  }
  out <- TRUE
  if (return_numeric) out <- lmom
  
  
  return(out)
}