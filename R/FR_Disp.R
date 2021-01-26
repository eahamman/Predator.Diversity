##' Functional Dispersion - Functional Response Parameters
##' @param parms matrix of parameters (currently 4 parameters, will recode)
##' @export
##' @examples
##' FR_Disp(rbind(1:4))

FR_Disp <- function(parms){
    ## FIXME: do something with apply?
    dispersion <- sqrt(sum((1-parms[,1]/mean(parms[,1]))^2+
                           (1-parms[,2]/mean(parms[,2]))^2+
                           (1-parms[,3]/mean(parms[,3]))^2+
                           (1-parms[,4]/mean(parms[,4]))^2))
  return(dispersion)
}
