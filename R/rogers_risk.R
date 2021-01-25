##' Hollings per-capital mortality risk
##' @param N0 initial prey density
##' @param s size of prey
##' @param d size at max attack rate
##' @param gamma shape parameter for power ricker attack rate
##' @param a attack rate
##' @param h handling time
##' @param tval length of predation experiment
##' @param afun form of attack rate size-dependence:  options, Ricker, powRicker (will expand later)
##' @export
##' @examples
##' rogers_risk()

rogers_risk <- function(N0,c,h,tval,s,d,gamma,afun){

  if(afun=="Ricker"){
    ar <- Ricker(s,c,d,gamma)
    p <-  (N0 - lambertW(ar*h*N0*exp(-ar*(tval-h*N0)))/(ar*h))/N0
  }

  if(afun=="powRicker"){
    ar <- powRicker(s,c,d,gamma)
    p <-  (N0 - lambertW(ar*h*N0*exp(-ar*(tval-h*N0)))/(ar*h))/N0
  }

  if(afun=="inda"){
    ar <- rep(c,length(s))#inda(s,c,d,gamma)
    p <- (N0 - lambertW(ar*h*N0*exp(-ar*(tval-h*N0)))/(ar*h))/N0
  }
  return(pmin(p,1))
}
