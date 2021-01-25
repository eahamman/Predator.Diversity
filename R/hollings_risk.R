##' Hollings per-capital mortality risk
##' @param N0 initial prey density
##' @param s size of prey
##' @param d size at max attack rate
##' @param gamma shape parameter for power ricker attack rate
##' @param a attack rate
##' @param h handling time
##' @param tval length of predation experiment
##' @param afun form of attack rate size-dependence:  options, inda Ricker, powRicker
##' @export
##' @examples
##' hollings_risk()

hollings_risk <- function(N0,c,h,tval,s,d,gamma,afun){

  if(afun=="Ricker"){
    ar <- Ricker(s,c,d,gamma)
    p <- ar/(1+ar*N0*h*tval)
  }

  if(afun=="powRicker"){
    ar <- powRicker(s,c,d,gamma)
    p <- ar/(1+ar*N0*h*tval)
  }

  if(afun=="inda"){
    ar <- rep(c,length(s))#inda(s,c,d,gamma)
    p <- ar/(1+ar*N0*h*tval)
  }

  return(pmin(p,1))
}
