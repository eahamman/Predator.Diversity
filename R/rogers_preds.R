##' Rogers per-capital mortality risk with multiple of the same predator
##' @param N0 initial prey density
##' @param s size of prey
##' @param d size at max attack rate
##' @param gamma shape parameter for power ricker attack rate
##' @param a attack rate
##' @param h handling time
##' @param P number of predators
##' @param tval length of predation experiment
##' @param afun form of attack rate size-dependence:  options, Ricker, powRicker (will expand later)
##' @export
##' @examples
##' rogers_risk()
rogers_preds = function(N0,a,h,P,tval) {
  (N0 - lambertW(a*h*N0*exp(-a*(P*tval-h*N0)))/(a*h))
}
