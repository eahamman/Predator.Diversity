##' Rogers per-capital mortality risk with multiple of the same predator
##' @param N0 initial prey density
##' @param a attack rate
##' @param h handling time
##' @param P number of predators
##' @param tval length of predation experiment
##' @export
##' @examples
##' ## rogers_preds()
##' @importFrom emdbook lambertW
rogers_preds = function(N0,a,h,P,tval) {
    (N0 - lambertW(a*h*N0*exp(-a*(P*tval-h*N0)))/(a*h))
}
