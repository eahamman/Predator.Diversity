##' Gompertz Growth Function
##' @param s_init initial size vector of prey
##' @param max_size maximum size of prey
##' @param r growth rate
##' @param time time steps
##' @export
##' @examples
##' Gompertz(max_size=100,s_init=1,r=1,time=0:10)
Gompertz <- function (max_size,s_init,r,time) {
    max_size*exp(-s_init*exp(-r*time))
} # Gompertz growth function (saturating)
## FIXME: not sensible when s_init==0???
