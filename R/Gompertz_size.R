##' Prey Growth Function
##' @param s_init initial size vector of prey
##' @param max_size maximum size of prey
##' @param r growth rate
##' @param time time steps
##' @export
##' @examples
##' Gompertz_size(max_size=100,s_init=1,r=1)
Gompertz_size <- function (max_size,s_init,r) {
  r*s_init*log(max_size/s_init)
} # Gompertz growth function (saturating)
## FIXME: not sensible when s_init==0???
