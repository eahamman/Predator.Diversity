##' Gompertz Growth Function
##' @param s_init initial size vector of prey
##' @param max_size maximum size of prey
##' @param r growth rate
##' @param time number of timesteps
##' @export
##' @examples
##' Gompertz()
Gompertz <- function (max_size,s_init,r,time){max_size*exp(-s_init*exp(-r*time))} # Gompertz growth function (saturating)
