##' Exponential Attack Rate Function
##' @param s size vector of prey
##' @param c maximum attack rate
##' @param d size at maximum attack rate (scaling factor along size classes)
##' @param g scaler parameter
##' @export
##' @examples
##' ## expona()
expona <- function(s,c,d,g) {c*exp(1-(s/d))}
