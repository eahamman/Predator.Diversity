##' Hyperbolic Attack Rate Function
##' @param s size vector of prey
##' @param c maximum attack rate
##' @param d size at maximum attack rate (scaling factor along size classes)
##' @param g scaler parameter
##' @export
##' @examples
##' ## hyperbol()
hyperbol <- function(s,c,d,g) {c/((1+s)/d)}
