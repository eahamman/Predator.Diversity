##' Size-independent attack rate (constant)
##' @param s size vector of prey
##' @param c maximum attack rate
##' @param d size at maximum attack rate (scaling factor along size classes)
##' @param g scaler parameter
##' @export
##' @examples
##' inda()
inda <- function (s,c,d,g) {rep(c,length.out=length(s))}
