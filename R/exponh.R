##' Exponential handling time
##' @param s size vector of prey
##' @param m handling time
##' @param n scaler parameter
##' @export
##' @examples
##' exponh(1,1,1)
exponh <- function(s,m,n) {m*exp(n*s)}
