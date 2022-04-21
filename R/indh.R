##' Independent handling time function
##' @param s size vector of prey
##' @param m handling time
##' @param n scaler parameter
##' @export

indh <- function(s,m,n) {rep(m,length.out=length(s))}