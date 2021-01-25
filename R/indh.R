##' Size-independent handling time
##' @param s size vector of prey
##' @param m handling time
##' @param n scaler parameter
##' @export
##' @examples
##' indh()
indh<- function(s,m,n) {rep(m,length.out=length(s))}
