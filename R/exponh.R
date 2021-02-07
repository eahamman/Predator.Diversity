##' Handling time functions
##' @param s size vector of prey
##' @param m handling time
##' @param n scaler parameter
##' @export
##' @examples
##' exponh(1,1,1)
exponh <- function(s,m,n) {m*exp(n*s)}

##' @rdname  exponh
indh <- function(s,m,n) {rep(m,length.out=length(s))}

##' @rdname  exponh
powFun1 <- function(s,m,n) {m*s^n}


#' @examples
#' s <- seq(0,10)
#' m <- 0.7
#' n <- 2
#' powFun1(s,m,n)
