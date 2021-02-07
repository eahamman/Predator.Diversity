#' Attack rate functions
#' @param s size vector of prey
#' @param c maximum rate
#' @param d size at maximum attack rate (scaling factor along size classes)
#' @param g scaler parameter (for power ricker)
powRicker <- function(s,c,d,g) {c*(s/d*exp(1-(s/d)))^g}

#' @rdname powRicker
Ricker <- function(s,c,d,g) {c*(s/d*exp(1-(s/d)))}

#' @rdname powRicker
expona <- function(s,c,d,g) {c*exp(1-(s/d))}

#' @rdname powRicker
powRicker2 <- function(s,c,d,g) { c*((s/d)^g*exp(1-(s/d)))}

#' @rdname powRicker
hyperbol <- function(s,c,d,g) {c/((1+s)/d)}

##' @rdname powRicker
inda <- function (s,c,d,g) {rep(c,length.out=length(s))}

#' @examples
#' s <- seq(0,10)
#' c <- 0.7
#' g <- 2
#' d <- 6
#' powRicker(s,c,d,g)
