##' Alternative Formation of Power Ricker Attack Rate
##' @param s size vector of prey
##' @param c maximum attack rate
##' @param d size at maximum attack rate (scaling factor along size classes)
##' @param g scaler parameter
##' @export
##' @examples
##' powRicker2()
powRicker2 <- function(s,c,d,g) { c*((s/d)^g*exp(1-(s/d)))}
