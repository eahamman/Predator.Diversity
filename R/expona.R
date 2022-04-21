#' Attack rate functions
#' @param s size vector of prey
#' @param c maximum rate
#' @param d size at maximum attack rate (scaling factor along size classes)
#' @param g scaler parameter (for power ricker)
#' @export
expona <- function(s,c,d,g) {c*exp(1-(s/d))}
