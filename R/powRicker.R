#' Power Ricker Attack rate functions
#' @param s size vector of prey
#' @param c maximum rate
#' @param d size at maximum attack rate (scaling factor along size classes)
#' @param g scaler parameter (for power ricker)
#' @export
powRicker <- function(s,c,d,g) {c*(s/d*exp(1-(s/d)))^g}
