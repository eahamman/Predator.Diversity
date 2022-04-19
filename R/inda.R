#' Independent attack rate functions
#' @param s size vector of prey
#' @param c maximum rate
#' @param d size at maximum attack rate (scaling factor along size classes)
#' @param g scaler parameter (for power ricker)
inda <- function (s,c,d,g) {rep(c,length.out=length(s))}
