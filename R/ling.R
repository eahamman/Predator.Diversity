##' Linear somatic growth (constant growth rate among sizes)
##' @param max_size maximum size of prey
##' @param s_init initial size of prey
##' @param r prey growth rate
##' @param time timestep for growth
##' @export
##' @examples
##' ling()
ling <- function(max_size,s_init,r,time){s_init+r*time}
