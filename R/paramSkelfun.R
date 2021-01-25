##' Reorganize Parameters
##' @param npred number of predators
##' @param predpars vector of predation parameters
##' @param growpars vector of growth parameters
##' @export
##' @examples
##' paramSkelfun()
paramSkelfun <- function(npred,
                         predpars=c("c","d","g","m","n"),
                         growpars=c("r","max_size","D"))
{list(predparmat=matrix(nrow=npred,ncol=length(predpars),
                        dimnames=list(paste0("pred",seq(npred)),predpars)),
      npred=npred,
      growpars=setNames(numeric(length(growpars)),growpars))
}

