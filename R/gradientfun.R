##' Reorganize Parameters
##' @param time vector timesteps for integration (between each reproduction event)
##' @param x initial condition vector (predator densities, prey densities)
##' @param afun attack rate function - default is size-independent
##' @param hfun handling time function - default is size-independent
##' @export
##' @examples
##' gradientfun()

gradientfun <- function(time,x,params, afun=inda, hfun=indh) {
  npred <- params[["npred"]]   ## need to extract number of predators as meta-parameter ...

  ## restore structure of parameters from parameter vector
  #Params <- params#relist(unlist(params),paramSkelfun(npred=npred))
  growpars <- params[["growpars"]]
  svec0<-params[["svec0"]]
  max_size <- growpars["max_size"]
  r <- growpars["r"]

  # Prey growth: labels on prey bins (svec) grow according to Gompertz growth
  svec <- Gompertz(max_size,-log((svec0)/max_size),r,time)
  
  X <- relist(x,list(pred=numeric(npred),prey=numeric(length(svec))))
  index_0 <- match(0,svec)  ## which element of svec corresponds to size == 0?
  index_pos1 <- index_0+1   ## first positive-size bin
  if(is.na(index_0)==FALSE){# If there are any negative size classes
    if (X$prey[index_0]>0) {  ## positive prey density in the zero size class
      ## move all non-positive-size populations to the smallest positive size class
      X$prey[index_pos1] <- X$prey[index_pos1]+sum(X$prey[1:index_0])
      X$prey[(1:index_0)] <- 0
    }
  }
  
  predrate <- calcPred(params$predparmat,svec,X$prey,X$pred,
                       afun=afun, hfun=hfun)  ## calculate size-spec predation 

  diffrate <- calcDiff(params$growpars,svec,X$prey,X$pred) # calculate variation in growth (diffusion)

  grad <- c(rep(0,npred),   ## predator changes (constant model!)
            diffrate-predrate)   ## prey gradient and diffusion

  return(list(grad,svec)) # return gradient and size vector
}
