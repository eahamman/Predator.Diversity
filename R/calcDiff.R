##' Diffusion Rate Calculation
##' @param growpars parameters for growth function (r,D)
##' @param svec vector of size classes (upper bounds)
##' @param prey vector of prey densities
##' @param pred vector of predator densities
##' @export
##' @examples
##' calcDiff()

calcDiff <- function(growpars,svec,prey,pred) { # why are we calling predators?
  D <- with(as.list(growpars),D) # acquire D value (different from earlier simulations - now it will be scaled by the size of each bin)
  lngth <- length(prey)  # acquire length of prey vector(number of size classes)
  delta_s <- svec[1:length(svec)]-c(0,svec[1:(length(svec)-1)]) # find the length of each size class

  dens_s <- 1/delta_s # find the density of each size class (#/size of bin) ## units: number/mm
  D_dens <- D*dens_s # scale the diffusion constant by the size of the cells...
  D_dens[!is.finite(D_dens)] <- 0 # replace Inf (or NaN) with 0 (no diffusing out of cells that had nothing in them to start with)

  var1 <- prey   #dummy variable taking the value of prey
  var1[1] <- 0.5*var1[1]   #set the first element in the dummy vector to half of the value to keep 0 flux at the boundary
  var1[length(var1)] <- 0.5*var1[length(var1)]  #set the final element in the dummy vector to half of the value to keep 0 flux at the boundary

  diff_R2L <- (c(D_dens[-1],0)/2)*(c(prey[-1],0)) # classes gain via diffusion from right to left
  diff_L2R <- (c(0,D_dens[-lngth])/2)*(c(0,prey[-lngth])) # classes gain via diffusion from left to right

  diffusion <- diff_R2L+diff_L2R-D_dens*(var1)  #cells gain via diffusion above, and then lose the amount that diffused out

  return(diffusion)   #output diffusion
}
