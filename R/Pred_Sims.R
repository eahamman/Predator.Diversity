##' Simulate predation by multiple predators with size-dependent functional responses
##' @param maxsize maximum size (all bins will approach this)
##' @param dd1 diffusion rate
##' @param r1 growth rate
##' @param prey_dens density of prey (one number)
##' @param new_prey number of new juveniles
##' @param svec0 initial size vector
##' @param pred_dens vector of predator density
##' @param predpar.mat matrix of predator functional response parameters (each line is a predator, each column is a parameter)
##' @param timevec vector of timesteps for ode (time between reproduction events)
##' @param num_steps number of overall timesteps (or reproduction events)
##' @param afun=inda attack rate functional form (default is size-independent attack rate)
##' @param hfun=indh handling time functional form (default is size-independent handling time)
##' @export
##' @examples
##' Pred_Sims()

Pred_Sims <- function(
  maxsize, # maximum size (all bins will approach this)
  dd1, # diffusion rate
  r1, # growth rate
  prey_dens, #density of prey (one number) or vector length of svec0
  new_prey, # number of new juveniles
  #smin, # smallest possible prey size
  #smax, # old max prey size (still called - need to delete this)
  #ds, # old prey step for svec0, (still called - need to delete this)
  svec0, # initial size vector (bin labels) for prey
  #p0, # vector of the number of prey in each size bin
  pred_dens, #density of predators (vector)
  predpar.mat,
  
  timevec,
  num_steps,
  afun=inda,
  hfun=indh,
  ...
){
  npred <- nrow(predpar.mat)#length(pred_dens)
  
  if(length(pred_dens)==1){
    pred_dens = rep(pred_dens,npred) #starting predator densities if not given as a vector
  }else{pred_dens = pred_dens}
  
  if(length(prey_dens)==1){
    p0 = rep(prey_dens,length(svec0)) #starting prey densities (uniform across starting size classes)
  }else{p0 = prey_dens}
  
  x0 <- c(pred_dens,p0,rep(0,length(svec0)-length(p0))) ## 2 pred, initial prey densities
  svec <- svec0 # initial size vector
  
  Summary=mat.or.vec(1,4) # Initialize summary matrix
  colnames(Summary)=c("time","size","abundance","density") # Column names for summary matrix
  
  for(step in 1:num_steps){
    
    pList1 <- list(#sizepars=c(smin=smin,smax=smax,ds=ds),  # make list of size paramaters, predpar matrix, npred and growth parameters
      predparmat=predpar.mat,                 
      npred=npred,
      growpars=c(r=r1,max_size=maxsize,D=dd1),
      svec0=svec) 
    
    ode1 <- ode(y=x0,times=timevec,func=gradientfun,parms=pList1,
                afun=afun, hfun=hfun
                ##  hfun and afun should get passed through
                ##  to gfun2, and in turn to calcPred
    )    # calculates population of prey over time
    
    num_class <- (ncol(ode1)-(npred+1))/2 # determine number of size classes
    
    end_prey <- ode1[length(timevec),(npred+2):(npred+1+num_class)] # prey abundance at the end of the previous reproductive period
    svec <- ode1[length(timevec),(npred+1+num_class+1):ncol(ode1)] # prey size distribution at the end of the previous reproductive period
    delta_s <- svec[1:length(svec)]-c(0,svec[1:(length(svec)-1)]) # find the length of each size class
    
    
    # Summarize prey distribution for this reproductive cycle
    out=mat.or.vec(length(svec),4) #summary for this reproductive cycle
    out[,1] <- t(rep(step,length(svec))) # timesteps
    out[,2] <- t(svec) # size distribution
    out[,3] <- t(end_prey) # prey abundance in each size class
    out[,4] <- t(end_prey/delta_s) # prey density (per mm) per size class
    
    # Combine with overall simulation summary
    Summary <- rbind(Summary,out)
    
    # New Size vector and corresponding new initial conditions for ode
    ## FIXME: hard-coded for now, should pass gfun through ode()
    
    # ** Reproduction happens here **
    Num_juv <- sum(end_prey)*new_prey
    
    if(r1>0){
      svec <- c(svec0[1],svec) # new size distribution
      p0 <- c(Num_juv,end_prey)   # new prey size distribution
    }else(p0 <- c(end_prey[1]+Num_juv,end_prey[2:length(end_prey)])) #growth (new size distribution), but no prey reproduction
    
    x0 <- c(pred_dens,p0) # initial condition vector
  }
  return(Summary)
}

