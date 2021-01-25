##' Functional Response Experiment Simulation
##' @param afun Size-dependent attack rate function
##' @param FRfun Hollings or Rogers
##' @param a maximum attack rate
##' @param a_var random effect variance
##' @param h handling time
##' @param d size at max attack rate
##' @param gamma shape parameter for size dependence
##' @param reps number of replicates (or blocks) in the experiment
##' @param Sizes vector of size classes
##' @param N0s vector of initial densities
##' @param tval number of timesteps in experiment
##' @param deterministic return probability instead of binomal RV
##' @param logRE implement random effects on log scale
##' @export
##' @examples
##' FR_Data_Sim()

FR_Data_Sim <- function(afun,FRfun,a,a_sd,h,d,gamma,reps,N0s,Sizes,tval,deterministic=FALSE,logRE=TRUE){
  
  test.vals <- expand.grid(N0=N0s,size=Sizes,block=1:reps)
  if(logRE==TRUE){
    RE <- rlnorm(reps,meanlog=0,sdlog=a_sd)
    c <- a*RE
  }
  if(logRE==FALSE){
    RE <- rnorm(reps,mean=0,sd=a_sd)
    c <- a+RE
  }
  
  
  if(FRfun=="Holling"|FRfun=="Hollings"){
    if(afun=="Ricker"){
      p <- with(test.vals,hollings_risk(N0,c[block],h,tval,size,d,gamma,afun="Ricker"))
    }
    if(afun=="powRicker"){
      p <- with(test.vals,hollings_risk(N0,c[block],h,tval,size,d,gamma,afun="powRicker"))
    }
    if(afun=="inda"){
      p <- with(test.vals,hollings_risk(N0,c[block],h,tval,size,d,gamma,afun="inda"))
    }
  }
  if(FRfun=="Rogers"){
    if(afun=="Ricker"){
      p <- with(test.vals,rogers_risk(N0,c[block],h,tval,size,d,gamma,afun="Ricker"))
    }
    if(afun=="powRicker"){
      p <- with(test.vals,rogers_risk(N0,c[block],h,tval,size,d,gamma,afun="powRicker"))
    }
    if(afun=="inda"){
      p <- with(test.vals,rogers_risk(N0,c[block],h,tval,size,d,gamma,afun="inda"))
    }
  }
  if(deterministic==FALSE){
    z <- rbinom(nrow(test.vals),prob=p,size=test.vals$N0)
  }else{z <- p*test.vals$N0}
  return(data.frame(test.vals,killed=z))
}


