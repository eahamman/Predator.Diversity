##' Functional Response Experiment Simulation
##' @param afun Size-dependent attack rate function
##' @param FRfun Hollings or Rogers
##' @param a maximum attack rate
##' @param a_sd random effect standard deviation
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
##' sim1 <- FR_Data_Sim("Ricker","Rogers",a=1,a_sd=0.1,
##'          h=1,d=1,reps=10,N0s=seq(10,40,by=10),Sizes=1:10,tval=10)
##' @importFrom stats rlnorm rbinom rnorm
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
  

  ## FIXME: these are all the same arg lists, could be significantly condensed???
  if(grepl("Hollings?",FRfun)) {
      p <- with(test.vals,
                switch(afun,
                       Ricker=hollings_risk(N0,c[block],h,tval,size,d,gamma,afun="Ricker"),
                       powRicker=hollings_risk(N0,c[block],h,tval,size,d,gamma,afun="powRicker"),
                       hollings_risk(N0,c[block],h,tval,size,d,gamma)))
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
  if(!deterministic){
      z <- rbinom(nrow(test.vals),prob=p,size=test.vals$N0)
  }else{z <- p*test.vals$N0}
  return(data.frame(test.vals,killed=z))
}


