##' Predation Rate Calculation
##' @param parmat matrix of parameters that define predator attack rates and handling times
##' @param svec vector of size classes (upper bounds)
##' @param preyvec vector of prey densities
##' @param predvec vector of predator densities
##' @param afun attack-rate function
##' @param hfun handling time function
##' @export
##' @examples
##' svec <- 1:10
##' preyvec <- rep(1,10)
##' predvec <- 1
##' parmat <- matrix(c(1,1,1,   ## c,d,g pred 1
##'                    1,1,1),  ## c,d,g pred2
##'        byrow=TRUE,
##'        nrow=2,dimnames=list(c("pred1","pred2"),c("c","d","g")))
##' ## calcPred()
calcPred <- function(parmat,svec,preyvec,predvec,afun=inda, hfun=indh) {
  amat <- with(as.data.frame(parmat),mapply(afun,c,d,g,MoreArgs=list(s=svec))) # Calculate matrix of attack rates (each predator, each prey size)
  hmat <- with(as.data.frame(parmat),mapply(hfun,m,n,MoreArgs=list(s=svec))) # Calculate matrix of handling times (each predator, each prey size)
  anmat <- sweep(amat,1,preyvec,"*") ## total attack rate (a_i(s)n(s))
  A <- colSums(anmat,na.rm=TRUE) ## grand total attack o ; ignore nonsensical values

  if (all(A==0)) {
    fmat <- matrix(0,nrow=nrow(anmat),ncol=ncol(anmat)) # if all attack rates are 0, empty matrix
  } else {
    fmat <- sweep(anmat,2,A,"/") ## proportional attack # otherwise, proportional attack rate on each size class (per predator)
  }
  H <- colSums(fmat*hmat)  ## weighted average handling times
  fr <- sweep(anmat,2,1+A*H,"/")  ## functional responses
  pred <- rowSums(sweep(fr,2,predvec,"*")) ## absolute predation rates

  return(pred) # return predation rate
}
