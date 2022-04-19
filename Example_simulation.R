

library(devtools)
install_github("eahamman/Predator.Diversity")
library(roxygen2)

library(Predator.Diversity)



#FR -Parameter Specification
max_a <- 0.5 # max attack rate
max_a_var <- 0 # variance on max attack rate
c1 <- max_a
c2 <- max_a
#Handling time
base_h <- 1 # handling time
h1 <- base_h
h2 <- base_h

#Size dependence parameters
d_1 <- 3.5# seq(3.5,24,length=20) #Size dependence scalar
d_2 <- 3.5 # seq(3.5,24,length=20) #Size dependence scalar
gamma_1 <- seq(0.001,3.5,length=20) #Size dependence exponent
gamma_2 <- seq(0.001,3.5,length=20) #Size dependence exponent

#Size and growth parameters
maxsize <- 45 # maximum prey size (all bins will approach this in cohort version)
dd1 <- 0.00 # diffusion rate

prey.growth.rate <- c(0.1) # prey growth rate,0.05
prey.sizes <- seq(1,maxsize,length=10) # prey size vector

#prey pop dynamics parameters
prey_density <-c(1) # prey density treatments THIS SHOULD BE AN ODD Seq FOR NOW!!
new_prey <- 0 # prey per ca-pita reproduction rate
timeint=seq(0,1,.1) #timesteps between reproduction events
predator_density <- 1 # density of predators 


#parameters for the size dependence function
d1 <- d_1
d2 <- d_2
gamma1 <-1# gamma_1[k]
gamma2 <- 1#gamma_2[kk]
c1 <- c1
c2 <- c2
h1 <- h1
h2 <- h2
time.steps=10
#create parameter matrices for each predator seperately and then in combination to input into simulation
parmat <- matrix(c(c1,d1,gamma1,h1,h1, c2,d2,gamma2,h2,h2),  ## c,d,g pred 1 and 2
                 byrow=TRUE,nrow=2,dimnames=list(c("pred1","pred2"),c("c","d","g","m","n")))
#select the functional form of the size dendence

P1 <- Pred_Sims(maxsize=max(prey.sizes), dd1=dd1, r1=prey.growth.rate, prey_dens=prey_density, new_prey=0, pred_dens=c(predator_density,1), #density of predators (vector)
                predpar.mat=parmat,timevec=timeint,num_steps=time.steps,afun=inda,hfun=indh,svec0=prey.sizes)
P1

?powRicker
