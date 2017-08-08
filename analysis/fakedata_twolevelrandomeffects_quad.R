FOR RANDOM SLOPE and INTERCEPT MODEL- matches twolevelrandomeffects.stan

rm(list=ls()) 
setwd("/users/kharouba/google drive/uOttawa/research/synchrony_fitness/")
library(rstan)
library(shinyStan)


Nint <- 25 # number of interactions (needed to generate parameters)

# True parameter values
mu_a <- 0 # mean fitness_z - for intercept (mean lm intercepts is 0.23)
mu_b1<- -0.044 # mean b1 slope from lm fits
mu_b2<- 0.0017 # mean b2 parameter from lm fits
sigma_y <- 0.98 # sd associated with response #

sigma_a<-0.39 #sd of intercepts from lm; 
a<-rnorm(Nint, mu_a, sigma_a) #generate intercepts for each species

sigma_b1<-0.13 #- sd of mean b1 slopes; 
sigma_b2<-0.011 # sd of mean b2 slopes
b1<-rnorm(Nint, mu_b1, sigma_b1); #generate slopes for each species
b2<-rnorm(Nint, mu_b2, sigma_b2); #generate slopes for each species


# Simulate/create the data
n_data_per_int <- round(runif(Nint, 10, 20)) # how many years per sp.?
intxn <- rep(1:Nint, n_data_per_int) #adds sppid-HK added
N <- length(intxn) #nrow of 'dataframe'
year <- rep(NA, N)
for (j in 1:Nint){
  year[intxn==j] <- rev(5 - 1:(n_data_per_int[j])) #assign 'new' year for each year/row for each species; from first year of study, number of years that differ from 1976, rev:like sort in descending order-HK added, series of years for each spp
}

year2<-year^2

ypred <- length(N) # Lizzie added
for (i in 1:N){ # actual STAN model
	s <- intxn[i] #sppid for each row
  
  CHECK WAY OF WRITING THIS
   ypred[i] <- a[intxn[s]] + b1[intxn[s]]*year[i]+b2[intxn[s]]*year2[i]; #mean? prediction is a function of vairance associated with species, fits slope with species random slope model, n loop, create data 
}
y <- rnorm(N, ypred, sigma_y);


fit_simple<-stan("analysis/stanmodels/twolevelrandomeffects_quad.stan", data=c("N","Nint","y","intxn","year", "year2"), iter=16000, chains=4)
print(fit_simple, pars=c("mu_a","mu_b1", "mu_b2","sigma_a","sigma_b1", "sigma_b2","sigma_y"))

fit_simple<-stan("analysis/stanmodels/twolevelrandomslopes_quad.stan", data=c("N","Nint","y","intxn","year", "year2"), iter=12000, chains=4)
print(fit_simple, pars=c("mu_b1", "mu_b2","sigma_b1", "sigma_b2","sigma_y"))

