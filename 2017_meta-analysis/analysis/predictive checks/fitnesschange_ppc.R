# Posterior Predictive checks for fitness CHANGE model (e.g. fitnes~year); 
#  varying and pooled intercepts, only single slope estimated


#complete pooling
comppool<-lm(fitness_z~newyear, data=go)
#with(tdata2, plot(pheno.change~temp.change)); with(tdata2, abline(lm(pheno.change~temp.change))

# No pooling:
uniquespp<-unique(go$intid)
slopefits<-rep(1< length(uniquespp))
varfits<-rep(1< length(uniquespp))
intfits<-rep(1< length(uniquespp))
for(eachsp in 1:length(uniquespp)){
	lmhere<-lm(fitness_z~newyear, data=subset(go, intid==uniquespp[eachsp]))
	slopefits[eachsp]<-coef(lmhere)[2]
	varfits[eachsp]<-(summary(lmhere)$sigma)**2
	intfits[eachsp]<-coef(lmhere)[1]
}
dr<-data.frame(cbind(uniquespp, slopefits, varfits, intfits))
dr$slopefits<-as.numeric(levels(dr$slopefits))[dr$slopefits]
dr$intfits<-as.numeric(levels(dr$intfits))[dr$intfits]
dr$varfits<-as.numeric(levels(dr$varfits))[dr$varfits] #factor to numeric

par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), ylim=range(y), type="n", xlab="Phenodiff", ylab="Fitness_z", bty="l", main="No pooling- linear models")
for (j in 1:Nint){
  abline(lm((y[intxn==j])~year[intxn==j]))
}

# Now grab the stan output 
goo <- extract(fit.model) 
goo <- extract(fit.model.int) 

#extract the means for now:
mu_b <- mean(goo$mu_b) # from stan output (what if mu_b<- -1.47)
sigma_y <- mean(goo$sigma_y) # from stan output (which I named 'goo' for now)
sigma_b <- mean(goo$sigma_b) # from stan output 

mu_a <- mean(goo$mu_a) # from stan output (what if mu_b<- -1.47)
sigma_a <- mean(goo$sigma_a) # from stan output 

a <- colMeans(goo$a) # this may not be ideal but deals with the non-pooling
b <- rnorm(Nint, mean=mu_b, sd=sigma_b) # this is one way to create fake data from the Stan output to use in the PP check

a <- rnorm(Nint, mean=mu_a, sd=sigma_a) # thi
b <- colMeans(goo$b)

#This is just creating basically one new set of data, or one predictive check
ypred <- length(N) # Lizzie added
for (n in 1:N){
     s <- intxn[n]
    ypred[n] <- a[s] + b[s]*year[n] # one way to handle the non-pooled intercepts, there may be other ways
}
y <- rnorm(N, ypred, sigma_y)

#Plot the data and see what the raw data predicted from the model looks like
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), ylim=range(y), type="n", xlab="year", ylab="Fitness_z",   bty="l", main="Data from posterior means")
for (j in 1:Nint){
   abline(lm((y[intxn==j])~year[intxn==j]))
}

