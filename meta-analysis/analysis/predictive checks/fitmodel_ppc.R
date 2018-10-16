# Posterior Predictive checks for fitness model (e.g. fitnes~mismatch); 
#  varying and pooled intercepts, only single slope estimated


#complete pooling
comppool<-lm(fitness_z~phenodiff, data=yano2)
#with(tdata2, plot(pheno.change~temp.change)); with(tdata2, abline(lm(pheno.change~temp.change))

# No pooling:
uniquespp<-unique(yano2$intid)
slopefits<-rep(1< length(uniquespp))
varfits<-rep(1< length(uniquespp))
intfits<-rep(1< length(uniquespp))
for(eachsp in 1:length(uniquespp)){
	lmhere<-lm(fitness_z~phenodiff, data=subset(yano2, intid==uniquespp[eachsp]))
	slopefits[eachsp]<-coef(lmhere)[2]
	varfits[eachsp]<-(summary(lmhere)$sigma)**2
	intfits[eachsp]<-coef(lmhere)[1]
}
dr<-data.frame(cbind(uniquespp, slopefits, varfits, intfits))
dr$slopefits<-as.numeric(levels(dr$slopefits))[dr$slopefits]
dr$intfits<-as.numeric(levels(dr$intfits))[dr$intfits]
dr$varfits<-as.numeric(levels(dr$varfits))[dr$varfits] #factor to numeric


# No pooling: QUAD vs linear
uniquespp<-unique(super$intid)
slopefits<-rep(1< length(uniquespp))
slopefits_quad<-rep(1< length(uniquespp))
quadfits<-rep(1< length(uniquespp))
varfits<-rep(1< length(uniquespp))
intfits<-rep(1< length(uniquespp))
lmfit<-rep(1< length(uniquespp))
quadfit<-rep(1< length(uniquespp))
intfits_quad<-rep(1< length(uniquespp))
pfit<-rep(1< length(uniquespp))
for(eachsp in 1:length(uniquespp)){
	linear<-gls(fitness_z~phenodiff, data=subset(super, intid==uniquespp[eachsp]))
	slopefits[eachsp]<-coef(linear)[2]
	intfits[eachsp]<-coef(linear)[1]
	lmfit[eachsp]<-AIC(linear)	
	pfit[eachsp]<-coef(summary(linear))[,4][[2]]
	quad<-gls(fitness_z~phenodiff_center+I(phenodiff_center^2), data=subset(super, intid==uniquespp[eachsp]))
	slopefits_quad[eachsp]<-coef(quad)[2]
	quadfits[eachsp]<-coef(quad)[3]
	quadfit[eachsp]<-AIC(quad)
	intfits_quad[eachsp]<-coef(quad)[1]
}
dr<-data.frame(cbind(uniquespp, lmfit, quadfit, pfit, slopefits_quad, quadfits))
dr$best<-lmfit-quadfit; dr

#for fitness~mismatch- quadratic:
ggplot(super, aes(x=year, y=fitness_z, colour=factor(intid))) + geom_smooth(method="lm", formula=y~x+I(x^2), se=FALSE)+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("fitness_z")+xlab("mismatch")

Otherwise:
option 1:
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), ylim=range(y), type="n", xlab="Phenodiff", ylab="Fitness_z", bty="l", main="No pooling- linear models")
for (j in 1:Nint){
  abline(lm((y[intxn==j])~year[intxn==j]))
}

option 2:
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), ylim=range(y), type="n", xlab="Temp change", ylab="Pheno change", bty="l", main="No pooling intercept, complete pooling slope")
for (j in 1:Nspp){
	m1<-lm((y[species==j])~year[species==j])
  abline(a=coef(m1)[1], b=coef(comppool)[2])
}


# Now grab the stan output 
goo <- extract(fit.model) 

## LINEAR MODEL FIRST ##
!! 3 model options
# Random effects model:
#extract the means for now:
mu_b <- mean(goo$mu_b) # from stan output (what if mu_b<- -1.47)
sigma_y <- mean(goo$sigma_y) # from stan output (which I named 'goo' for now)
mu_a <- mean(goo$mu_a) # from stan output 
sigma_a <- mean(goo$sigma_a) # from stan output 
sigma_b <- mean(goo$sigma_b) # from stan output 


a <- rnorm(Nint, mean=mu_a, sd=sigma_a) # this is one way to create fake data from the Stan output to use in the PP check
b <- rnorm(Nint, mean=mu_b, sd=sigma_b) # this is one way to create fake data from the Stan output to use in the PP check

# Random intercept model:
mu_a <- mean(goo$mu_a) # from stan output 
sigma_y <- mean(goo$sigma_y) # from stan output (which I named 'goo' for now)
sigma_a <- mean(goo$sigma_a) # from stan output

b <- colMeans(goo$b) # this may not be ideal but deals with the non-pooling
a <- rnorm(Nint, mean=mu_a, sd=sigma_a) # this is one way to create fake data from the Stan output to use in the PP check

# Random slope model:
mu_b <- mean(goo$mu_b) # from stan output 
sigma_y <- mean(goo$sigma_y) # from stan output (which I named 'goo' for now)
sigma_b <- mean(goo$sigma_b) # from stan output

a <- colMeans(goo$a) # this may not be ideal but deals with the non-pooling
b <- rnorm(Nint, mean=mu_b, sd=sigma_b) # this is one way to create fake data from the Stan output to use in the PP check

## Create the PP data using new a nd b for each of 71 species
#This is just creating basically one new set of data, or one predictive check
ypred <- length(N) # Lizzie added
for (n in 1:N){
     s <- intxn[n]
    ypred[n] <- a[s] + b[s]*year[n] # one way to handle the non-pooled intercepts, there may be other ways
}
y <- rnorm(N, ypred, sigma_y)

#Plot the data and see what the raw data predicted from the model looks like
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), ylim=range(y), type="n", xlab="phenodiff", ylab="Fitness_z",   bty="l", main="Data from posterior means")
for (j in 1:Nint){
   abline(lm((y[intxn==j])~year[intxn==j]))
}


## QUADRATIC MODEL ##
mu_b1 <- mean(goo$mu_b1) # from stan output 
mu_b2 <- mean(goo$mu_b2) # from stan output 
sigma_y <- mean(goo$sigma_y) # from stan output (which I named 'goo' for now)
sigma_b1 <- mean(goo$sigma_b1) # from stan output
sigma_b2 <- mean(goo$sigma_b2) # from stan output

a <- colMeans(goo$a) # this may not be ideal but deals with the non-pooling
b1<-rnorm(Nint, mu_b1, sigma_b1); #generate slopes for each species
b2<-rnorm(Nint, mu_b2, sigma_b2);

ypred <- length(N) # Lizzie added
for (n in 1:N){
     s <- intxn[n]
    ypred[n] <- a[s] + b1[s]*year[n]+b2[s]*year2[n] # one way to handle the non-pooled intercepts, there may be other ways
}
y <- rnorm(N, ypred, sigma_y) 

dr<-data.frame(cbind(intxn, year, y))
ggplot(dr, aes(x=year, y=y, colour=factor(intxn))) + geom_smooth(method="lm", formula=y~x+I(x^2), se=FALSE)+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("fitness_z")+xlab("mismatch")

!!! THE REST IS FOR LOOKING AT SLOPES
### posterior slope from Stan vs histogram of no pooling slopes, and complete pooling
hist(slopefits, main="Dist'n of no pooling slopes w posterior slope (blue) and complete pooling slope (red)")
abline(v = mu_b, col = "blue", lwd = 2) # mean of lm fits
abline(v=coef(comppool)[2], col="red", lwd=2)

##posterior intercepts from stan vs. histogram of no pooling slopes
hist(intfits, col=rgb(1,0,0,0.5), main="Dist'n of no pool ints (pink) w post ints (blue), mu_a (blue), complete pool int (red)")
hist(a, xlim=range(intfits), col=rgb(0,0,1,0.5), add=TRUE)
abline(v = mean(goo$a), col = "blue", lwd = 2) # mean of lm fits
abline(v=coef(comppool)[1], col="red", lwd=2)

# okay, but predictive checks are about much more than ONE simulation, one draw ...
# Create the data using new a and b for each of 71 species, 100 times

ypred<-length(N)
y.sd100 <- matrix(0, ncol=100, nrow=Nint)
for (i in 1:100){
     for (n in 1:N){
         s <- intxn[n]
         ypred[n] <- a[s] + b[s]*year[n] # I think a[s] would also work 
     }
   y <- rnorm(N, ypred, sigma_y)
   y.df <- as.data.frame(cbind(y, intxn))
   y.sd <- aggregate(y.df["y"], y.df["intxn"], FUN=sd)
   y.sd100[,i] <- y.sd[,2] 
}
hist(rowMeans(y.sd100))

## looking at slopes
b100 <- matrix(0, ncol=100, nrow=Nint)
for (j in 1:100){
     b100[,j] <- rnorm(Nint, mean=mu_b, sd=sigma_b)
}

hist(rowMeans(b100), col=rgb(1,0,0,0.5), xlim=range(slopefits), main="Mean(b) from 100 random draws based on Stan model",  ylab="mean(b from one draw)") 
abline(v = mean(slopefits), col = "blue", lwd = 2) # mean of lm fits
hist(slopefits, col=rgb(0,0,1,0.5), add=TRUE)

#100 random draws instead of means
hist(b100, col=rgb(1,0,0,0.5), main="100 random draws based on Stan model",  ylab="") 
abline(v = mean(slopefits), col = "blue", lwd = 2) # mean of lm fits
hist(slopefits, col=rgb(0,0,1,0.5), xlim=range(b100), add=TRUE)

#1 random draws instead of means
hist(b100[,2], col=rgb(1,0,0,0.5), xlim=c(-2.7, 2.8), main="1 random draw based on Stan model",  ylab="") 
abline(v = mean(slopefits), col = "blue", lwd = 2) # mean of lm fits
hist(slopefits, col=rgb(0,0,1,0.5), xlim=c(-2.7, 2.8), add=TRUE)