
//Two-level (1 hierarchical grouping) partial pooling intercept, i.e. random intercept model

data{
	int<lower=0> N; 				//Level 1: Number of observations
	int<lower=0> Nint; 				//Level 2: Number of interactions (Grouping factor)
	int intxn[N]; 	//interactions identity, coded as int
	
	//predictors
	vector[N] year; 	//year of data point
	
	//response
	real y[N]; 		//mismatch (days or index)

}

parameters{	
	// hyperparameters
	real mu_a;				//mean intercept across interactions (population)
	
	real<lower=0> sigma_y; 	//measurement error, noise etc. (population standard deviation)
	
	real a[Nint]; 		//the intercept for each interaction
	real b[Nint]; 		//the slope for each interaction 
	real<lower=0> sigma_a;	//variation of intercept among interactions; [sd of random effects]
	
}

transformed parameters{
 //Individual mean
 real ypred[N];
 
//Individual mean
for (i in 1:N){
		ypred[i]<-a[intxn[i]]+b[intxn[i]]*year[i];
	}
	
}

model{
	//Random effects distribution	
	a~normal(mu_a, sigma_a);
	y~normal(ypred, sigma_y);
}