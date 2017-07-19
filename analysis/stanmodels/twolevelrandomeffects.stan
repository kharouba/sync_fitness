//Two-level (1 hierarchical grouping) partial pooling intercept, i.e. random intercept model AND slopes

data{
	int<lower=0> N; 				//Level 1: number of observations
	int<lower=0> Nint; 				//Level 2: number of interactions (i.e. grouping factor)
	int intxn[N]; 	//interaction identity
	
	//predictors
	vector[N] year; 	//year of data point
	
	//response
	real y[N]; 		//mismatch (days or index)
}

parameters{
	// hyperparameters
	real mu_a;            // mean intercept across interaction (population intercept)
  	real mu_b;                 // mean slope across interaction  (population slope)
  	real<lower=0> sigma_y; 		//measurement error, noise etc. [population sd]

  	real a[Nint]; 		//the intercept for each interaction (random effect)
	real b[Nint]; 		//the slope for each interaction (random effect)
  	real<lower=0> sigma_a;     // variation of intercept among interaction; (sd of random effect)
  	real<lower=0> sigma_b;     // variation of slope among interaction; (sd of random effect)	
}

transformed parameters{
 //Individual mean
 real ypred[N];
 

//Individual mean
for (i in 1:N){
		ypred[i]=a[intxn[i]]+b[intxn[i]]*year[i];
	}
	
}


model{
	//Random effects distribution
	a~normal(mu_a, sigma_a);		
	b~normal(mu_b, sigma_b);
	
	y~normal(ypred, sigma_y);
}
	