//Two-level (1 hierarchical grouping) partial pooling intercept, i.e. random intercept model 

data{
	int<lower=0> N; 				//Level 1: number of observations
	int<lower=0> Nint; 				//Level 2: number of interactions (i.e. grouping factor)
	int intxn[N]; 	//interaction identity
	
	//predictors
	vector[N] year; 	//year of data point
	vector[N] year2; //quadratic term
	
	//response
	real y[N]; 		//mismatch (days or index)
}

parameters{
	// hyperparameters
	real mu_a;            // mean intercept across interaction (population intercept)
  	real<lower=0> sigma_y; 		//measurement error, noise etc. [population sd]

  	real a[Nint]; 		//the intercept for each interaction (random effect)
  	real<lower=0> sigma_a;     // variation of intercept among interaction; (sd of random effect)
  	real b1[Nint]; 		//the slope for each interaction (random effect)
	real b2[Nint]; 		//the slope (curvature) for each interaction (random effect)
  	
}

transformed parameters{
 //Individual mean
 real ypred[N];
 

//Individual mean
for (i in 1:N){
		ypred[i]=a[intxn[i]]+b1[intxn[i]]*year[i]+b2[intxn[i]]*year2[i];
	}
	
}


model{
	//Random effects distribution
	a~normal(mu_a, sigma_a);		
	
	y~normal(ypred, sigma_y);
}
