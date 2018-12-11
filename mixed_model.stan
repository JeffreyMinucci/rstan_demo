data {
  int N;      // sample size
  real mpg[N];
  int<lower=4,upper=8> cyl[N];
  real wt[N];
  real qsec[N];
}

transformed data{ //manually create dummy variables for cyl
  vector[N] cyl4;
  vector[N] cyl6;
  vector[N] cyl8;
  for (i in 1:N){
    cyl4[i] = cyl[i] == 4;
    cyl6[i] = cyl[i] == 6;
    cyl8[i] = cyl[i] == 8;
  }
  
}
parameters {
  vector[3] beta;
  vector[3] b;    // intercepts for each cylinder group
  real<lower=0> sigma;
  real<lower=0> tau;  // sd of distribution the gammas are drawn from
}

model {
  real mu;
  
  //priors
  beta[1] ~ normal(0,10);
  beta[2] ~ normal(0,10);
  beta[3] ~ normal(0,10);
  sigma ~ cauchy(0,5);
  
  tau ~ cauchy(0,5);
  b[1] ~ normal(0,tau);
  b[2] ~ normal(0,tau);
  b[3] ~ normal(0,tau);

  
  //likelihood function
  for(i in 1:N){
    mu = beta[1] + beta[2]*wt[i] + beta[3]*qsec[i] + b[1]*cyl4[i] + b[2]*cyl6[i] + b[3]*cyl8[i];
    mpg[i] ~ normal(mu, sigma);
  }
}
