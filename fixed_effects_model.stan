data {
  int N;      // sample size
  real mpg[N];
  real<lower=4,upper=8> cyl[N];
  real wt[N];
  real qsec[N];
}

parameters {
  vector[4] beta;
  //vector[2] beta;
  real<lower=0> sigma;
}

model {
  real mu;
  
  //priors
  beta[1] ~ normal(0,10);
  beta[2] ~ normal(0,10);
  beta[3] ~ normal(0,10);
  beta[4] ~ normal(0,10);
  sigma ~ cauchy(0,5);
  
  //likelihood function
  for(i in 1:N){
    mu = beta[1] + beta[2]*cyl[i] + beta[3]*wt[i] + beta[4]*qsec[i];
    //mu = beta[1] + beta[2]*wt[i];
    mpg[i] ~ normal(mu, sigma);
  }
}
