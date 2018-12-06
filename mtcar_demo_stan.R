library(rstan)

# Let's use the motortrend car mpg dataset for our example problem
cardata <- mtcars
head(cardata)

# Converting categorical variables to factors
# cardata$vs <- factor(cardata$vs) #inline or V engine config
# cardata$am <- factor(cardata$am) #automatic or manual transmission?

# Put our model's data in a list (the form rstan requires)
stanData <- list(N = nrow(cardata), mpg = cardata$mpg, cyl = cardata$cyl, vs = cardata$vs, wt = cardata$wt,
                 am = cardata$am, qsec = cardata$qsec)

# Define our model in stan's language
model <- c('
           data {
              int<lower=1> N;               //number of data points
              real mpg[N];                  //mpg, our response variable
              int<lower=4,upper=8> cyl[N];  //number of cylinders 
              int<lower=0,upper=1> vs[N];   //0=V config or 1=inline?
              real wt[N];                   //weight 
              int<lower=0,upper=1> am[N];   // 0=auto, 1=manual?
              real qsec[N];                   // quarter mile time
           }
           parameters {
              vector[7] beta;               //1 intercept, 5 terms + 1 interaction
              real<lower=0> sigma_e;        // error sd
           }
           model {
              real mu;
              for (i in 1:N){
                mu = beta[1] + beta[2]*cyl[i]*vs[i] + beta[3]*cyl[i] + beta[4]*vs[i] + beta[5]*wt[i] + beta[6]*am[i] + beta[7]*qsec[i];
                mpg[i] ~ normal(mu, sigma_e);
              }
           }')

#run the analysis
fit <- stan(model_code = model, data = stanData, iter = 2000, chains = 4, cores = 1)


#look at the trace
traceplot(fit, probs = c(0.025, 0.5, 0.975))

#posterior credible intervals
plot(fit)

#posterior densities
plot(fit, plotfun = 'dens')






### Now let's try a mixed effects model with transmission as a random (intercept) variable

