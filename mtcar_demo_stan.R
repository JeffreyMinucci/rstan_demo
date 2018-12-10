library(rstan)
library(rstanarm)
library(bayesplot)

# Let's use the motortrend car mpg dataset for our example problem
cardata <- mtcars
head(cardata)

# Converting categorical variables to factors
# cardata$vs <- factor(cardata$vs) #inline or V engine config
# cardata$am <- factor(cardata$am) #automatic or manual transmission?

# Put our model's data in a list (the form rstan requires)
stanData <- list(N = nrow(cardata), mpg = cardata$mpg, cyl = cardata$cyl,wt = cardata$wt,
                 qsec = cardata$qsec)


#run the analysis
fit <- stan('fixed_effects_model.stan', data = stanData, iter = 3000, chains = 4, cores = 1)


#look at the trace
traceplot(fit)

#text based summary
summary(fit)

#posterior credible intervals
plot(fit)

#posterior densities
plot(fit, plotfun = 'dens')




### using the stanarm package to simply model specification
fit2 <- stan_glm(mpg ~ cyl+wt+qsec, data = cardata, cores=1)
summary(fit2)
prior_summary(fit2) #we can see the default priors stanarm used

plot(fit2)

#pretty posterior densities using bayesplot package
posterior <- as.matrix(fit2)
mcmc_areas(posterior,prob=0.95, pars=c('cyl', 'wt', 'qsec'))

ppc_dens_overlay(y=cardata$mpg, yrep = posterior_predict(fit2,draws=50))

#compare to an ols linear model
summary(lm(mpg~cyl+wt+qsec,data=cardata))



### Now let's try a mixed effects (hierarchical) model with # of cylinders as a random (intercept) variable
fit_mixed <- stan_glmer(mpg ~ cyl + wt + qsec + (1|vs),data=cardata, cores=1, control = list(adapt_delta = 0.99))
plot(fit_mixed, 'trace')
summary(fit_mixed)
prior_summary(fit_mixed)



