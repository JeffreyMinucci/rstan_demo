library(rstan)
library(rstanarm)
library(bayesplot)

# Let's use the motortrend car mpg dataset for our example problem
cardata <- mtcars
head(cardata)



# What we might do with an ols linear model
summary(lm(mpg~cyl+wt+qsec,data=cardata))


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





### Now let's try a mixed effects (hierarchical) model with # of cylinders as a random (intercept) variable

#using rstanarm (easy mode)
fit_mixed <- stan_glmer(mpg ~  wt + qsec + (1|cyl),data=cardata, cores=1, control = list(adapt_delta = 0.99))
#fit_mixed <- stan_glmer(mpg ~  wt + qsec + (1|cyl),data=cardata, algorithm="fullrank")
plot(fit_mixed, 'trace')
summary(fit_mixed)
prior_summary(fit_mixed)

posterior <- as.matrix(fit_mixed)
mcmc_areas(posterior,prob=0.95, pars=c( 'wt', 'qsec'))
ppc_dens_overlay(y=cardata$mpg, yrep = posterior_predict(fit_mixed,draws=50))



#original rstan
fit_mixed2 <- stan('mixed_model.stan', data = stanData, iter = 3000, chains = 4, cores = 1,
                   control = list(adapt_delta = 0.99))


#look at the trace
traceplot(fit_mixed2)

#text based summary
summary(fit_mixed2)

#posterior credible intervals
plot(fit_mixed2)

#posterior densities
plot(fit_mixed2, plotfun = 'dens')

