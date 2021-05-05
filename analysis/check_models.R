#Model checking
require(brms)
require(ggplot2)


#uncomment which model should be checked


m = brm(file = '../results/ModelB_20210326') # model B
#m = brm(file = '../results/ModelA_20210322') # model A
#m = brm(file = '../results/modelB_noslope_20210330') # model C

#m = brm(file = '../results/ModelB_sim_20210330') # model B_sim
#m = brm(file = '../results/ModelA_sim_20210330') # model A_sim



#make the distributions from the custom families availble
expose_functions(m, vectorize = TRUE)
source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')


#visually check posteriors and MCMC traceplots
plot(m)


# visually check the fit with posterior predictive plots and the bayesplot package, with mostly default options:

#ResponseTime
pp_check(m, resp = 'ResponseTime')

#nCorrect
pp_check(m, resp = 'nCorrect', type = 'bars')



#ignore the parameters that are only needed for technical reasons
pars = parnames(m)[!startsWith(parnames(m), 'z_') & !startsWith(parnames(m), 'L_')  & !startsWith(parnames(m), 'Intercept')]


#check rhats

min(rhat(m)[pars])
max(rhat(m)[pars])


#check n_eff (effective sample size)
f = rstan::summary(m$fit)$summary
min(f[pars,'n_eff'])
max(f[pars,'n_eff'])



    