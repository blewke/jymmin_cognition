
require(cmdstanr)
require(tibble)
require(brms)


source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')

stanvars_bb_ssln <- stanvar(scode = paste(stan_funs_ssln, stan_funs), block = "functions")

dat = tibble( x = c(1,2,3,4,5), y = c(6,7,8,9,0))

print('fit model m1')
m1 = brm(data = dat,formula = y|vint(10) ~ x, cores = 4, 
         family = beta_binomial2,
         #file = 'test_brms',
         backend = 'cmdstanr',
         stanvars = stanvars_bb_ssln)

#summary(m1)
print('fit model m2')
m2 = brm(data = dat,formula = y|trials(10) ~ x, cores = 4, 
         family = binomial,
         #file = 'test_brms',
         backend = 'cmdstanr',
         stanvars = stanvars_bb_ssln)

print('fit a helper model with rstan as backend, just to be able to expose its functions. shoudl warning aoccur for this fit, they can be ignored.')
mrstangauss = brm(data = data.frame(y =  c(1,2)),formula = y ~ 1, chains = 2, cores = 2, 
         family = gaussian,
         #file = 'test_brms',
         #backend = 'cmdstanr',
         stanvars = stanvars_bb_ssln,
         #save_pars = save_pars(all=TRUE),
         backend = 'rstan',
         iter = 1500,
         seed = 20
         )



#expose_functions(mrstan, vectorize = TRUE)
print('expose the functions of the helper model. expect a lot of ignorable warnings')
print('*****************************************')
expose_functions(mrstangauss, vectorize = TRUE)
print('*****************************************')
print('end of ignorable warnings')

#pp_check(m1)

#lm1 = loo(m1, reloo = TRUE)
#lm2 = loo(m2, reloo = TRUE)

#loo(m1, moment_match =  TRUE)
#loo(m1)

print('compute loo with reloo for m1')
m1 = add_criterion(m1, 'loo', reloo = 'TRUE')
print('compute loo with reloo for m2')
m2 = add_criterion(m2, 'loo', reloo = 'TRUE')


loo_compare(m1,m2)

sessionInfo()

