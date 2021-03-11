
#require(cmdstanr)
require(tibble)
require(brms)


source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')

stanvars_bb_ssln <- stanvar(scode = paste(stan_funs_ssln, stan_funs), block = "functions")

dat = tibble( y = c(1,2,2,2,1,1,1,   2,1,2,3,2,2, 3,3,2,4,3,3,2,3,  4,5,3,4,4,3,4,4,3), 
              x = c(6,6,6,6,6,6,6,   7,7,7,7,7,7, 8,8,8,8,8,8,8,8,  9,9,9,9,9,9,9,9,9))

dif_point = tibble(x =100,y =0)


#dat = tibble( x = c(1,2,2,2,1,1,1,1,3,0,2,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,2,2,1,2,1,2,1,2,2), 
#              y = c(6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6))
              
#dif_point = tibble(x = 10, y= 7)
dat2 = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dif_point)

print('fit model m1')


m1 = brm(data = dat2,formula = y|trials(10) ~ x, cores = 4, 
              family = binomial,
              #file = 'test_brms',
              backend = 'rstan',
              iter = 10000,
              control = list(adapt_delta = 0.999),
              #future = TRUE,
              save_pars = save_pars(all=TRUE),
              #stanvars = stanvars_bb_ssln
              )



#pp_check(m1)

#lm1 = loo(m1, reloo = TRUE)
#lm2 = loo(m2, reloo = TRUE)

#loo(m1, moment_match =  TRUE)
#loo(m1)

print('compute loo with reloo for m1')
#m1 = add_criterion(m1, 'loo', reloo = 'TRUE', overwrite = TRUE)
#print('compute loo with reloo for m2')
#m2 = add_criterion(m2, 'loo', reloo = 'TRUE')

#m1$criteria = NULL


l = loo(m1, reloo = TRUE, reloo_args = list(cores = 4))
l

l2 = loo(m1, reloo = TRUE, reloo_args = list(cores = 2))

#lrstan = loo(m1rstan, reloo = TRUE, reloo_args = list(cores = 4))


#reloo1 <- reloo(m1, loo = loo(m1, overwrite = TRUE), chains = 3)

#expose_functions(m1rstan, vectorize = TRUE)

#loo(m1rstan)
#loo(m1rstan, moment_match = TRUE)
#loo(m1rstan, reloo = TRUE)

#loo_compare(m1,m2)

sessionInfo()

