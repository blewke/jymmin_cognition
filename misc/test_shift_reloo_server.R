require(brms)
require(tibble)

dat = tibble( y= c(11, 10,11,12,11,15,11,12,12,11,13,14,13,14,12,11,12,11,11,12,11,15,11,12,12,11,13,14,13,14,12,11,12,16))


print('shifted_lognormal')

print('m.shift')

m.shift = brm(
  data = dat,
  formula = y ~ 1,
  family = shifted_lognormal
  )

summary(m.shift)

l = loo(m.shift)
l

lreloo = loo(m.shift, reloo = TRUE)
lreloo

#### off-the shelf-family , with problematic point

print('m.shift2')

m.shift2 = brm(
  data = rbind(dat, tibble(y =4)),
  formula = y ~ 1,
  family = shifted_lognormal
  
)

l2 = loo(m.shift2)
l2

lreloo2 = loo(m.shift2, reloo = TRUE)
lreloo2

#summary(m.shift2)

#prior_summary(m.shift2)


#source('../misc/test_sum_shifted_lognormal family.R')
source('../helper_functions/sum_shifted_lognormal family.R')

stanvars_ssln <- stanvar(scode = stan_funs_ssln, block = "functions")

stanvars_ssln_zero = stanvar(scode = stan_funs_ssln, block = "functions")

#require(loo)

print('m.shift_sum0')


#---- no problematic point, prob = 0

m.shift_sum0 = brm(
  data = dat,
  formula = bf(y|vint(1) ~ 1, family = sum_shifted_lognormal),
  stanvars = stanvars_ssln_zero,
  control = list(adapt_delta = 0.99),
  seed = 100,
  #file = 'test_m.shift_sum',
  prior = c(prior(gamma(7,1), class = shift),
            prior(normal(1,2), class = Intercept),
            prior(normal(0,2), class = sigma))
)

#plot(m.shift_sum0)
summary(m.shift_sum0)

expose_functions(m.shift_sum0, vectorize = TRUE)

ls0 = brms::loo(m.shift_sum0)
ls0

#pp_check(m.shift_sum0)

lreloos0 = loo(m.shift_sum0, reloo = TRUE)
lreloos0


#------ with problematic point


#-------with problematic point, and 0 in lpdf

print('m.shift_sum2.0')

m.shift_sum2.0 = brm(
  data = rbind(dat, tibble(y = 4)),
  formula = bf(y|vint(1) ~ 1, family = sum_shifted_lognormal),
  stanvars = stanvars_ssln_zero,
  control = list(adapt_delta = 0.99),
  seed = 100,
  save_pars = save_pars(all=TRUE),
  #file = 'test_m.shift_sum',
  prior = c(prior(gamma(7,1), class = shift),
            prior(normal(1,2), class = Intercept),
            prior(normal(0,2), class = sigma))
)

#plot(m.shift_sum2.0)
summary(m.shift_sum2.0)

expose_functions(m.shift_sum2.0, vectorize = TRUE)

ls2.0 = brms::loo(m.shift_sum2.0)
ls2.0

#pp_check(m.shift_sum2.0)

#lms2.0 = loo(m.shift_sum2.0, moment_match = TRUE)
#lms2.0

lreloos2.0 = loo(m.shift_sum2.0, reloo = TRUE)
lreloos2.0


#m.shift_sum2 = add_criterion(m.shift_sum2, 'loo', reloo = TRUE)

#loo(m.shift_sum2)

