#model sim_dat from file simulate_data
require(brms)

#write.csv(sim_dat, 'sim_dat')

#make the betabinomial custom family
source('~/Documents/examples and tests r/beta binomial family.R')
sim_dat = read.csv('sim_dat')


sim_dat_small = sim_dat[sim_dat$SubjectCode < 6 &sim_dat$Level < 4  & !is.na(sim_dat$SubjectCode),]


m.SimBetaBinomialSlope = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  #prior = priors,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBetaBinomialSlope',
  seed = 9,
  inits = 0,
)

expose_functions(m.SimBetaBinomialSlope, vectorize = TRUE)

summary(m.SimBetaBinomialSlope)
plot(m.SimBetaBinomialSlope)
pp_check(m.SimBetaBinomialSlope)

conditional_effects(m.SimBetaBinomialSlope)

prior_summary(m.SimBetaBinomialSlope)


priors1 = c(prior(exponential(1), class = sd, coef = 'Intercept', group = 'SubjectCode'),
          prior(exponential(1), class = sd, coef = 'Intercept', group = 'LevelType'),
          prior(exponential(0.5), class = sd, coef = 'Intercept', group = 'LevelType:Level'),
          #prior(exponential(1), class = sd, coef = 'Intercept', group = 'LevelType' ),
          prior(student_t(2,0,1), class = b, coef = 'scalenTrialLevel')
)

priors2 = c(prior(exponential(1.5), class = sd),
            prior(student_t(2,0,1), class = b),
            prior(lkj_corr_cholesky(1), class = L)
)


priors3 = c(prior(exponential(1.5), class = sd),
            prior(student_t(2,0,1), class = b),
            prior(lkj_corr_cholesky(1), class = L),
            prior(gamma(3,2), class = phi)
)

m.SimBetaBinomialSlopePrior = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBetaBinomialSlopePrior',
  seed = 9,
  inits = 0,
)



m.SimBinomialSlopePrior = brm(
  Accuracy| trials(ClocksInSet) ~ 0 + scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat, 
  family = binomial, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBinomialSlopePrior',
  seed = 9,
  inits = 0,
)

require(ggplot2)

pp_check(m.SimBinomialSlopePrior)
plot(m.SimBinomialSlopePrior)
mcmc_plot(m.SimBinomialSlopePrior, pars = c("^r_", "^b_", "^sd_","^phi")) +
  ggtitle('SimBinomialSlopePrior')




m.SimSmallBetaBinomialSlopePrior = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors2,
  chains = 2,
  cores = 2,
  iter = 1000,
  file = 'm.SimSmallBetaBinomialSlopePrior2',
  seed = 9,
  #inits = 0,
)

expose_functions(m.SimSmallBetaBinomialSlopePrior, vectorize = TRUE)
pp_check(m.SimSmallBetaBinomialSlopePrior)#
plot(m.SimSmallBetaBinomialSlopePrior)
summary(m.SimSmallBetaBinomialSlopePrior)
conditional_effects(m.SimSmallBetaBinomialSlopePrior)

prior_summary(m.SimSmallBetaBinomialSlopePrior)

f = fitted(m.SimSmallBetaBinomialSlopePrior)
fs = cbind(sim_dat_small, f)

require(ggplot2)


ggplot(sim_dat_small, aes(x = nTrialLevel, y = Accuracy/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(alpha = 0.5)+
  facet_grid(LevelType ~ Level)

#require(gtools)
ggplot(fs, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(alpha = 0.5)+
  facet_grid(LevelType ~ Level)


ggplot(fs, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line()+
  geom_line(aes(y = Accuracy/ClocksInSet),alpha = 0.2)+
  facet_grid(LevelType ~ Level)




#many iterations, ca. 6h
m.SimSmallBetaBinomialSlopePrior3 = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors2,
  chains = 4,
  cores = 4,
  iter = 2000,
  file = 'm.SimSmallBetaBinomialSlopePrior3.2',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)


pp_check(m.SimSmallBetaBinomialSlopePrior3)
plot(m.SimSmallBetaBinomialSlopePrior3)
conditional_effects(m.SimSmallBetaBinomialSlopePrior3)




#fewer iterations
m.SimSmallBetaBinomialSlopePrior3.2 = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors2,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimSmallBetaBinomialSlopePrior3.2',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)





m.SimSmallBetaBinomialSlopePriorOnly2 = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'only',
  prior = priors2,
  chains = 2,
  cores = 2,
  iter = 2000,
  file = 'm.SimSmallBetaBinomialSlopePriorOnly2',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)

expose_functions(m.SimSmallBetaBinomialSlopePriorOnly2, vectorize = TRUE)
pp_check(m.SimSmallBetaBinomialSlopePriorOnly2)

fp = fitted(m.SimSmallBetaBinomialSlopePriorOnly2)
fps = cbind(sim_dat_small, fp)


ggplot(fps, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(alpha = 0.5)+
  facet_wrap(sim_dat_small$Level+(sim_dat_small$LevelType)*nLevelsPerType)



m.SimSmallBinomialSlopePriorOnly2 = brm(
  Accuracy| trials(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_small, 
  family = binomial, 
  stanvars = stanvars,
  sample_prior = 'only',
  prior = priors2,
  chains = 2,
  cores = 2,
  iter = 2000,
  file = 'm.SimSmallBinomialSlopePriorOnly2',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)


fpbin = fitted(m.SimSmallBinomialSlopePriorOnly2)
fpbins = cbind(sim_dat_small, fpbin)


ggplot(fpbins, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(alpha = 0.5)+
  facet_grid(LevelType ~ Level)

plot(m.SimSmallBinomialSlopePriorOnly2)


m.SimSmallBinomialPriorOnly2 = brm(
  Accuracy| trials(ClocksInSet) ~  +(1 |SubjectCode) + (1 |LevelType/Level), 
  data = sim_dat_small, 
  family = binomial, 
  stanvars = stanvars,
  sample_prior = 'only',
  #prior = priors2,
  chains = 2,
  cores = 2,
  iter = 2000,
  file = 'm.SimSmallBinomialPriorOnly2',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)

plot(m.SimSmallBinomialPriorOnly2)
fpbin2 = fitted(m.SimSmallBinomialPriorOnly2)
fpbins2 = cbind(sim_dat_small, fpbin2)

ggplot(fpbins2, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(alpha = 0.5)+
  #facet_wrap(sim_dat_small$Level+(sim_dat_small$LevelType)*nLevelsPerType)
  facet_grid(LevelType ~ Level)

pp_check(m.SimSmallBinomialPriorOnly2)


m.SimSmallBetaBinomialSlopePrior3phi = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors3,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimSmallBetaBinomialSlopePrior3phi',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)

plot(m.SimSmallBetaBinomialSlopePrior3phi)
