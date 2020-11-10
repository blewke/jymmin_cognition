#model sim_dat from file simulate_data
require(brms)
require(ggplot2)

#write.csv(sim_dat, 'sim_dat')

#make the betabinomial custom family
source('~/Documents/examples and tests r/beta binomial family.R')


sim_dat_beta = read.csv('sim_dat_beta.csv')
sim_dat_beta_slope = read.csv('sim_dat_beta_slope.csv')


sim_dat_beta$obs = 1:nrow(sim_dat_beta)


sim_dat_beta_small = sim_dat_beta[sim_dat_beta$SubjectCode < 6 &sim_dat_beta$Level < 4  & !is.na(sim_dat_beta$SubjectCode),]
sim_dat_beta_slope_small = sim_dat_beta_slope[sim_dat_beta_slope$SubjectCode < 6 &sim_dat_beta_slope$Level < 4  & !is.na(sim_dat_beta_slope$SubjectCode),]



ggplot(sim_dat_beta_slope, aes(x = nTrialLevel, y = Accuracy/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(alpha = 0.5)+
  facet_grid(LevelType ~ Level)



###
m.SimBSBetaBinomialSlope = brm(
  Accuracy| vint(ClocksInSet) ~ 0 + scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_beta_slope_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  #prior = priors,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSBetaBinomialSlope',
  seed = 25,
  #inits = 0,
)

expose_functions(m.SimBSBetaBinomialSlope, vectorize = TRUE)

summary(m.SimBSBetaBinomialSlope)
plot(m.SimBSBetaBinomialSlope)
pp_check(m.SimBSBetaBinomialSlope)

conditional_effects(m.SimBSBetaBinomialSlope)

prior_summary(m.SimBSBetaBinomialSlope)


pp_check(m.SimBSBetaBinomialSlope)

fu = fitted(m.SimBSBetaBinomialSlope)
fus = cbind(sim_dat_beta_slope_small, fu)


ggplot(fus, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  #geom_ribbon(aes(ymin = Q2.5/ClocksInSet, ymax = Q97.5/ClocksInSet ), alpha = 0.2, linetype = 0)+
  geom_line(alpha = 1)+
  #geom_line(data=frs)+
  #facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  facet_grid(LevelType ~ Level ~ SubjectCode)





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


priors4 = c(prior(student_t(2,0,1), class = b)
)



m.SimBetaBinomialSlopePrior = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_beta_slope_small, 
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


######
m.SimBSBinomialSlopePrior = brm(
  Accuracy| vint(ClocksInSet) ~ 1 + scale(nTrialLevel), 
  data = sim_dat_beta_slope_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors4,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSBinomialSlopePrior',
  seed = 25,
  #inits = 0,
)

#require(ggplot2)

expose_functions(m.SimBSBinomialSlopePrior, vectorize = TRUE)
m.SimBSBinomialSlopePrior <- add_criterion(m.SimBSBinomialSlopePrior, "loo")


pp_check(m.SimBSBinomialSlopePrior)
plot(m.SimBSBinomialSlopePrior)
mcmc_plot(m.SimBSBinomialSlopePrior, pars = c("^r_", "^b_", "^sd_","^phi")) +
  ggtitle('SimBSBinomialSlopePrior')


conditional_effects(m.SimBSBinomialSlopePrior)

prior_summary(m.SimBSBinomialSlopePrior)


fx = fitted(m.SimBSBinomialSlopePrior)
fxs = cbind(sim_dat_beta_slope_small, fx)


ggplot(fxs, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  #geom_ribbon(aes(ymin = Q2.5/ClocksInSet, ymax = Q97.5/ClocksInSet ), alpha = 0.2, linetype = 0)+
  geom_line(alpha = 1)+
  #geom_line(data=frs)+
  #facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  facet_grid(LevelType ~ Level ~ SubjectCode)







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
  file = 'm.SimSmallBetaBinomialSlopePrior3',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)


pp_check(m.SimSmallBetaBinomialSlopePrior3)
plot(m.SimSmallBetaBinomialSlopePrior3)
conditional_effects(m.SimSmallBetaBinomialSlopePrior3)



#####
#fewer iterations
m.SimBSSmallBetaBinomialSlopePrior3.2 = brm(
  Accuracy| vint(ClocksInSet) ~ 0 + scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_beta_slope_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors3,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSSmallBetaBinomialSlopePrior3.2',
  seed = 25,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)


expose_functions(m.SimBSSmallBetaBinomialSlopePrior3.2, vectorize = TRUE)
m.SimBSSmallBetaBinomialSlopePrior3.2 = add_criterion(m.SimBSSmallBetaBinomialSlopePrior3.2, "loo")


summary(m.SimBSSmallBetaBinomialSlopePrior3.2)
plot(m.SimBSSmallBetaBinomialSlopePrior3.2)
pp_check(m.SimBSSmallBetaBinomialSlopePrior3.2)

conditional_effects(m.SimBSSmallBetaBinomialSlopePrior3.2)

prior_summary(m.SimBSSmallBetaBinomialSlopePrior3.2)


fv = fitted(m.SimBSSmallBetaBinomialSlopePrior3.2)
fvs = cbind(sim_dat_beta_slope_small, fv)


ggplot(fvs, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  geom_ribbon(aes(ymin = Q2.5/ClocksInSet, ymax = Q97.5/ClocksInSet ), alpha = 0.5, linetype = 0)+
  geom_line(alpha = 1)+
  #geom_line(data=frs)+
  #facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  facet_grid(LevelType ~ Level ~ SubjectCode)





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


###
m.SimBSSmallBinomialSlopePrior2 = brm(
  Accuracy| trials(ClocksInSet) ~ 0 + scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_beta_slope_small, 
  family = binomial, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors2,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSSmallBinomialSlopePrior2',
  seed = 25,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)

m.SimBSSmallBinomialSlopePrior2 = add_criterion(m.SimBSSmallBinomialSlopePrior2, "loo")


summary(m.SimBSSmallBinomialSlopePrior2)
plot(m.SimBSSmallBinomialSlopePrior2)
pp_check(m.SimBSSmallBinomialSlopePrior2)

conditional_effects(m.SimBSSmallBinomialSlopePrior2)

prior_summary(m.SimBSSmallBinomialSlopePrior2)

fw = fitted(m.SimBSSmallBinomialSlopePrior2)
fws = cbind(sim_dat_beta_slope_small, fw)


ggplot(fws, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  geom_ribbon(aes(ymin = Q2.5/ClocksInSet, ymax = Q97.5/ClocksInSet ), alpha = 0.5, linetype = 1)+
  geom_line(alpha = 1)+
  #geom_line(data=frs)+
  #facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  facet_grid(LevelType ~ Level ~ SubjectCode)



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
  data = sim_dat_beta_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors3,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSmallBetaBinomialSlopePrior3phi',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)

plot(m.SimSmallBetaBinomialSlopePrior3phi)



expose_functions(m.SimSmallBetaBinomialSlopePrior3phi, vectorize = TRUE)
pp_check(m.SimSmallBetaBinomialSlopePrior3phi)

fp = fitted(m.SimSmallBetaBinomialSlopePrior3phi)
fps = cbind(sim_dat_beta_small, fp)


ggplot(fps, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.4)+
  geom_line(alpha = 1)+
  facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType)



m.SimSmallBetaBinomialSlopePrior2Obs = brm(
  Accuracy| trials(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level) + (1|obs), 
  data = sim_dat_beta_small, 
  family = binomial, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors2,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSmallBetaBinomialSlopePrior2Obs',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)

plot(m.SimSmallBetaBinomialSlopePrior2Obs)


pp_check(m.SimSmallBetaBinomialSlopePrior2Obs)

fq = fitted(m.SimSmallBetaBinomialSlopePrior2Obs)
fqs = cbind(sim_dat_beta_small, fq)


ggplot(fqs, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  geom_line(alpha = 0.5)+
  geom_line(data=fps)+
  facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  #facet_grid(LevelType ~ Level)





m.SimSmallBetaBinomialSlopePrior3phi5 = brm(
  Accuracy| vint(ClocksInSet) ~ scale(nTrialLevel) +(1 + scale(nTrialLevel)|SubjectCode) + (1 + scale(nTrialLevel)|LevelType/Level), 
  data = sim_dat_beta_slope_small, 
  family = beta_binomial2, 
  stanvars = stanvars,
  sample_prior = 'yes',
  prior = priors3,
  chains = 2,
  cores = 4,
  iter = 100,
  file = 'm.SimBSmallBetaBinomialSlopePrior3phi5',
  seed = 9,
  #inits = 0,
  control = list(adapt_delta = 0.92)
)


plot(m.SimSmallBetaBinomialSlopePrior3phi5)

expose_functions(m.SimSmallBetaBinomialSlopePrior3phi5, vectorize = TRUE)

pp_check(m.SimSmallBetaBinomialSlopePrior3phi5)

fr = fitted(m.SimSmallBetaBinomialSlopePrior3phi5)
frs = cbind(sim_dat_beta_slope_small, fr)


ggplot(frs, aes(x = nTrialLevel, y = Estimate/ClocksInSet, color = as.factor(SubjectCode)))+
  #geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  geom_ribbon(aes(ymin = Q2.5/ClocksInSet, ymax = Q97.5/ClocksInSet ), alpha = 0.2, linetype = 0)+
  geom_line(alpha = 0.5)+
  #geom_line(data=frs)+
  #facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  facet_grid(LevelType ~ Level)

fr2 = posterior_predict(m.SimSmallBetaBinomialSlopePrior3phi5)


require(tibble)
mean = rep(NA,ncol(fr2))
fr2.1 = tibble(mean,Q2.5 = NA, Q97.5 = NA)

for (s in 1:ncol(fr2)){
  fr2.1$mean[s] = mean(fr2[,s])
  fr2.1$Q2.5[s] = quantile(fr2[,s],probs = 0.025)
  fr2.1$Q97.5[s] = quantile(fr2[,s],probs = 0.975)
}

frs2 = cbind(sim_dat_beta_slope_small, fr2.1)

ggplot(frs2, aes(x = nTrialLevel, y = mean/ClocksInSet, color = as.factor(SubjectCode)))+
  #geom_line(aes(y = Accuracy/ClocksInSet), alpha = 0.2)+
  geom_ribbon(aes(ymin = Q2.5/ClocksInSet, ymax = Q97.5/ClocksInSet ), alpha = 0.2, linetype = 1)+
  geom_line(alpha = 0.5)+
  #geom_line(data=frs)+
  #facet_wrap(sim_dat_beta_small$Level+(sim_dat_beta_small$LevelType)*nLevelsPerType*sim_dat_beta_small$SubjectCode)
  facet_grid(LevelType ~ Level~ SubjectCode)




WAIC(m.SimBSSmallBetaBinomialSlopePrior3.2, m.SimBSBinomialSlopePrior)

loo_compare(m.SimBSSmallBetaBinomialSlopePrior3.2, m.SimBSBinomialSlopePrior, criterion = 'loo')

lc = loo_compare(m.SimBSSmallBetaBinomialSlopePrior3.2, m.SimBSSmallBinomialSlopePrior2, m.SimBSBinomialSlopePrior,criterion = 'loo')

print(lc,simplify = FALSE)

loo_compare(m.SimBSSmallBinomialSlopePrior2, m.SimBSBinomialSlopePrior, criterion = 'loo')

