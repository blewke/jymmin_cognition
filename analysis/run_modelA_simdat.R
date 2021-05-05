require(brms)

#set the number of cores for reloo
options(mc.cores = 6)

#brms families
source("../analysis/data_preprocessing.R")
source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')

stanvars_bb_ssln <- stanvar(scode = paste(stan_funs_ssln, stan_funs), block = "functions")

#priors
source("../analysis/priors.R")

sim_dat = read.csv('../jym_data/sim_dat_select.csv')

names(sim_dat)[names(sim_dat) == "RT"] <- "ResponseTime"
names(sim_dat)[names(sim_dat) == "Accuracy"] <- "nCorrect"

sim_dat$Jymmin[sim_dat$Jymmin == 1] = 'Yes'
sim_dat$Jymmin[sim_dat$Jymmin == 0] = 'No'

sim_dat = add_nTrialScaled(sim_dat)

filename = '../results/ModelA_sim_20210330'

priorModelB = c (priors_accuracy, priors_duration)

ModelB_sim =  brm (
  data = sim_dat,
  formula =
    bf (ResponseTime|vint(ClocksInSet) ~ 1 + nTrialScaled + (1 + nTrialScaled|s|SubjectCode) + (1 + nTrialScaled|l|Level) , family = sum_shifted_lognormal)+
    bf (nCorrect|vint(ClocksInSet) ~ 1 + nTrialScaled + (1 + nTrialScaled|s|SubjectCode) + (1 + nTrialScaled|l|Level) , family = beta_binomial2)
  ,
  prior = priorModelB,
  #backend = 'cmdstanr',
  sample_prior = 'yes',
  save_pars = save_pars(all=TRUE),
  stanvars = stanvars_bb_ssln, 
  cores = 6,
  chains = 6,
  warmup = 1000,
  iter = 1800,
  seed = 4,
  file = filename,
  sample_file = '../results/ModelAsimchaindata',
  control = list(adapt_delta = 0.97, max_treedepth = 14)
)

summary(ModelB_sim)

sessionInfo()

expose_functions(ModelB_sim, vectorize = TRUE)

#loo(ModelB_sim)

ModelB_sim = add_criterion(ModelB_sim, 'loo', reloo = TRUE, file = filename)

loo(ModelB_sim)
