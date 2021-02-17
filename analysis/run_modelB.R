require(brms)
#require(tibble)
#require(tidyverse)

#brms families
source("../analysis/data_preprocessing.r")
source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')
#source('../helper_functions/stanvars bb ssln.R')

#stanvars_ssln <- stanvar(scode = stan_funs_ssln, block = "functions")
#stanvars_bb_ssln <- stanvar(scode = stan_funs_bb_ssln, block = "functions")
#stanvars_bb <- stanvar(scode = stan_funs, block = "functions")
stanvars_bb_ssln <- stanvar(scode = paste(stan_funs_ssln, stan_funs), block = "functions")


#priors
source("../analysis/priors.r")

#load data
#source("/Users/brittalewke/Documents/Canada Data and scripts/jymmin_cognition/analysis/data_preprocessing.r")

all_data = data_preprocessing("/Users/brittalewke/Documents/Canada Data and scripts/Jymmin_data.csv", dateformat = 'mdy')
all_data = add_nTrialLevel(all_data)
#rdata = random_assignment(all_data)

subject_data = all_data[!substring(all_data$SubjectCode,1,1) == 'X',]


rdata = real_assignment(subject_data)

study_data = rdata[rdata$Period %in% 1:2  & rdata$Stage == 'Exercise',]
study_data = remove_empty_rows(study_data)
study_data$nTrialLevelScale = study_data$nTrialLevel/mean(study_data$nTrialLevel)

study_data_timed = study_data[study_data$LevelType == 0,]

rm(all_data)
rm(subject_data)
rm(study_data)
rm(rdata)


priorModelB = c (priors_accuracy, priors_duration, priors_jymmin)

ModelB =  brm (
  data = study_data_timed,
  formula =
    bf (DurationInSeconds|vint(ClocksInSet) ~ 1 + Jymmin*nTrialLevelScale + (1 + nTrialLevelScale|s|SubjectCode) + (1 + nTrialLevelScale|l|Level) , family = sum_shifted_lognormal)+
    bf (nCorrect|vint(ClocksInSet) ~ 1 + Jymmin*nTrialLevelScale + (1 + nTrialLevelScale|s|SubjectCode) + (1 + nTrialLevelScale|l|Level) , family = beta_binomial2)
  ,
  prior = priorModelB,
  #backend = 'cmdstanr',
  sample_prior = 'yes',
  save_pars = save_pars(all=TRUE),
  stanvars = stanvars_bb_ssln, 
  cores = 4,
  chains = 4,
  iter = 20,
  seed = 4,
  #file = '../results/ModelB2',
  sample_file = '../results/ModelB2chaindata',
  control = list(adapt_delta = 0.99, max_treedepth = 14)
  
)

expose_functions(ModelB, vectorize = TRUE)

ModelB_loo = loo(ModelB, moment_match = TRUE)

save(list = 'ModelB_loo', file ='../results/ModelB3_loo.RData')
