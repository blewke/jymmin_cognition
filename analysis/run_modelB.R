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

all_data = data_preprocessing("../jym_data/Jymmin_Data.csv", dateformat = 'mdy')
all_data = add_nTrialLevel(all_data)
#rdata = random_assignment(all_data)

subject_data = all_data[!substring(all_data$SubjectCode,1,1) == 'X',]


rdata = real_assignment(subject_data)

study_data = rdata[rdata$Period %in% 1:2  & rdata$Stage == 'Exercise',]
study_data = remove_empty_rows(study_data)
#study_data$nTrialLevelScale = study_data$nTrialLevel/mean(study_data$nTrialLevel)

study_data_timed = study_data[study_data$LevelType == 0,]
study_data_timed = add_nTrialScale(study_data_timed)



rm(all_data)
rm(subject_data)
rm(study_data)
rm(rdata)


priorModelB = c (priors_accuracy, priors_duration, priors_jymmin)

ModelB =  brm (
  data = study_data_timed,
  formula =
    bf (DurationInSeconds|vint(ClocksInSet) ~ 1 + Jymmin*nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = sum_shifted_lognormal)+
    bf (nCorrect|vint(ClocksInSet) ~ 1 + Jymmin*nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = beta_binomial2)
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
  file = '../results/ModelB',
  sample_file = '../results/ModelBchaindata',
  control = list(adapt_delta = 0.95, max_treedepth = 14)
)

summary(ModelB)

sessionInfo()

expose_functions(ModelB, vectorize = TRUE)

ModelB = add_criterion(ModelB, 'loo', reloo = TRUE)

loo(ModelB)