require(brms)
#require(cmdstanr)

#brms families
source("../analysis/data_preprocessing.R")
source('../misc/test_sum_shifted_lognormal family.R')
#source('../helper_functions/beta binomial family.R')

#stanvars_bb_ssln <- stanvar(scode = paste(stan_funs_ssln, stan_funs), block = "functions")
stanvars_ssln <- stanvar(scode = stan_funs_ssln, block = "functions")


#priors
#source("../analysis/priors.R")

#all_data = data_preprocessing("../jym_data/Data_blindx.csv")

all_data = data_preprocessing("../jym_data/Jymmin_Data.csv", dateformat = 'mdy')
all_data = add_nTrialLevel(all_data)
#rdata = random_assignment(all_data)

#subject_data = all_data[all_data$SubjectCode %in% 1:24,]
subject_data = all_data[!substring(all_data$SubjectCode,1,1) == 'X',]


rdata = real_assignment(subject_data)

study_data = rdata[rdata$Period %in% 1:2  & rdata$Stage == 'Exercise',]
study_data = remove_empty_rows(study_data)

study_data_timed = study_data[study_data$LevelType == 0,]
study_data_timed = add_nTrialScaled(study_data_timed)


rm(all_data)
rm(subject_data)
rm(study_data)
rm(rdata)


priors_duration = 
  c(
   
    prior(normal(1.5,0.5), class = Intercept),
    
    prior(normal(0,0.3), class = sd, group = 'SubjectCode'),   
  
    prior(normal(1,0.25), class = Intercept, dpar = 'shift'),
    prior(normal(0,0.25), class = sd, dpar = 'shift', group = 'SubjectCode'),
    
    prior(exponential(1.2), class = Intercept, dpar = 'sigma'),
    prior(normal(0,0.2), class = sd, dpar = 'sigma', group = 'SubjectCode'),

    prior(normal(-0.25, 0.25), class = b, coef = 'nTrialScaled'),
      
    prior(normal(0,0.2), class = sd, coef = 'nTrialScaled', group = 'SubjectCode')
    
  )







priorModelA_rt = priors_duration

ModelA =  brm (
  data = study_data_timed[study_data_timed$Level == 5,],
  formula =
    bf (ResponseTime|vint(ClocksInSet) ~ 1 + nTrialScaled + (1 + nTrialScaled|s|SubjectCode),
        shift ~ (1|SubjectCode),
        sigma ~ (1|SubjectCode),
        family = sum_shifted_lognormal)#+
    #bf (nCorrect|vint(ClocksInSet) ~ 1 + nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = beta_binomial2)
  ,
  prior = priorModelA_rt,
  #backend = 'cmdstanr',
  sample_prior = 'yes',
  save_pars = save_pars(all=TRUE),
  stanvars = stanvars_ssln, 
  cores = 6,
  chains = 6,
  warmup = 1000,
  iter = 1800,
  seed = 8,
  file = '../results/ModelA_variable_shiftsigma_20210316_3',
  sample_file = '../results/ModelArtchaindata_shiftsigma_20210316',
  control = list(adapt_delta = 0.95, max_treedepth = 14)
  
)

sessionInfo()

summary(ModelA)

expose_functions(ModelA, vectorize = TRUE)

ModelA_loo = loo(ModelA)

ModelA_loo

print('try reeloo')
options(mc.cores = 6)
#ModelA_reloo = loo(ModelA, reloo = TRUE, reloo_args = list(cores = 6, chains = 6))
ModelA_reloo = loo(ModelA, reloo = TRUE)
ModelA_reloo

#save(list = 'ModelA_reloo', file ='../results/ModelA_variable_shift.RData')

#model$loo <- loo(model, reloo= TRUE)

