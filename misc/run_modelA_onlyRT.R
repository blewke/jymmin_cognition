require(brms)
#require(cmdstanr)

#brms families
source("../analysis/data_preprocessing.R")
source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')

stanvars_bb_ssln <- stanvar(scode = paste(stan_funs_ssln, stan_funs), block = "functions")

#priors
source("../analysis/priors.R")

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
study_data_timed = add_nTrialScale(study_data_timed)


rm(all_data)
rm(subject_data)
rm(study_data)
rm(rdata)


priors_duration = #prior(student_t(3,2,8), class = Intercept, resp = 'DurationInSeconds'),
  c(
    #prior(student_t(3,2,2.5), class = Intercept, resp = 'DurationInSeconds'), ## new!
    #prior(student_t(3,1,1), class = Intercept, resp = 'DurationInSeconds'),
    prior(normal(1.5,1.75), class = Intercept),# resp = 'DurationInSeconds'),
    
    #prior(student_t(3,0,1.5), class = sd, group = 'Level', resp = 'DurationInSeconds'), # new!!
    #prior(student_t(3,0,1.5), class = sd, group = 'SubjectCode', resp = 'DurationInSeconds'),
    prior(normal(0,0.5), class = sd, group = 'Level'),# resp = 'DurationInSeconds'), 
    prior(normal(0,0.5), class = sd, group = 'SubjectCode'),# resp = 'DurationInSeconds'),
    
    #prior(student_t(3,0,5), class = sigma, resp = 'DurationInSeconds'),
    prior(normal(0,1), class = sigma),# resp = 'DurationInSeconds'),
    
    ##!!prior(normal(1,2), class = shift, resp = 'DurationInSeconds'),
    #prior(student_t(3,2,3), class = shift, resp = 'DurationInSeconds'),
    prior(gamma(2,1), class = shift),# resp = 'DurationInSeconds'),
    
    #prior(student_t(3, -0.5, 1.5), class = b, coef = 'nTrialLevelScale', resp = 'DurationInSeconds'),
    prior(normal(-0.25, 0.5), class = b, coef = 'nTrialScale'),# resp = 'DurationInSeconds'),
    
    #(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'DurationInSeconds'),
    #prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'DurationInSeconds')
    
    prior(normal(0,0.25), class = sd, coef = 'nTrialScale', group = 'SubjectCode'),# resp = 'DurationInSeconds'),
    prior(normal(0,0.25), class = sd, coef = 'nTrialScale', group = 'Level')#, resp = 'DurationInSeconds')
    
  )







priorModelA_rt = priors_duration

ModelA =  brm (
  data = study_data_timed,
  formula =
    bf (DurationInSeconds|vint(ClocksInSet) ~ 1 + nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = sum_shifted_lognormal)#+
    #bf (nCorrect|vint(ClocksInSet) ~ 1 + nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = beta_binomial2)
  ,
  prior = priorModelA_rt,
  #backend = 'cmdstanr',
  sample_prior = 'yes',
  save_pars = save_pars(all=TRUE),
  stanvars = stanvars_bb_ssln, 
  cores = 6,
  chains = 6,
  warmup = 1000,
  iter = 1800,
  seed = 4,
  file = '../results/ModelA_rtdelta085',
  #sample_file = '../results/ModelArtchaindata',
  control = list(adapt_delta = 0.85, max_treedepth = 14)
  
)

sessionInfo()

summary(ModelA)

expose_functions(ModelA, vectorize = TRUE)

ModelA_loo = loo(ModelA, moment_match = TRUE)

ModelA_loo

save(list = 'ModelA_loo', file ='../results/ModelA_loo_delta085).RData')

#model$loo <- loo(model, reloo= TRUE)

