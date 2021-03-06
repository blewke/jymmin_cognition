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



priors_accuracy = c(
  #prior(student_t(3,0,2.5), class = Intercept, resp = 'nCorrect'),
  prior(normal(0,2), class = Intercept),#, resp = 'nCorrect'),
  
  #prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'nCorrect'),
  prior(normal(0,1), class = sd, group = 'Level'),#, resp = 'nCorrect'),
  #prior(student_t(3,0,2), class = sd,group = 'SubjectCode', resp = 'nCorrect'),
  prior(normal(0,1), class = sd, group = 'SubjectCode'),#, resp = 'nCorrect'),
  
  prior(gamma(3,0.1), class = phi),#, resp = 'nCorrect'),
  
  #prior(student_t(3, 0.5, 8), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'),
  #prior(student_t(3, 0.5, 2), class = b, coef = 'nTrialScale', resp = 'nCorrect'),
  prior(normal(1, 1.5), class = b, coef = 'nTrialScale'),#, resp = 'nCorrect'),
  
  #prior(student_t(3,0,1.5), class = sd, coef = 'nTrialScale', group = 'SubjectCode', resp = 'nCorrect'),
  #prior(student_t(3,0,1.5), class = sd, coef = 'nTrialScale', group = 'Level', resp = 'nCorrect')
  prior(normal(0,0.75), class = sd, coef = 'nTrialScale', group = 'SubjectCode'),#, resp = 'nCorrect'),
  prior(normal(0,0.75), class = sd, coef = 'nTrialScale', group = 'Level')#, resp = 'nCorrect')
  
)





priorModelA_rt = priors_accuracy

ModelA =  brm (
  data = study_data_timed,
  formula =
    #bf (DurationInSeconds|vint(ClocksInSet) ~ 1 + nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = sum_shifted_lognormal)#+
    bf (nCorrect|vint(ClocksInSet) ~ 1 + nTrialScale + (1 + nTrialScale|s|SubjectCode) + (1 + nTrialScale|l|Level) , family = beta_binomial2)
  ,
  prior = priorModelA_rt,
  #backend = 'cmdstanr',
  sample_prior = 'yes',
  save_pars = save_pars(all=TRUE),
  stanvars = stanvars_bb_ssln, 
  cores = 6,
  chains = 6,
  warmup = 100,
  iter = 200,
  seed = 4,
  file = '../results/ModelA_Acc95',
  sample_file = '../results/ModelAccchaindata',
  control = list(adapt_delta = 0.95, max_treedepth = 14)
  
)

summary(ModelA)

sessionInfo()

expose_functions(ModelA, vectorize = TRUE)

ModelA_loo = loo(ModelA, moment_match = TRUE)

save(list = 'ModelA_loo', file ='../results/ModelAAcc_loo.RData')

#model$loo <- loo(model, reloo= TRUE)

