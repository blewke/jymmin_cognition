require(brms)
#require(tibble)
#require(tidyverse)

#brms families
source("../analysis/data_preprocessing.r")
source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')
source('../helper_functions/stanvars bb ssln.R')

#stanvars_ssln <- stanvar(scode = stan_funs_ssln, block = "functions")
stanvars_bb_ssln <- stanvar(scode = stan_funs_bb_ssln, block = "functions")
#stanvars_bb <- stanvar(scode = stan_funs, block = "functions")





#load data
#source("/Users/brittalewke/Documents/Canada Data and scripts/jymmin_cognition/analysis/data_preprocessing.r")

all_data = data_preprocessing("/Users/brittalewke/Documents/Canada Data and scripts/Data_blindx.csv")
all_data = add_nTrialLevel(all_data)
#rdata = random_assignment(all_data)

subject_data = all_data[all_data$SubjectCode %in% 1:24,]


rdata = random_assignment(subject_data)

study_data = rdata[rdata$Period %in% 1:2  & rdata$Stage == 'Exercise',]
study_data = remove_empty_rows(study_data)
study_data$nTrialLevelScale = study_data$nTrialLevel/mean(study_data$nTrialLevel)

study_data_timed = study_data[study_data$LevelType == 0,]

rm(all_data)
rm(subject_data)
rm(study_data)
rm(rdata)



priorModelB = c(prior(student_t(3,2,8), class = Intercept, resp = 'DurationInSeconds'),
                prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'DurationInSeconds'),
                prior(student_t(3,0,3), class = sd, group = 'SubjectCode', resp = 'DurationInSeconds'),
                prior(student_t(3,0,5), class = sigma, resp = 'DurationInSeconds'),
                prior(normal(1,2), class = shift, resp = 'DurationInSeconds'),
                prior(student_t(3, 0.5, 2), class = b, coef = 'nTrialLevelScale', resp = 'DurationInSeconds'),
                prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'DurationInSeconds'),
                prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'DurationInSeconds'),
                
                prior(student_t(3,0,2.5), class = Intercept, resp = 'nCorrect'),
                prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'nCorrect'),
                prior(student_t(3,0,2), class = sd,group = 'SubjectCode', resp = 'nCorrect'),
                prior(gamma(3,0.1), class = phi, resp = 'nCorrect'),
                prior(student_t(3, 0.5, 8), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'),
                prior(student_t(3,0,4), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'nCorrect'),
                prior(student_t(3,0,4), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'nCorrect'),
                
                prior(student_t(3,0,4), class = b, coef = 'JymminYes', resp = 'DurationInSeconds'),
                prior(student_t(3,0,4), class = b, coef = 'JymminYes:nTrialLevelScale', resp = 'DurationInSeconds'),
                
                prior(student_t(3,0,8), class = b, coef = 'JymminYes', resp = 'nCorrect'),
                prior(student_t(3,0,4), class = b, coef = 'JymminYes:nTrialLevelScale', resp = 'nCorrect')
)




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
  iter = 2000,
  seed = 4,
  file = '../results/ModelB',
  sample_file = '../results/ModelBchaindata',
  control = list(adapt_delta = 0.99, max_treedepth = 15)
  
)


