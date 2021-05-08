#### examine the parameter estimates of the fitted models
require(brms)

###### model A #####--------------------------

#load model
mA = brm(file = '../results/ModelA_20210322')

## get a summary
summary(mA)


#select all parameters that were not just technical fitting paramters:
parsA = parnames(mA)[!startsWith(parnames(mA), 'z_') & !startsWith(parnames(mA), 'L_')  & !startsWith(parnames(mA), 'Intercept')]
pars = parsA[!startsWith(parsA, 'prior')& !startsWith(parsA, 'lp_') ]

# get all the model parameters and the estimates
post_sumAall = posterior_summary(mA, pars = pars, fixed = T)




###### model B #####-------------------------

#load the model
mB = brm(file = '../results/ModelB_20210326')

# get a summary
summary(mB)

#select all parameters that were not just technical fitting parameters:
parsB = parnames(mB)[!startsWith(parnames(mB), 'z_') & !startsWith(parnames(mB), 'L_')  & !startsWith(parnames(mB), 'Intercept')]
pars = parsB[!startsWith(parsB, 'prior')& !startsWith(parsB, 'lp_') ]



# get all the model parameters and the estimates
post_sumBall = posterior_summary(mB, pars = pars, fixed = TRUE)

#look at jymmin parameters

jym_pars = c('b_nCorrect_JymminYes',
             'b_nCorrect_JymminYes:nTrialScaled',
             'b_ResponseTime_JymminYes',
             'b_ResponseTime_JymminYes:nTrialScaled')

post_sumBjym = posterior_summary(mB,jym_pars, fixed = TRUE)
print(post_sumBjym)


#now get all the individual samples from the posterior for the jymmin parameters
psB = posterior_samples(mB,pars = jym_pars)
names(psB) <- sub(':', '', names(psB))

## see what percentage of samples is above zero for the jymmin parameters



#intercept for response time, alpha_RT,Jymmin
RTJlarger0 = sum(psB$b_ResponseTime_JymminYes > 0)/length(psB$b_ResponseTime_JymminYes)

#intercept for number of correct responses, alpha_Acc,Jymmin
nCJlarger0 = sum(psB$b_nCorrect_JymminYes > 0)/length(psB$b_nCorrect_JymminYes)

#learning rate for response time, beta_RT,Jymmin
RTJslopelarger0 = sum(psB$b_ResponseTime_JymminYesnTrialScaled > 0)/nrow(psB)

#learning rate for number of correct responses, beta_Acc,Jymmin
nCJslopelarger0 = sum(psB$b_nCorrect_JymminYesnTrialScaled > 0)/nrow(psB)




###### model C #####-------------------------

mC = brm(file = '../results/modelB_noslope_20210330')

# get a summary
summary(mC)

#select all parameters that were not just technical fitting paramters:
parsC = parnames(mC)[!startsWith(parnames(mC), 'z_') & !startsWith(parnames(mC), 'L_')  & !startsWith(parnames(mC), 'Intercept')]
pars = parsC[!startsWith(parsC, 'prior')& !startsWith(parsC, 'lp_') ]

# get all the model parameters and the estimates
post_sumCall = posterior_summary(mC, pars = pars, fixed = TRUE)

#look at jymmin parameters

jym_parsC = c('b_nCorrect_JymminYes',
             'b_ResponseTime_JymminYes')

post_sumCjym = posterior_summary(mC,jym_parsC, fixed = TRUE)
print(post_sumCjym)


#now get all the individual samples from the posterior for the jymmin parameters
psC = posterior_samples(mC,pars = jym_parsC)
names(psC) <- sub(':', '', names(psC))

## see what percentage of samples is above zero for the jymmin parameters


#intercept for response time, alpha_RT,Jymmin
RTJlarger0_C = sum(psC$b_ResponseTime_JymminYes > 0)/length(psC$b_ResponseTime_JymminYes)

#intercept for number of correct responses, alpha_Acc,Jymmin
nCJlarger0_C = sum(psC$b_nCorrect_JymminYes > 0)/length(psC$b_nCorrect_JymminYes)




###### model B_sim #####-------------------------

#load model
mBsim = brm(file = '../results/ModelB_sim_20210330')

## get a summary
summary(mBsim)


#select all parameters that were not just technical fitting paramters:
parsBsim = parnames(mBsim)[!startsWith(parnames(mBsim), 'z_') & !startsWith(parnames(mBsim), 'L_')  & !startsWith(parnames(mBsim), 'Intercept')]
pars = parsBsim[!startsWith(parsBsim, 'prior')& !startsWith(parsBsim, 'lp_') ]

# get all the model parameters and the estimates
post_sumBsimall = posterior_summary(mBsim, pars = pars, fixed = T)



