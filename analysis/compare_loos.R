#compare the models with loo

require(ggplot2)
require(brms)
require(tibble)

###### models A, B and C #####

#load the precomputed loo objects that were computed right after the models were run

load('../results/ModelA_basic_loo.RData') #model A
load('../results/ModelB_basic_loo.RData') #model B
load('../results/ModelB_noslope_basic_loo.RData') #model C, here also called B_noslope


#look at them, they all have one high pareto k diagnostic value
print(basic_loo_A)
print(basic_loo_B)
print(basic_loo_B_noslope)

#we can already compare them, but the approximation is not great
loo_compare(basic_loo_A,basic_loo_B)



#get the pointwise values
pwA = as_tibble(basic_loo_A$pointwise)
pwB = as_tibble(basic_loo_B$pointwise)
pwC = as_tibble(basic_loo_B_noslope$pointwise)


#which point is the problematic one?
which.max(pwA$influence_pareto_k)
#it's #1284

#elpd values without that data point:
sum(pwA$elpd_loo[-1284])
sum(pwB$elpd_loo[-1284])
sum(pwC$elpd_loo[-1284])


#elpd differences:
# A-B
sum(pwA$elpd_loo[-1284]) - sum(pwB$elpd_loo[-1284])
# A-C
sum(pwA$elpd_loo[-1284]) - sum(pwC$elpd_loo[-1284])
# B-C
sum(pwB$elpd_loo[-1284]) - sum(pwC$elpd_loo[-1284])


# standard error of the pointwise differences
# as defined in equation (23) in Vehtari, Gelman and Gabry 2017

# A and B
pw_diff_elpdAB = pwA$elpd_loo-pwB$elpd_loo
sqrt(var(pw_diff_elpdAB[-1284])*nrow(pwB[-1284,]))

# A and C 
pw_diff_elpdAC = pwA$elpd_loo-pwC$elpd_loo
sqrt(var(pw_diff_elpdAC[-1284])*nrow(pwA[-1284,]))

# B and C
pw_diff_elpdBC = pwB$elpd_loo-pwC$elpd_loo
sqrt(var(pw_diff_elpdBC[-1284])*nrow(pwB[-1284,]))


# nice little plot to see the pareto-k values and the respnse times
# we need to load a fitted model to have access to the data

mB = brm(file = '../results/ModelB_20210326')

#now plot
ggplot(data = cbind(mB$data,pwB))+
  geom_point(alpha = 0.5, aes(x= influence_pareto_k, y = log(ResponseTime/ClocksInSet)))



###### models on simulated data, models A_sim and B_sim #####


#the loo was already computed and saved in the model directly after model fitting
#load the models

mBsim = brm(file = '../results/ModelB_sim_20210330')
mAsim = brm(file = '../results/ModelA_sim_20210330')

## get all the values we need:

#elpd values
loo(mAsim)$elpd
loo(mBsim)$elpd

#comparison
loo_compare(mAsim, mBsim)

#get out the values we need
lcsim = loo_compare(mAsim, mBsim)
#elpd difference
-lcsim[2,'elpd_diff']
# standard error of the elpd difference
lcsim[2,'se_diff']



