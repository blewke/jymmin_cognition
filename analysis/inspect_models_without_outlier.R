#### inspect the models that were fitted without the outlier,
#### the point that loo says is problematic

require(brms)

#load the fitted models
mB_no_outlier = brm(file = '../results/ModelB_no_20210325')
mA_no_outlier = brm(file = '../results/ModelA_nooutlier_20210322')

#load the already comluted loo objects
load('../results/ModelB_no_basic_loo.RData')
load('../results/ModelA_no_basic_loo.RData')


#compare loo
loo_compare(basic_loo_A,basic_loo_B)

#look at parameter estimates for model B
summary(mB_no_outlier)

