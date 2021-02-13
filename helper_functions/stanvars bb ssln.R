# stan_funs <- "
#   real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
#     return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
#   }
#   int beta_binomial2_rng(real mu, real phi, int T) {
#     return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
#   }
# "
# 
# 
# stan_funs_bb_ssln <- "
#   real sum_shifted_lognormal_lpdf(real y, real mu_underlying, real sigma_underlying, real ndt_underlying,int set_size) {
#   
#   real var_sum = log((exp(square(sigma_underlying))-1)/set_size + 1 );
#   real mu_sum = log(set_size*exp(mu_underlying))+0.5*(square(sigma_underlying) - var_sum);
#   real new_y = y - ndt_underlying*set_size;
#   
#   return lognormal_lpdf(new_y | mu_sum, sqrt(var_sum));
#   }
#   
#     real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
#     return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
#   }
#   int beta_binomial2_rng(real mu, real phi, int T) {
#     return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
#   }
#   
#   "

print('I do not need this file anymore: stanvars bb ssln')