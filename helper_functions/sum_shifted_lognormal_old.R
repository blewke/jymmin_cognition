require(brms)


sum_shifted_lognormal <- custom_family(
  "sum_shifted_lognormal", dpars = c("mu", "sigma",'ndt'),
  links = c('identity', "identity", 'identity'), lb = c(NA, 0, NA),
  type = "real", vars = "vint1[n]"
)


stan_funs <- "
  real sum_shifted_lognormal_lpdf(real y, real mu_underlying, real sigma_underlying, real ndt_underlying,int set_size) {
  
  real var_sum = log((exp(square(sigma_underlying))-1)/set_size + 1 );
  real mu_sum = log(set_size*exp(mu_underlying))+0.5*(square(sigma_underlying) - var_sum);
  
  real new_y = y - ndt_underlying*set_size;
  
    return lognormal_lpdf(new_y | mu_sum, sqrt(var_sum));
  }
  
  "



#   real sum_shifted_lognormal_rng(real mu_underlying, real sigma_underlying, real ndt_underlying, int set_size) {
#   
#   real var_sum = log((exp(square(sigma_underlying))-1)/set_size + 1 );
#   real mu_sum = log(set_size*exp(mu_underlying))+0.5*(square(sigma_underlying) - var_sum);
#   
#   
#   
#   return lognormal_rng(mu_sum, sqrt(var_sum)) + ndt_underlying.*set_size;
#   }
# "



##old
log_lik_sum_shifted_lognormal <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  ndt <- prep$dpars$ndt
  set_size <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  sum_shifted_lognormal_lpdf(y, mu, sigma, ndt, set_size)
}




# 
# log_lik_shifted_lognormal <- function(i, prep) {
#   sigma <- get_dpar(prep, "sigma", i = i)
#   ndt <- get_dpar(prep, "ndt", i = i)
#   args <- list(meanlog = get_dpar(prep, "mu", i), sdlog = sigma, shift = ndt)
#   out <- log_lik_censor("shifted_lnorm", args, i = i, prep = prep)
#   out <- log_lik_truncate(out, pshifted_lnorm, args, i = i, prep = prep)
#   log_lik_weight(out, i = i, prep = prep)
# }
# 




posterior_predict_sum_shifted_lognormal <- function(i, prep, ...) {
  mu_underlying <- prep$dpars$mu[, i]
  sigma_underlying <- prep$dpars$sigma
  ndt_underlying <- prep$dpars$ndt
  set_size <- prep$data$vint1[i]
  
  var_sum = log((exp(sigma_underlying^2)-1)/set_size + 1 );
  mu_sum = log(set_size*exp(mu_underlying))+0.5*(sigma_underlying^2 - var_sum);
  ndt_sum = ndt_underlying*set_size
  
  #sum_shifted_lognormal_rng(mu, sigma, ndt, set_size)
  rshifted_lnorm(1,mu_sum, sqrt(var_sum), ndt_sum)

}


# posterior_predict_sum_shifted_lognormal <- function(i, prep, ntrys = 5, ...) {
#   rcontinuous(
#     n = prep$nsamples, dist = "shifted_lnorm",
#     meanlog = get_dpar(prep, "mu", i = i),
#     sdlog = get_dpar(prep, "sigma", i = i),
#     set_size <- prep$data$vint1[i],
#     shift = get_dpar(prep, "ndt", i = i)*set_size,
#     lb = prep$data$lb[i], ub = prep$data$ub[i],
#     ntrys = ntrys
#   )
# }


posterior_predict_shifted_lognormal <- function(i, prep, ntrys = 5, ...) {
  rcontinuous(
    n = prep$nsamples, dist = "shifted_lnorm",
    meanlog = get_dpar(prep, "mu", i = i),
    #meanlog = 
    sdlog = get_dpar(prep, "sigma", i = i),
    shift = get_dpar(prep, "ndt", i = i),
    lb = prep$data$lb[i], ub = prep$data$ub[i],
    ntrys = ntrys
  )
}
