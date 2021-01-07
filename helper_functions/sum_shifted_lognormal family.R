require(brms)

sum_shifted_lognormal <- custom_family(
  #"sum_shifted_lognormal", dpars = c("mu", "sigma",'ndt'),
  "sum_shifted_lognormal", dpars = c("mu", "sigma",'shift'),
  links = c('identity', "identity", 'identity'), lb = c(NA, 0, NA),
  type = "real", vars = "vint1[n]"
)


#stan_funs <- "
stan_funs_ssln <- "
  real sum_shifted_lognormal_lpdf(real y, real mu_underlying, real sigma_underlying, real ndt_underlying,int set_size) {
  
  real var_sum = log((exp(square(sigma_underlying))-1)/set_size + 1 );
  real mu_sum = log(set_size*exp(mu_underlying))+0.5*(square(sigma_underlying) - var_sum);
  real new_y = y - ndt_underlying*set_size;
  
  return lognormal_lpdf(new_y | mu_sum, sqrt(var_sum));
  }
  "


log_lik_sum_shifted_lognormal <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  #ndt <- prep$dpars$ndt
  shift <- prep$dpars$shift
  set_size <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  sum_shifted_lognormal_lpdf(y, mu, sigma, shift, set_size)
  #shifted_lognormal_lpdf(y, mu, sigma, shift, set_size)
  #sum_shifted_lognormal_lpdf(y, mu, sigma, shift, set_size)
}


 
posterior_predict_sum_shifted_lognormal <- function(i, prep, ...) {
  mu_underlying <- prep$dpars$mu[, i]
  sigma_underlying <- prep$dpars$sigma
  #ndt_underlying <- prep$dpars$ndt
  ndt_underlying <- prep$dpars$shift
  set_size <- prep$data$vint1[i]
  
  var_sum = log((exp(sigma_underlying^2)-1)/set_size + 1 );
  mu_sum = log(set_size*exp(mu_underlying))+0.5*(sigma_underlying^2 - var_sum);
  ndt_sum = ndt_underlying*set_size
  rshifted_lnorm(1,mu_sum, sqrt(var_sum), ndt_sum)
}


posterior_epred_sum_shifted_lognormal <- function(prep) {
  mu <- prep$dpars$mu
  set_size <- prep$data$vint1
  set_size <- matrix(set_size, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * set_size
}

