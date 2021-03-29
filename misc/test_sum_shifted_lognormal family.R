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
  real new_y = max([0.0, y - exp(ndt_underlying)*set_size]);
  
  return lognormal_lpdf(new_y | mu_sum, sqrt(var_sum));
  }
  
  real sum_shifted_lognormal_rng(real mu_underlying, real sigma_underlying, real ndt_underlying,int set_size) {
  real var_sum = log((exp(square(sigma_underlying))-1)/set_size + 1 );
  real mu_sum = log(set_size*exp(mu_underlying))+0.5*(square(sigma_underlying) - var_sum);
  real ndt_sum = exp(ndt_underlying)*set_size;
  
  return lognormal_rng(mu_sum, sqrt(var_sum)) + ndt_sum;
  
  }
  
  "




log_lik_sum_shifted_lognormal <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma[, i]
  #ndt <- prep$dpars$ndt
  shift <- prep$dpars$shift[, i] ### added i
  set_size <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  sum_shifted_lognormal_lpdf(y, mu, sigma, shift, set_size)
  #shifted_lognormal_lpdf(y, mu, sigma, shift, set_size)
  #sum_shifted_lognormal_lpdf(y, mu, sigma, shift, set_size)
}




posterior_predict_sum_shifted_lognormal <- function(i, prep, ...) {
  mu_underlying <- prep$dpars$mu[, i]
  
  sigma_underlying <- prep$dpars$sigma
  #sigma_underlying <- prep$dpars$sigma[, i]
  
  #ndt_underlying <- prep$dpars$ndt
  #ndt_underlying <- prep$dpars$shift[i]### added i
  ndt_underlying <- prep$dpars$shift[, i]
  set_size <- prep$data$vint1[i]
  
  
  sum_shifted_lognormal_rng(mu_underlying, sigma_underlying, ndt_underlying, set_size)
}




######this is correct now maybe??? !!!!!!!!!
posterior_epred_sum_shifted_lognormal <- function(prep) {
  mu_underlying <- prep$dpars$mu
  set_size <- prep$data$vint1
  sigma_underlying <- prep$dpars$sigma
  ndt_underlying <- prep$dpars$shift
  set_size <- matrix(set_size, nrow = nrow(mu_underlying), ncol = ncol(mu_underlying), byrow = TRUE)
  var_sum <- log((exp(sigma_underlying^2)-1)/set_size + 1 );
  mu_sum <- log(set_size*exp(mu_underlying))+0.5*(sigma_underlying^2 - var_sum);
  ndt_sum <- exp(ndt_underlying)*set_size
  
  exp(mu_sum + var_sum / 2) + ndt_sum
}

