###Define Custom Response Distributions with brms, the beta binmial family

#https://cran.r-project.org/web/packages/brms/vignettes/brms_customfamilies.html
#this code was written by Paul Bürkner
#slight modifications by Britta Lewke



require(brms)


#make custom familiy beta binomial for overdispersed data

beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)



stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"


stanvars <- stanvar(scode = stan_funs, block = "functions")



log_lik_beta_binomial2 <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}



#Next, we will define the function necessary for the posterior_predict method:
  
  posterior_predict_beta_binomial2 <- function(i, prep, ...) {
    mu <- prep$dpars$mu[, i]
    phi <- prep$dpars$phi
    trials <- prep$data$vint1[i]
    beta_binomial2_rng(mu, phi, trials)
  }



posterior_epred_beta_binomial2 <- function(prep) {
  mu <- prep$dpars$mu
  trials <- prep$data$vint1
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}


