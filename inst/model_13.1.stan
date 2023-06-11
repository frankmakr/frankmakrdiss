/**
 * An analysis of variance in a multilevel framework:
 * The data is on the context x person level.
 * The finite-population standard deviation
 * represents the variation in the data.
 * The superpopulation standard deviation
 * represents the variation among the modeled probabilty distribution
 * and is relevant for predicting values of groups not represented in the data.
 *
 * The distribution of the responses shows
 * that the full range of the scale from 0 to 100 was used.
 * In addition the distribution is concentrated at specific values:
 * 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, and 1.
 * Here these characteristics are not specifically represented.
 * Instead a normal distribution is used for the responses
 * for comparing the results.
 *
 * The code is optimised by
 * reparameterizing the group effects
 * to shift the data's correlation with the parameters
 * to the prior population parameters.
 */

data {
  int<lower = 1> N;  // Number of observations in data

  int<lower = 1> J;  // Number of group levels in data
  int<lower = 1> K;

  int<lower = 1, upper = J> jj[N];  // Group id's
  int<lower = 1, upper = K> kk[N];

  real<lower = 0, upper = 1> y[N];  // Observed responses
}

parameters {
  // Intercepts
  real mu_obs;
  real mu_j;
  real mu_k;

  // Variance components
  real<lower = 0> sigma_gamma;  // Superpopulation standard deviation
  real<lower = 0> sigma_delta;
  vector<multiplier = sigma_gamma>[J] gamma_raw;
  vector<multiplier = sigma_delta>[K] delta_raw;

  // Normal distribution
  real<lower = 0> sigma_y;  // Superpopulation standard deviation
}

transformed parameters {
  // Mean of Normal distribution
  vector[N] mu_normal = mu_obs + gamma_raw[jj] + delta_raw[kk];
}

model {
  // Population prior model
  { sigma_gamma, sigma_delta } ~ gamma(2, 4);

  // Population model
  mu_obs ~ student_t(7, 0, 0.4);  // stan-dev wiki prior recommendations
  { mu_j, mu_k } ~ normal(0, 0.001);

  gamma_raw ~ normal(mu_j, sigma_gamma);
  delta_raw ~ normal(mu_k, sigma_delta);

  sigma_y ~ gamma(2, 4);  // stan-dev wiki prior recommendations

  // Observational model
  y ~ normal(mu_normal, sigma_y);
}

generated quantities {
  // Global intercept
  real mu_global = mu_obs + mu_j + mu_k;

  // Group errors from estimated model averaging over the groups
  vector[J] gamma_err = gamma_raw;
  vector[K] delta_err = delta_raw;

  // Effects within each group
  vector[J] gamma = rep_vector(mu_obs, J) + gamma_raw;
  vector[K] delta = rep_vector(mu_obs, K) + delta_raw;

  // Group effects with average value of other predictors
  vector[J] gamma_avg = gamma + mean(delta_raw);
  vector[K] delta_avg = delta + mean(gamma_raw);

  // Errors for calculating pooling factors
  vector[J] epsilon_gamma = gamma_raw - rep_vector(mu_j, J);
  vector[K] epsilon_delta = delta_raw - rep_vector(mu_k, K);
  vector[N] epsilon_y = to_vector(y) - mu_normal;

  // Finite-population standard deviations
  real<lower = 0> s_gamma = sd(gamma_raw);
  real<lower = 0> s_delta = sd(delta_raw);
  real<lower = 0> s_y = sd(epsilon_y);

  // Replications for posterior predictive checks (Stan user's guide 26.6):
  real y_rep[N];  // Population parameters replicated 

  {
    vector[J] gamma_raw_rep
      = to_vector(normal_rng(rep_vector(mu_j, J), sigma_gamma));
    vector[K] delta_raw_rep
      = to_vector(normal_rng(rep_vector(mu_k, K), sigma_delta));
    vector[N] mu_normal_rep = mu_obs + gamma_raw_rep[jj] + delta_raw_rep[kk];
    y_rep = normal_rng(mu_normal_rep, sigma_y);
  }
}
