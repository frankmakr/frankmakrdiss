/**
 * Parameterization of the Beta distribution according to Michael Betancourt
 * <https://betanalpha.github.io/assets/case_studies/probability_densities.html#24_the_beta_family>
 *
 * Mean and the ratio between the variance and the maximum variance:
 * mu = a / (a + b)
 * phi = 1 / (a + b + 1)
 * From this follows
 * a = mu * (1 / phi - 1)
 * b = (mu - 1) * (1 - 1 / phi)
 * and the variance is
 * sigma^2 = mu * (1 - mu) * phi
 */

data {
  int<lower = 1> K;                // Number of data points
  int<lower = 1> N;                // Number of persons
  array[K] int<lower = 1> y;       // Counts of persons

  int<lower = 1> L;                // Number of expected sample sizes
  array[L] int<lower = 1> N_tilde; // Expected sample sizes
}

parameters {
  vector<lower = 0, upper = 1>[K] theta; // Probability to be choosen
  real<lower = 0, upper = 1> mu;         // Population parameter
  real<lower = 0, upper = 1> phi;        // Population concentration
}

transformed parameters {
  row_vector[2] shape_beta;
  {
  real inv_phi = inv(phi);
  shape_beta = [ mu * (inv_phi - 1),
                 (mu - 1) * (1 - inv_phi) ];
  }
}

model {
  // Superpopulation model
  mu ~ beta(2, 10);
  phi ~ beta(1, 10);

  // Population model
  theta ~ beta(shape_beta[1], shape_beta[2]);

  // Observational model
  y ~ binomial(N, theta);
}

generated quantities {
  // Posterior retrodictive check
  array[K] int y_rep = beta_binomial_rng(
                         rep_array(N, K), shape_beta[1], shape_beta[2]);
  
  // Posterior prediction for expected sample sizes
  array[L, K] int y_tilde;
  for (l in 1:L) {
    y_tilde[l] = beta_binomial_rng(
                  rep_array(N_tilde[l], K), shape_beta[1], shape_beta[2]);
  }
}
