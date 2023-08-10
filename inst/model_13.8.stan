/**
 * Beta binomial model for comparing the number of OVL's > 0.5
 */
data {
  int<lower = 1> K;          // Number of data points
  int<lower = 1> N;          // Number of OVL's
  array[K] int<lower = 0> y; // Counts of OVL's > 0.5
}

parameters {
  vector<lower = 0, upper = 1>[K] theta; // Probability for OVL's > 0.5
  row_vector<lower = 0>[2] shape_beta;   // Population Beta distribution
}

model {
  // Superpopulation model
  shape_beta ~ gamma(2, 0.5);

  // Population model
  theta ~ beta(shape_beta[1], shape_beta[2]);

  // Observational model
  y ~ binomial(N, theta);
}

generated quantities {
  // Posterior retrodictive check
  array[K] int y_rep = binomial_rng(N, theta);
}

