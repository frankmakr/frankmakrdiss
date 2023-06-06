/**
 * Regression model to predict
 * the given number of communities per person.
 * The analyzed predictors are the demographic variables
 * being substantial different from the population proportions:
 * Gender (male = 0, female = 1),
 * age in years (with agegroups in population),
 * student status (fulltime = 0, parttime = 1),
 * knowledge of theories of psychological sense of community (no knowledge = 1).
 * A censored negative binomial distribution
 * was chosen for the observations,
 * parameterized with mean mu and concentration phi.
 * The code is optimised by precounting
 * the censored and uncensored observations.
 */

functions {
/**
 * Return the counts of the censored observations.
 * @param y Observations.
 * @param cens Maximum observed value.
 * @return Counts of the censored observations.
 */
  int count_cens(array[] int y, int cens) {
    int sum_cens = 0;
    for (n in 1:size(y)) {
      sum_cens += (y[n] >= cens);
    }
    return sum_cens;
  }
}

data {
  int<lower = 1> N;          // Number of observations
  int<lower = 1> P;          // Number of columns in the predictor matrix
  matrix[N, P] X;            // Predictor matrix
  array[N] int<lower = 0> y; // Observed counts
  int<lower = max(y)> Max;   // Maximum observed value

  // Proportions in the population
  real<lower = 0, upper = 1> female_probs;
  vector<lower = 0, upper = 1>[8] agegroup_probs;
  real<lower = 0, upper = 1> parttime_probs;
  real<lower = 0, upper = 1> noexpert_probs;
}

transformed data {
  // Building arrays for censored and uncensored observations
  int<lower = 0> N_cens = count_cens(y, Max - 1);
  int N_noncens = N - N_cens;
  array[N_noncens] int<lower = 0, upper = Max - 1> y_noncens;
  array[N_cens] int cens;
  array[N_noncens] int noncens;
  int index_cens = 0;
  int index_noncens = 0;
  for (n in 1:N) {
    if (y[n] >= Max - 1) {
      index_cens += 1;
      cens[index_cens] = n;
    } else {
      index_noncens += 1;
      noncens[index_noncens] = n;
      y_noncens[index_noncens] = y[n];
    }
  }
}

parameters {
  // Regression parameters
  real beta_mu;               // Hierarchical prior location
  real<lower = 0> beta_sigma; // Hierarchical prior scale
  vector[P] beta;

  // Negative binomial distribution
  real<lower = 0> phi; // Concentration parameter
}

transformed parameters{
  // Precalculating the linear predictor
  vector[N] lambda = exp(X * beta);
}

model {
  // Superpopulation model
  beta_mu ~ student_t(7, 0, 1);
  beta_sigma ~ gamma(2, 2);

  // Population model
  beta ~ normal(beta_mu, beta_sigma);
  phi ~ gamma(2, 0.1);

  // Observational model
  y_noncens ~ neg_binomial_2_log_glm(
                X[noncens, 2:5], beta[1], beta[2:5], phi);
  target += neg_binomial_2_lccdf(Max - 1 | lambda[cens], phi);
}

generated quantities {
  // Overdispersion factor relative to poisson distribution
  real psi = inv(phi);

  // Posterior retrodictive check
  array[N] int y_rep = neg_binomial_2_rng(lambda, phi);
  {
    // Censoring of values greater Max
    for (n in 1:N) {
      if (y_rep[n] > Max) {
        y_rep[n] = Max;
      }
    }
  }

  // Prediction of observations from population proportions
  array[N] int y_tilde;
  {
    array[N] int age_pop;
    matrix[N, P] X_tilde = append_col(ones_vector(N), rep_matrix(0, N, P - 1));
    vector[N] X_tilde_beta;
    X_tilde[, 2] = to_vector(bernoulli_rng(rep_array(female_probs, N)));
    for (n in 1:N) {
      age_pop[n] = categorical_rng(agegroup_probs);
      if (age_pop[n] == 1) {
        X_tilde[n, 3] = discrete_range_rng(14, 17);
      } else if (age_pop[n] == 2) {
        X_tilde[n, 3] = discrete_range_rng(18, 24);
      } else if (age_pop[n] == 3) {
        X_tilde[n, 3] = discrete_range_rng(25, 31);
      } else if (age_pop[n] == 4) {
        X_tilde[n, 3] = discrete_range_rng(32, 38);
      } else if (age_pop[n] == 5) {
        X_tilde[n, 3] = discrete_range_rng(39, 45);
      } else if (age_pop[n] == 6) {
        X_tilde[n, 3] = discrete_range_rng(46, 52);
      } else if (age_pop[n] == 7) {
        X_tilde[n, 3] = discrete_range_rng(53, 59);
      } else if (age_pop[n] == 8) {
        X_tilde[n, 3] = discrete_range_rng(60, 80);
      }
    }
    X_tilde[, 4] = to_vector(bernoulli_rng(rep_array(parttime_probs, N)));
    X_tilde[, 5] = to_vector(bernoulli_rng(rep_array(noexpert_probs, N)));
    X_tilde_beta = X_tilde * beta;
    y_tilde = neg_binomial_2_log_rng(X_tilde_beta, phi);
    // Censoring of values greater Max
    for (n in 1:N) {
      if (y_tilde[n] > Max) {
        y_tilde[n] = Max;
      }
    }
  }
}
