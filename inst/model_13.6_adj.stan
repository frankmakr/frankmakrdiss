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
 * Here these characteristics are represented
 * by formulating the response distribution as a
 * finite mixture of ordered categories and a Beta distribution.
 *
 * The code is optimised by
 * (a) precalculating the counts of the categories
 * to eliminate the if-conditions for the finite mixture and
 * (b) by reparameterizing the group effects
 * to shift the data's correlation with the parameters
 * to the prior population parameters.
 *
 * Parameterization of the Beta distribution according to Betancourt:
 * https://betanalpha.github.io/assets/case_studies/probability_densities.html#242_the_location-dispersion_parameterization
 *
 * Since sigma^2 <= mu * (1 - mu),
 * a more useful parameterization instead of mean and variance
 * is the mean and the ratio between the variance and the maximum variance.
 * mu = a / (a + b)
 * phi = 1 / (a + b + 1)
 * From this follows
 * a = mu * (1 / phi - 1)
 * b = (mu - 1) * (1 - 1 / phi)
 * and the variance is
 * sigma^2 = mu * (1 - mu) * phi
 *
 * The total mean and total variance of the mixture with proportion lambda is:
 * mu_t = sum^n_i=1 lambda_i * mu_i
 * sigma^2_t = sum^n_i=1 lambda_i * (sigma^2_i + mu_i^2 - mu_t^2)
 */

functions{
  /**
   * Return the counts of the response categories in the observations.
   * @param y Observations.
   * @param cat Categories to count.
   * @return Counts of the categories.
   */ 
  int[] num_cat(real[] y, real[] cat) {
    int N_cat = size(cat);
    int sum_cat[N_cat] = rep_array(0, N_cat);
    for (n in 1:size(y)) {
      for (c in 1:N_cat) {
        sum_cat[c] += (y[n] == cat[c]);
      }
    }
    return sum_cat;
  }

  /**
   * Return the induced Dirichlet density according to Michael Betancourt:
   * https://betanalpha.github.io/assets/case_studies/ordinal_regression.html#22_Surgical_Cut
   * This is relevant for formulating a principled prior for ordered categories.
   */
  real induced_dirichlet_lpdf(vector cut_points, vector alpha, real anchor) {
    int C = num_elements(cut_points) + 1;
    vector[C - 1] sigma = inv_logit(anchor - cut_points);
    vector[C] probs;
    matrix[C, C] J = rep_matrix(0, C, C);
    // Induced ordinal probabilities
    probs[1] = 1 - sigma[1];
    for (c in 2:(C - 1)) {
      probs[c] = sigma[c - 1] - sigma[c];
    }
    probs[C] = sigma[C - 1];
    // Baseline column of Jacobian
    for (c in 1:C) {
      J[c, 1] = 1;
    }
    // Diagonal entries of Jacobian
    for (c in 2:C) {
      real rho = sigma[c - 1] * (1 - sigma[c - 1]);
      J[c, c] = -rho;
      J[c - 1, c] = rho;
    }
    return dirichlet_lpdf(probs | alpha) + log_determinant(J);
  }
}

data {
  int<lower = 0> N; // Number of observations

  int<lower = 0> J; // Number of group levels in data
  int<lower = 0> K;

  int<lower = 0> P; // Number of observational predictors
  int<lower = 0> U; // Number of group predictors
  int<lower = 0> V;

  int<lower = 1, upper = J> jj[N]; // Group id's
  int<lower = 1, upper = K> kk[N];

  matrix[N, P] x; // Observational predictors
  matrix[J, U] u; // Group predictors
  matrix[K, V] v;

  real<lower = 0, upper = 1> y[N]; // Observed responses
}

transformed data {
  // Calculate the counts of the categories
  int C = 11;
  real cat[C] = { 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 };
  int N_cat[C] = num_cat(y, cat);
  int N_sumcat = sum(N_cat);
  int N_con = N - N_sumcat;

  int jj_cat[N_sumcat];      // Array for categorical group id's
  int kk_cat[N_sumcat];
  int jj_con[N_con];         // Array for continuous group id's
  int kk_con[N_con];
  matrix[N_sumcat, P] x_cat; // Matrix for categorical predictors
  matrix[N_con, P] x_con;    // Matrix for continuous predictors
  int y_cat[N_sumcat];       // Array for categorical responses
  real y_con[N_con];         // Array for continuous responses

  int index_cat = 0;         // Index for loop
  int index_con = 0;

  // Broadcast the arrays for the categorical and continuous responses
  for (n in 1:N) {
    for (c in 1:C) {
      if (y[n] == cat[c]) {
        index_cat += 1;
        jj_cat[index_cat] = jj[n];
        kk_cat[index_cat] = kk[n];
        x_cat[index_cat] = x[n];
        y_cat[index_cat] = c;
      }
    }
    if (y[n] == cat[1]) continue;
    if (y[n] == cat[2]) continue;
    if (y[n] == cat[3]) continue;
    if (y[n] == cat[4]) continue;
    if (y[n] == cat[5]) continue;
    if (y[n] == cat[6]) continue;
    if (y[n] == cat[7]) continue;
    if (y[n] == cat[8]) continue;
    if (y[n] == cat[9]) continue;
    if (y[n] == cat[10]) continue;
    if (y[n] == cat[11]) continue;
    index_con += 1;
    jj_con[index_con] = jj[n];
    kk_con[index_con] = kk[n];
    x_con[index_con] = x[n];
    y_con[index_con] = y[n];
  }
}

parameters {
  // Intercepts
  real mu_obs_logit;
  real mu_j_logit;
  real mu_k_logit;

  // Variance components
  real<lower = 0> sigma_gamma; // Superpopulation standard deviation
  real<lower = 0> sigma_delta;
  vector<multiplier = sigma_gamma>[J] gamma_raw_logit;
  vector<multiplier = sigma_delta>[K] delta_raw_logit;

  // Adjusting covariates
  vector[P] beta_ord_logit; // Regression parameters
  vector[P] beta_con_logit;
  vector[U] zeta_logit;
  vector[V] eta_logit;

  // Finite mixture
  real<lower = 0, upper = 1> lambda; // Mixing proportion

  // Ordered categories
  ordered[C - 1] cut_points; // Cut points

  // Beta distribution
  real<lower = 0, upper = 1> phi; // Ratio var / max(var)
}

transformed parameters {
  // Latent ordinal effects
  vector[N_sumcat] theta_logit = mu_obs_logit + x_cat * beta_ord_logit
    + gamma_raw_logit[jj_cat] + delta_raw_logit[kk_cat];

  // Beta distribution with logit link
  vector[N_con] mu_beta_logit = inv_logit(mu_obs_logit + x_con * beta_con_logit
    + gamma_raw_logit[jj_con] + delta_raw_logit[kk_con]);
  real inv_phi = inv(phi);
  vector[N_con] shape_beta[2]
    = { mu_beta_logit * (inv_phi - 1), (mu_beta_logit - 1) * (1 - inv_phi) };
}

model {
  // Population prior model
  { sigma_gamma, sigma_delta } ~ gamma(2, 4);

  // Population model
  mu_obs_logit ~ student_t(7, 0, 0.4);
  { mu_j_logit, mu_k_logit } ~ normal(0, 0.001);

  beta_ord_logit ~ student_t(7, 0, 0.3);
  beta_con_logit ~ student_t(7, 0, 0.3);
  zeta_logit ~ student_t(7, 0, 0.3);
  eta_logit ~ student_t(7, 0, 0.3);

  gamma_raw_logit ~ normal_id_glm(u, mu_j_logit, zeta_logit, sigma_gamma);
  delta_raw_logit ~ normal_id_glm(v, mu_k_logit, eta_logit, sigma_delta);
 
  lambda ~ uniform(0, 1);

  cut_points ~ induced_dirichlet(rep_vector(1, C), 0);

  phi ~ uniform(0, 1);

  // Observational model
  N_sumcat ~ binomial(N, lambda);
  y_cat ~ ordered_logistic(theta_logit, cut_points);
  y_con ~ beta(shape_beta[1], shape_beta[2]);
}

generated quantities {
  // Global intercept
  real mu_global_logit = mu_obs_logit + mu_j_logit + mu_k_logit;

  // Parameters on real scale
  real mu_global = inv_logit(mu_global_logit);
  vector[P] beta_ord = inv_logit(beta_ord_logit);
  vector[P] beta_con = inv_logit(beta_con_logit);
  vector[U] zeta = inv_logit(zeta_logit);
  vector[V] eta = inv_logit(eta_logit);
  vector[J] gamma_raw = inv_logit(gamma_raw_logit);
  vector[K] delta_raw = inv_logit(delta_raw_logit);

  // Group errors from estimated model averaging over the groups
  vector[J] gamma_err = inv_logit(gamma_raw_logit - u * zeta_logit);
  vector[K] delta_err = inv_logit(delta_raw_logit - v * eta_logit);

  // Effects within each group
  vector[J] gamma = inv_logit(rep_vector(mu_obs_logit, J) + gamma_raw_logit);
  vector[J] delta = inv_logit(rep_vector(mu_obs_logit, K) + delta_raw_logit);

  // Group effects with average value of other group predictors
  vector[J] gamma_avg = inv_logit(
    rep_vector(mu_obs_logit, J) + gamma_raw_logit + mean(delta_raw_logit));
  vector[J] delta_avg = inv_logit(
    rep_vector(mu_obs_logit, K) + delta_raw_logit + mean(gamma_raw_logit));

  // Group effects with average value of all other predictors
  vector[J] gamma_avg2 = inv_logit(rep_vector(mu_obs_logit, J)
    + lambda * mean(x_cat * beta_ord_logit)
    + (1 - lambda) * mean(x_con * beta_con_logit)
    + gamma_raw_logit + mean(delta_raw_logit));
  vector[K] delta_avg2 = inv_logit(rep_vector(mu_obs_logit, K)
    + lambda * mean(x_cat * beta_ord_logit)
    + (1 - lambda) * mean(x_con * beta_con_logit)
    + delta_raw_logit + mean(gamma_raw_logit));

  // Errors for calculating pooling factors
  vector[J] epsilon_gamma = inv_logit(gamma_raw_logit)
    - inv_logit(rep_vector(mu_j_logit, J) + u * zeta_logit);
  vector[K] epsilon_delta = inv_logit(delta_raw_logit)
    - inv_logit(rep_vector(mu_k_logit, K) + v * eta_logit);
  vector[N_sumcat] epsilon_y_cat = (to_vector(y_cat) * 0.1 - 0.1)
    - inv_logit(theta_logit);
  vector[N_con] epsilon_y_con = to_vector(y_con) - inv_logit(mu_beta_logit);
  vector[N] epsilon_y = append_row(epsilon_y_cat, epsilon_y_con);

  // Superpopulation standard deviations
  real<lower = 0> sigma_y_cat;
  real<lower = 0> sigma_y_con;
  real<lower = 0> sigma_y;

  // Finite-population standard deviations
  real<lower = 0> s_gamma = sd(gamma_raw);
  real<lower = 0> s_delta = sd(delta_raw);
  real<lower = 0> s_y_cat;
  real<lower = 0> s_y_con;
  real<lower = 0> s_y;

  // Posterior retrodictive check, population parameters replicated
  real y_rep[N];
  {
    vector[J] gamma_raw_logit_rep
      = to_vector(normal_rng(mu_j_logit + u * zeta_logit, sigma_gamma));
    vector[K] delta_raw_logit_rep 
      = to_vector(normal_rng(mu_k_logit + v * eta_logit, sigma_delta));
    vector[N] theta_logit_rep = mu_obs_logit + x * beta_ord_logit
      + gamma_raw_logit_rep[jj] + delta_raw_logit_rep[kk];
    vector[N] mu_beta_logit_rep = inv_logit(mu_obs_logit + x * beta_con_logit
      + gamma_raw_logit_rep[jj] + delta_raw_logit_rep[kk]);
    vector[N] shape_beta_rep[2]
      = { mu_beta_logit_rep * (inv_phi - 1),
          (mu_beta_logit_rep - 1) * (1 - inv_phi) };

    for (n in 1:N) {
      if (bernoulli_rng(lambda)) {
        y_rep[n] = ordered_logistic_rng(theta_logit_rep[n], cut_points)
                     * 0.1 - 0.1;
      } else {
        y_rep[n] = round(beta_rng(shape_beta_rep[1][n], shape_beta_rep[2][n])
          * 100) * 0.01;
        for (c in 1:C) {
          while (y_rep[n] == cat[c]) {
            y_rep[n] = round(beta_rng(shape_beta_rep[1][n], shape_beta_rep[2][n])
              * 100) * 0.01;
          }
        }
      }
    }
  }

  // Superpopulation and finite_population standard deviations
  {
    vector[2] mix_prob = [ lambda, 1 - lambda ]';
    vector[2] mu_mixcomp = [ mean(inv_logit(theta_logit)),
                             mean(inv_logit(mu_beta_logit)) ]';
    vector[2] sigma2_mixcomp = [ square(pi()) * inv(3) * 0.01,
                                 mu_mixcomp[2] * (1 - mu_mixcomp[2]) * phi ]';
    real mu_mix = sum(mix_prob .* mu_mixcomp);
    real sigma2_mix
      = sum(mix_prob .* (sigma2_mixcomp + square(mu_mixcomp) - square(mu_mix)));
    vector[N_sumcat] e_y_cat = (to_vector(y_cat) * 0.1 - 0.1)
      - inv_logit(theta_logit);
    vector[N_con] e_y_con = to_vector(y_con) - inv_logit(mu_beta_logit);
    sigma_y_cat = sqrt(sigma2_mixcomp[1]);
    sigma_y_con = sqrt(sigma2_mixcomp[2]);
    sigma_y = sqrt(sigma2_mix);
    s_y_cat = sd(e_y_cat);
    s_y_con = sd(e_y_con);
    s_y = sd(append_row(e_y_cat, e_y_con));
  }


  // Counts for checking
  int counts[14];
  {
    for (c in 1:C) {
      counts[c] = N_cat[c];
    }
    counts[12] = N_sumcat;
    counts[13] = N_con;
    counts[14] = N;
  }
}
