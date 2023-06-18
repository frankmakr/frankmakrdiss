/**
 * Model-based clustering with dissimilarities: A Bayesian approach
 * (Oh & Raftery, 2007)
 */

functions {
  matrix calc_mds_config(int N, int D, matrix Y) {
    real inv_N = inv(N);
    matrix[N, N] Z = identity_matrix(N) - inv_N;
    matrix[N, N] B = -0.5 * Z * Y.^2 * Z;
    vector[D] eigenval_B_sort = reverse(eigenvalues_sym(B))[1:D];
    matrix[N, N] eigenvec_B = eigenvectors_sym(B);
    matrix[N, D] eigenvec_B_sort;
    matrix[N, D] mds_config;
    for (n in 1:N) {
      eigenvec_B_sort[n] = reverse(eigenvec_B[n])[1:D];
    }
    mds_config = diag_post_multiply(eigenvec_B_sort, sqrt(eigenval_B_sort));
    return mds_config;
  }

  matrix calc_cov_mat(int N, int D, matrix X) {
    real inv_N = inv(N);
    row_vector[D] col_means_X;
    matrix[N, D] X_c;
    matrix[D, D] cov_mat_X;
    for (d in 1:D) {
      col_means_X[d] = mean(X[, d]);
    }
    X_c = X - rep_matrix(col_means_X, N);
    cov_mat_X = inv_N * crossprod(X_c);
    return cov_mat_X;
  }

  matrix rotate_procrustes(int N, int D, matrix X, matrix Y) {
    real inv_N = inv(N);
    vector[N] ones_vec = ones_vector(N);
    matrix[N, N] Z = identity_matrix(N) - inv_N;
    matrix[D, D] X_Z_Y = X' * Z * Y;
    matrix[D, D] T = svd_V(X_Z_Y) * svd_U(X_Z_Y)';
    real scale_factor = trace(X_Z_Y * T)
                       * inv(trace(quad_form(Z, Y)));
    vector[D] translation_vector = inv_N * (X - scale_factor * Y * T)'
                                     * ones_vec;
    matrix[N, D] Y_hat = scale_factor * Y * T
                           + ones_vec * translation_vector';
    return Y_hat;
  }

  vector calc_dp_probs(int C, vector stick_pieces) {
    vector[C - 1] cumprod1m_stick_pieces
                    = exp(cumulative_sum(log1m(stick_pieces)));
    vector[C] dp_probs;
    dp_probs[1] = stick_pieces[1];
    dp_probs[2:(C - 1)]
      = stick_pieces[2:(C - 1)] .* cumprod1m_stick_pieces[1:(C - 2)];
    dp_probs[C] = cumprod1m_stick_pieces[C - 1];
    return dp_probs;
  }

  real partial_sum(array[] real Y_slice,
                   int start, int end,
                   array[] row_vector mds_est,
                   vector Delta_psi,
                   real U, int N_choose_2, int N) {
    vector[N_choose_2] Delta_lower_tri; // Estimated distances
    int index_mds_delta = 1;
    for (j in 1:(N - 1)) {
      for (i in (j + 1):N) {
        Delta_lower_tri[index_mds_delta] = distance(mds_est[i], mds_est[j]);
        index_mds_delta += 1;
      }
    }
    vector[N_choose_2] inv_Delta_psi = inv(Delta_psi);
    array[2] vector[N_choose_2] Delta_shape_inv_gamma
      = { inv_Delta_psi + 2, Delta_lower_tri .* (inv_Delta_psi + 1) };
    return inv_gamma_lpdf(Y_slice |
                Delta_shape_inv_gamma[1][start:end],
                Delta_shape_inv_gamma[2][start:end])
              - inv_gamma_lcdf(U |
                Delta_shape_inv_gamma[1][start:end],
                Delta_shape_inv_gamma[2][start:end]);
  }

  vector trunc_inv_gamma_rng(vector mu, vector psi, real U) {
    int N = num_elements(mu);
    vector[N] inv_psi = inv(psi);
    vector[N] a = inv_psi + 2;
    vector[N] b = mu .* (inv_psi + 1);
    vector[N] inv_a1 = inv(a + 1);
    vector[N] mode = b .* inv_a1;
    array[N] real dens_mode;
    vector[N] y;
    real u;
    real v;
    real dens_u;
    int index = 1;
    for (n in 1:N) {
      dens_mode[n]= exp(inv_gamma_lpdf(mode[n] | a[n], b[n]) 
                      - inv_gamma_lcdf(U | a[n], b[n]));
    }
    while (index <= N) {
      u = uniform_rng(0, U);
      v = uniform_rng(0, dens_mode[index]);
      dens_u = exp(inv_gamma_lpdf(u | a[index], b[index])
                 - inv_gamma_lcdf(U | a[index], b[index]));
      if (dens_u < v) continue;
      y[index] = u;
      index += 1;
    }
    return y;
  }
}

data {
  int<lower = 1> N;                     // Number of data points
  int<lower = 1> D;                     // Number of mds dimensions
  int<lower = 1> C;                     // Maximum number of clusters
  int<lower = 0> U;                     // Maximum scaled E-Distance
  matrix<lower = 0, upper = U>[N, N] Y; // Observed E-distance matrix
  int<lower = 1> n_cores;               // Number of cores
}

transformed data {
  // Multidimensional scaling
  // Identifiability indices
  int n_mds_zero = choose(D, 2) + D;
  int n_mds_unconstrained = 2 * N - n_mds_zero - D;
  int N_choose_2 = choose(N, 2);

  // Unique observed distances
  array[N_choose_2] real Y_lower_tri;
  {
    int index_Y_lower_tri = 1;
    for (j in 1:(N - 1)) {
      for (i in (j + 1):N) {
          Y_lower_tri[index_Y_lower_tri] = Y[i, j];
          index_Y_lower_tri += 1;
      }
    }
  }

  // Classical multidimensional scaling (Torgerson, 1952)
  matrix[N, D] mds_obs_mat = calc_mds_config(N, D, Y); 
  cov_matrix[D] mds_obs_cov_mat = calc_cov_mat(N, D, mds_obs_mat);
  cholesky_factor_cov[D] mds_obs_L = cholesky_decompose(mds_obs_cov_mat);
  array[N] row_vector[D] mds_obs_array;
  {
    for (n in 1:N) {
      mds_obs_array[n] = mds_obs_mat[n];
    }
  }

  // Dirichlet process
  // Dirichlet is uniform over the simplex when concentration parameter is 1
  real dp_alpha = 1;

  // Determine grainsize for threading
  int grainsize = N_choose_2 %/% n_cores;
}

parameters {
  // Multidimensional scaling
  // Constraining the first D * (D + 1) / 2 points to Zero and
  // the next D points positive
  array[n_mds_zero] real mds_est_1;
  array[D] real<lower = 0> mds_est_2;
  array[n_mds_unconstrained] real mds_est_3;
  vector<lower = 0>[N_choose_2] Delta_psi;   // True distance noise parameter

  // Dirichlet process
  vector<lower = 0, upper = 1>[C - 1] dp_lambda_raw; // Mixing proportions
  // Components with ordered constraint for identifiability
  ordered[C] dp_mu_raw_1;
  vector[C] dp_mu_raw_2;
  real<lower = 1> dp_nu;
  vector<lower = 0>[D] dp_sigma;
  cholesky_factor_corr[D] dp_L;                      // Components correlations
}

transformed parameters {
  // Mulidimensional scaling
  array[N] row_vector[D] mds_est;     // Estimated MDS configuration
  {
    array[N, D] real mds_est_array;
    int index_mds_zeros = 1;
    int index_mds_reals = 1;
    matrix[N, D] mds_est_mat;
    for (d in 1:D) {
      mds_est_array[, d] = append_array(segment(mds_est_1, index_mds_zeros, d),
                             append_array(rep_array(mds_est_2[d], 1),
                               segment(mds_est_3, index_mds_reals, N - d - 1)));
      index_mds_zeros += d;
      index_mds_reals += N - d - 1;
    }
    mds_est_mat = rotate_procrustes(N, D, mds_obs_mat, to_matrix(mds_est_array));
    for (n in 1:N) {
      mds_est[n] = mds_est_mat[n];
    }
  }

  // Dirichlet process
  vector[C] dp_lambda = calc_dp_probs(C, dp_lambda_raw);
  array[C] row_vector[D] dp_mu;
  {
    for (c in 1:C) {
      dp_mu[c] = [ dp_mu_raw_1[c], dp_mu_raw_2[c] ];
    }
  }
  matrix[D, D] dp_sigma_L = diag_pre_multiply(dp_sigma, dp_L);
}

model {
  // Multidimensional scaling
  // Population model
  mds_est_1 ~ normal(0, 0.1);
  mds_est_2 ~ normal(0, 2);
  mds_est_3 ~ normal(0, 2);
  Delta_psi ~ inv_gamma(4, 1);
  
  // Observational model
  // Scale has the upper bound of 10
  // Mean and scale parameterization for gamma distribution
  target += reduce_sum_static(partial_sum, Y_lower_tri,
                       grainsize,
                       mds_est, Delta_psi, U, N_choose_2, N);

  // Dirichlet process
  // Population model
  dp_lambda_raw ~ beta(1, dp_alpha); // Marginal beta distribution
  dp_mu_raw_1 ~ normal(0, 3);
  dp_mu_raw_2 ~ normal(0, 3);
  dp_nu ~ gamma(2, 0.1);
  dp_sigma ~ normal(0, 1);
  dp_L ~ lkj_corr_cholesky(3);       // eta > 1 for smaller correlations
  
  // Observational model
  {
    vector[C] log_dp_lambda = log(dp_lambda);
    for (n in 1:N) {
      vector[C] dp_log_mix = log_dp_lambda;
      for (c in 1:C) {
        dp_log_mix[c] += multi_student_t_cholesky_lupdf(
                           mds_est[n] | dp_nu, dp_mu[c], dp_sigma_L);
      }
      target += log_sum_exp(dp_log_mix);
    }
  }
}

generated quantities {
  matrix[D, D] dp_Sigma = multiply_lower_tri_self_transpose(dp_sigma_L);

  // Posterior retrodictive check
  array[N_choose_2] real Y_lower_tri_rep;
  {
    array[N] int mix_component;
    array[N] row_vector[D] mds_est_rep;
    vector[N_choose_2] Delta_lower_tri_rep;
    int index_Delta_lower_tri_rep = 1;
    for (n in 1:N) {
      mix_component[n] = categorical_rng(dp_lambda);
      mds_est_rep[n] = multi_student_t_cholesky_rng(
                         dp_nu, dp_mu[mix_component[n]], dp_sigma_L)';
    }
    for (j in 1:(N - 1)) {
      for (i in (j + 1):N) {
        Delta_lower_tri_rep[index_Delta_lower_tri_rep] = distance(
          mds_est_rep[i], mds_est_rep[j]);
        index_Delta_lower_tri_rep +=1;
      }
    }
    Y_lower_tri_rep = to_array_1d(trunc_inv_gamma_rng(
                                    Delta_lower_tri_rep, Delta_psi, U));
  }

  // Cluster densities
  array[N, C] real log_cluster_density;
  {
    for (n in 1:N) {
      for (c in 1:C) {
        log_cluster_density[n][c] = multi_student_t_cholesky_lpdf(
                                      mds_est[n] | dp_nu, dp_mu[c], dp_sigma_L);
      }
    }
  }
}
