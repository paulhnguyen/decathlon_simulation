// idea: run cubic model on individual data. use mean and variances of the individual beta and mu parameters as the prior for the decathlon parameters. procedure: run simple_cubic.stan on individual data. extract moments, and put into vectors in R. then plug into the simple_moment_matched_cubic.stan. only do this for the slope and intercept coefficients, not the errors (can double check this with sameer.)

data {
  int<lower=0> N_decathlon;       // Number of decathlon athletes
  int<lower=0> N_pred;            // Number of ages to predict for
  int<lower=1> A_decathlon;       // Number of unique decathlon athletes
  int<lower=0, upper=A_decathlon> athlete_decathlon[N_decathlon];  // Athlete ID for decathlon
  vector[N_decathlon] Y_decathlon;  // Decathlon performance data = Event scores
  vector[N_decathlon] age_decathlon; // Ages of decathlon athletes
  vector[N_pred] age_pred;         // New age values for prediction
  vector[N_pred] is_new_athlete; // check if new athlete
  int<lower=0> athlete_pred[N_pred];  // Athlete IDs for predictions
  
  // moments from the individual data
  vector[3] beta_means;
  // vector[3] beta_sds;
  real mu_mean;
  // real mu_sd;
}

parameters {
  vector[A_decathlon] alpha_decathlon;  // Random intercepts for decathletes
  real beta1;                   // Linear age effect
  real beta2;                   // Quadratic age effect
  real beta3;                   // Cubic age effect
  real mu_alpha;                // Mean intercept across all athletes
  real<lower=0> sigma_alpha_decathlon;     // Standard deviation of athlete intercepts
  real<lower=0> sigma_decathlon; // Residual standard deviation for decathlon events
}

model {
  // Priors
  beta1 ~ normal(beta_means[1], 5);
  beta2 ~ normal(beta_means[2], 2);
  beta3 ~ normal(beta_means[3], 1);
  mu_alpha ~ normal(mu_mean, 10);
  sigma_alpha_decathlon ~ inv_gamma(2, 1);
  sigma_decathlon ~ inv_gamma(2, 1);

  // Hierarchical prior for athlete-specific intercepts
  alpha_decathlon ~ normal(mu_alpha, sigma_alpha_decathlon);


  // Likelihood for decathlon events

  Y_decathlon~ normal(
    alpha_decathlon[athlete_decathlon] 
    + beta1 * age_decathlon 
    + beta2 * square(age_decathlon) 
    + beta3 * pow(age_decathlon, 3), 
    sigma_decathlon
  );
  

}

generated quantities {
  vector[N_pred] Y_pred;        // Predictions for all cases

  for (n in 1:N_pred) {
    real alpha_effect;
    
    if (is_new_athlete[n] == 1) {
      // New athlete -> Draw from normal prior
      alpha_effect = normal_rng(mu_alpha, sigma_alpha_decathlon);
    } else {
      // Existing athlete -> Use learned effect
      alpha_effect = alpha_decathlon[athlete_pred[n]];
    }
    
    // Generate prediction
    Y_pred[n] = normal_rng(
      alpha_effect 
      + beta1 * age_pred[n] 
      + beta2 * square(age_pred[n]) 
      + beta3 * pow(age_pred[n], 3), 
      sigma_decathlon
    );
  }
}



