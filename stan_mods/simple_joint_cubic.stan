data {
  int<lower=0> N_decathlon;       // Number of decathlon athletes
  int<lower=0> N_individual;      // Number of individual event athletes
  int<lower=0> N_pred;            // Number of ages to predict for
  int<lower=1> A_decathlon;       // Number of unique decathlon athletes
  int<lower=1> A_individual;      // Number of unique individual event athletes
  int<lower=0, upper=A_decathlon> athlete_decathlon[N_decathlon];  // Athlete ID for decathlon
  int<lower=0> athlete_individual[N_individual]; // Athlete ID for individual events
  vector[N_decathlon] Y_decathlon;  // Decathlon performance data
  vector[N_individual] Y_individual; // Individual event performance data
  vector[N_decathlon] age_decathlon; // Ages of decathlon athletes
  vector[N_individual] age_individual; // Ages of individual event athletes
  vector[N_pred] age_pred;         // New age values for prediction
  vector[N_pred] is_new_athlete; // check if new athlete
  int<lower=0> athlete_pred[N_pred];  // Athlete IDs for predictions
}

parameters {
  vector[A_decathlon] alpha_decathlon;  // Random intercepts for decathletes
  vector[A_individual] alpha_individual; // Random intercepts for individual athletes
  real beta1;                   // Linear age effect
  real beta2;                   // Quadratic age effect
  real beta3;                   // Cubic age effect
  real mu_alpha_decathlon;                // Mean intercept across all athletes
  real mu_alpha_individual;
  real<lower=0> sigma_alpha_decathlon;     // Standard deviation of athlete intercepts
  real<lower=0> sigma_alpha_individual; // standard deviation of athlete intercepts for individual data
  real<lower=0> sigma_decathlon; // Residual standard deviation for decathlon events
  real<lower=0> sigma_individual;// Residual standard deviation for individual events
}

model {
  // Priors
  beta1 ~ normal(0, 5);
  beta2 ~ normal(0, 2);
  beta3 ~ normal(0, 1);
  mu_alpha_decathlon ~ normal(0, 10);
  mu_alpha_individual ~ normal(0, 10);
  sigma_alpha_decathlon ~ inv_gamma(2, 1);
  sigma_alpha_individual ~ inv_gamma(2, 1);
  sigma_decathlon ~ inv_gamma(2, 1);
  sigma_individual ~ inv_gamma(2, 1);

  // Hierarchical prior for athlete-specific intercepts
  alpha_decathlon ~ normal(mu_alpha_decathlon, sigma_alpha_decathlon);
  alpha_individual ~ normal(mu_alpha_individual, sigma_alpha_individual);

  // Likelihood for decathlon events

  Y_decathlon~ normal(
    alpha_decathlon[athlete_decathlon] +
    + beta1 * age_decathlon 
    + beta2 * square(age_decathlon) 
    + beta3 * pow(age_decathlon, 3), 
    sigma_decathlon
  );
  

  // Likelihood for individual events
  
  Y_individual ~ normal(
    alpha_individual[athlete_individual]  +
    + beta1 * age_individual
    + beta2 * square(age_individual) 
    + beta3 * pow(age_individual, 3), 
    sigma_individual
  );
  
}

generated quantities {
  vector[N_pred] Y_pred;        // Predictions for all cases

  for (n in 1:N_pred) {
    real alpha_effect;
    
    if (is_new_athlete[n] == 1) {
      // New athlete -> Draw from normal prior
      alpha_effect = normal_rng(mu_alpha_decathlon, sigma_alpha_decathlon);
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



