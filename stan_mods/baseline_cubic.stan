data {
  int<lower=0> N_decathlon;       // Number of decathlon athletes
  int<lower=0> N_pred;            // Number of ages to predict for
  int<lower=1> A_decathlon;       // Number of unique decathlon athletes
  int<lower=0> athlete_decathlon[N_decathlon];  // Athlete ID for decathlon
  vector[N_decathlon] Y_decathlon;  // Decathlon performance data = Total Points
  vector[N_decathlon] age_decathlon; // Ages of decathlon athletes
  vector[N_pred] age_pred;         // New age values for prediction
  vector[N_pred] is_new_athlete; // check if new athlete
  int<lower=0> athlete_pred[N_pred];  // Athlete IDs for predictions
}
transformed data {
  vector[N_decathlon] x_std;
  vector[N_decathlon] y_std;
  vector[N_pred] x_pred_std;
  x_std = (age_decathlon - mean(age_decathlon)) / sd(age_decathlon);
  y_std = (Y_decathlon - mean(Y_decathlon)) / sd(Y_decathlon);
  x_pred_std = (age_pred - mean(age_decathlon)) / sd(age_decathlon);
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
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  beta3 ~ normal(0, 1);
  mu_alpha ~ normal(0, 1); # points have been standardized already so this is a reasonable prior
  sigma_alpha_decathlon ~ inv_gamma(2, 1);
  sigma_decathlon ~ inv_gamma(2, 1);


  // Hierarchical prior for athlete-specific intercepts
  alpha_decathlon ~ normal(mu_alpha, sigma_alpha_decathlon);


  // Likelihood for decathlon events
  y_std ~ normal(
  alpha_decathlon[athlete_decathlon] 
  + beta1 * x_std 
  + beta2 * square(x_std) 
  + beta3 * pow(x_std, 3), 
  sigma_decathlon
  );
}

generated quantities {
  vector[N_pred] Y_pred;        // Predictions for all cases
  vector[N_pred] alpha_effect;
  for (i in 1:N_pred) {
    if (is_new_athlete[i] == 1) {
      // New athlete -> Draw from normal prior
      alpha_effect[i] = normal_rng(mu_alpha, sigma_alpha_decathlon);
    } else {
      // Existing athlete ->  Use learned effect
      alpha_effect[i] = alpha_decathlon[athlete_pred[i]];
    }
  // Generate prediction
  Y_pred[i] = (normal_rng(alpha_effect[i] + 
         + beta1 * x_pred_std[i] 
         + beta2 * square(x_pred_std[i]) 
         + beta3 * pow(x_pred_std[i], 3),
         sigma_decathlon) *sd(Y_decathlon)) + mean(Y_decathlon);
  }
    
    
}



