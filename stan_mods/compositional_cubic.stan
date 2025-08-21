data {
  int<lower=0> N_decathlon;   // Number of decathlon observations 
  int<lower=0> N_pred;  
  int<lower=1> N_prev_events;      // Number of events leading up to current event. if modeling hundred_m, use simple_cubic.stan. If modeling 1500m, N_events = 9
  int<lower=1> A_decathlon;  // Unique decathlon athletes
  int<lower=0, upper=A_decathlon> athlete_decathlon[N_decathlon]; // Athlete ID (decathlon)
  matrix[N_decathlon, N_prev_events] Y_prev_events; // a predictor
  vector[N_decathlon] Y_decathlon;  // Decathlon event scores for this event 
  vector[N_decathlon] age_decathlon; // Ages of decathlon athletes

  
  // to make predictions
  // vector[N_pred] age_pred;
  vector[N_pred] is_new_athlete;
  int<lower=0, upper=A_decathlon> athlete_pred[N_pred];  // Athlete IDs for predictions
}

parameters {
  real beta1; // Linear age effect
  real beta2; // Quadratic age effect
  real beta3; // Cubic age effect
  vector[N_prev_events] betaY;   // Compositional effects
  real mu_alpha;

  vector[A_decathlon] alpha_decathlon;  // Athlete-specific intercepts (decathlon)


  real<lower=0> sigma_athlete_decathlon;  // Variability in athlete effects (decathlon)
  real<lower=0> sigma_decathlon;  // Event-specific noise

  
}

model {
  // Priors
    beta1 ~ normal(0, 1); # age slopes
    beta2 ~ normal(0, 1);
    beta3 ~ normal(0, 1);
    mu_alpha ~ normal(0, 1); // Prior mean for event-specific random intercepts (athlete-level)
    sigma_decathlon ~ inv_gamma(2, 1);
    sigma_athlete_decathlon ~ inv_gamma(2, 1);

  
  for (j in 1:N_prev_events)  {
      betaY[j] ~ normal(0, 1);  // Prior for compositional effects
    }

  // Athlete-specific random effects
  for (i in 1:A_decathlon) { // athlete i
      alpha_decathlon[i] ~ normal(mu_alpha, sigma_athlete_decathlon);
      }
  
  
    for (i in 1:N_decathlon) {
      real mu = alpha_decathlon[athlete_decathlon[i]] +
        beta1 * age_decathlon[i] +
        beta2 * square(age_decathlon[i]) +
        beta3 * pow(age_decathlon[i], 3);
      
        for (k in 1:(N_prev_events)) {
          mu += betaY[k] * Y_prev_events[i, k];
        }
      Y_decathlon[i] ~ normal(mu, sigma_decathlon);
      }
      
}
 
  

 
generated quantities {
  vector[N_pred] alpha_effect;        // Predictions for all cases

  for (n in 1:N_pred) {
    if (is_new_athlete[n] == 1) {
      // New athlete -> Draw from normal prior
      alpha_effect[n] = normal_rng(mu_alpha, sigma_athlete_decathlon);
    } else {
      // Existing athlete -> Use learned effect
      alpha_effect[n] = alpha_decathlon[athlete_pred[n]];
    }
  }
}

