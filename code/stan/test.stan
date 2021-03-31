
 forJags <- list(
          
                        # Dynamic Model
                          y = Y,
                          nParties = nParties,
                          nPeriods =  time_window[1]+1, 
                          nPolls = nrow(Y),
                          iid = polls$iid,
                          nInst = max(polls$iid),
                          date = polls$t,
                          size = NObs, 
                          R0 = diag(rep(1,(nParties-1)))*0.01, n0 = 7, # Prior on Wishardt
                          shock = solve(diag(rep(1,(nParties-1)))), n_shock = 30.8642,
                          
                        # Fundamental Model
                          LA = nrow(index),
                          L = nrow(index)+1,# Number of elections
                          N = length(election_res), #Number of observations
                          v_obs = c(election_res/100),  # Dependent variable
                          ind = index,        # Create index function (which observation part of election row l)
                          x = election_pred, # Predictors past elections
                          K = ncol(election_pred),   # Number of predictors
                          election = dat_sub$election,
                          xE = as.matrix(election_pred_E), # Predictors for upcoming election
                          b_prior = structural_inits$b, b0_prior = structural_inits$b0
        
                        )
  


data {
  int<lower=0> nParties;
  int<lower=0> nInst;
  int<lower=0> nPolls;
  int<lower=0> nPeriods;
  int<lower=0, upper = nInst> iid[nPolls];
  int<lower=0, upper = nPeriods> date[nPolls];
  int<lower=0> size[nPolls];
  matrix<lower=0>[nPolls,nParties] y;
  int<lower=0> LA;
  int<lower=0> L;
  int<lower=0> Nobs;
  int<lower=0> Nmis;
  int<lower=0> N;
  int<lower=0> election[N];
  int ind[LA,2];
  int<lower=0> K;
  
  int<lower = 1, upper = Nobs + Nmis> ii_obs[Nobs];
  int<lower = 1, upper = Nobs + Nmis> ii_mis[Nmis];
  vector[Nobs] v_obs;
  matrix[N,K] x;
  matrix[Nmis,K] xE;
  int s[LA];
  matrix[L,K] b_prior;
  vector[L] b0_prior;    
}
parameters {
  simplex[Nmis] v_mis;
  vector[L] b0;
  matrix[L,K] b;
  real<lower=0> tau_b0;
  real<lower=0> tau_b[K];
  real drift_b0;
  real drift[K];  
}
transformed parameters {
  vector[N] v;
  vector[N] a;
  vector[nParties] vE;
  vector[nParties] a_pred;
  real<lower=0> sigma_b0;
  real<lower=0> sigma_b[K];
  real mu_b0[L];
  matrix[L,K] mu_b;
  matrix[nPeriods,nParties] alphastar;

  v[ii_obs] = v_obs;
  v[ii_mis] = v_mis;
  
  for (i in 1:N)
    a[i] = exp(b0[election[i]] + b[election[i],] * x[i,]');

  for (j in 1:nParties)
    a_pred[j] = exp(b0[L] + b[L,] * xE[j,]');

  sigma_b0 = sqrt(tau_b0);
  sigma_b = sqrt(tau_b);

  
  mu_b0[1] = 0;
  for (k in 1:K)
    mu_b[1,k] = 0;

  for (j in 2:L)
    mu_b0[j] = b0[j-1] + drift_b0;

  for (k in 1:K)
    for(j in 2:L)
      mu_b[j,k] = b[j-1,k] + drift[k];

  for (j in 1:(nParties)) {
        alphastar[nPeriods,j] <- log(vE[j]/vE[nParties]);
      } 
}
model {
  int pos; 
  pos = 1;

  b0[1] ~ normal(0, 10); // initialization
  drift_b0 ~ normal(0, 10);
  tau_b0 ~ inv_gamma(1, 1);

  for (j in 2:L)
    b0[j] ~ normal(mu_b0[j], sigma_b0);

  for (k in 1:K) {
    b[1,k] ~ normal(0, 10);
    drift[k] ~ normal(0, 10);
    tau_b[k] ~ inv_gamma(1, 1);
  }

  for (k in 1:K)
    for(j in 2:L)
      b[j,k] ~ normal(mu_b[j,k], sigma_b[k]);
  
  for (l in 1:LA) {
    segment(v, pos, s[l]) ~ dirichlet(segment(a, pos, s[l]));
    pos = pos + s[l];
  }

  vE ~ dirichlet(a_pred);


}