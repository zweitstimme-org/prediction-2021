data {
  int<lower=0> nParties;
  int<lower=0> nInst;
  int<lower=0> nPolls;
  int<lower=0> nPeriods;
  int<lower=0, upper = nInst> iid[nPolls];
  int<lower=0, upper = nPeriods> date[nPolls];
  int<lower=0> y[nPolls,nParties];
  int<lower=0> LA;
  int<lower=0> L;
  int<lower=0> Nobs;
  int<lower=0> Nmis;
  int<lower=0> N;
  int<lower=0> election[N];
  int ind[LA,2];
  int<lower=0> K;
  real n_shock;

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
  simplex[nParties] vE;
  vector[L] b0;
  matrix[L,K] b;
  real<lower=0> tau_b0;
  real<lower=0> tau_b[K];
  real drift_b0;
  real drift[K]; 
  cholesky_factor_corr[nParties-1] S_cor; 
  cholesky_factor_corr[nParties-1] S_shock_cor; 
  vector<lower=0>[nParties-1] sigma_evo;
  vector<lower=0>[nParties-1] sigma_shock;
  vector[nParties] alphastarforecast;
  matrix[nPeriods,nParties] alphastar;
  matrix[nInst,nParties] house_effect_raw;
}
transformed parameters {
  vector[N] v;
  vector[N] a;
  vector[nParties] a_pred;
  real<lower=0> sigma_b0;
  real<lower=0> sigma_b[K];
  real mu_b0[L];
  matrix[L,K] mu_b;
  matrix[nPeriods,nParties] alphastar_prior;
  vector[nParties] alphastarforecast_prior;
  matrix[nInst,nParties] house_effect;
  matrix[nPeriods,nParties] alpha;
  matrix[nPeriods,nParties] ea;
 
  vector[nParties] ef;
  vector[nParties] forecast;
  


  // cov_matrix[nParties-1] S;
  // cov_matrix[nParties-1] S_shock;

  // S = quad_form_diag(S_cor, sigma_evo);
  // S_shock = quad_form_diag(S_shock_cor, sigma_shock);

  alphastar_prior = alphastar;
  alphastarforecast_prior = alphastarforecast;
  
  house_effect = house_effect_raw;
  
  for (j in 2:nParties)
          house_effect[1, j] = 0 - sum(house_effect[2:nInst, j]);
      
      
      for(c in 1:nInst)  
          house_effect[c, 1] = 0 - sum(house_effect[c, 2:nParties] );
      

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
    mu_b0[j] = b0_prior[j-1] + drift_b0;

  for (k in 1:K)
    for(j in 2:L)
      mu_b[j,k] = b_prior[j-1,k] + drift[k];
  


    for (j in 1:(nParties)) {
        alphastar_prior[nPeriods,j] = log(vE[j]/vE[nParties]);
      } 

   for (i in 1:(nPeriods-1))
    alphastar_prior[i,nParties] = 0;   
  
   for(k in 1:nPolls) {
  alphastar_prior[date[k],] = (alphastar_prior[date[k],]  + house_effect[iid[k],]);  
  }

  alphastarforecast_prior[nParties] = 0;  


  for (i in 1:(nPeriods)) 
        for (j in 1:(nParties)) 
          ea[i,j] = exp(alphastar_prior[i,j]);

  for (i in 1:(nPeriods)) 
    for (j in 1:(nParties)) 
      alpha[i,j] = ea[i,j]/sum(ea[i,]);   

  for (j in 1:(nParties))
    ef[j] = exp(alphastarforecast_prior[j]);
  
  for (j in 1:(nParties))   
    forecast[j] = ef[j]/sum(ef[1:nParties]);
    

               
}
model {
  
  int pos; 
  pos = 1;

  b0[1] ~ normal(b0_prior[1], 10); // initialization
  drift_b0 ~ normal(0, 10);
  tau_b0 ~ inv_gamma(1, 1);

  for (j in 2:L)
    b0[j] ~ normal(mu_b0[j], sigma_b0);

  for (k in 1:K) {
    b[1,k] ~ normal(b_prior[1,k], 10);
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


  sigma_evo ~ normal(0, 0.1);
  sigma_shock ~ normal(0.2, 0.1);

  S_cor ~ lkj_corr_cholesky(50);
  S_shock_cor ~ lkj_corr_cholesky(100);

  for (i in 1:(nPeriods-1)){
    
    alphastar[i,1:(nParties-1)] ~ multi_normal_cholesky(alphastar_prior[(i+1),1:(nParties-1)], diag_pre_multiply(sigma_evo, S_cor)); 
    }

  
  alphastarforecast[1:(nParties-1)] ~ multi_normal_cholesky(alphastar_prior[nPeriods, 1:(nParties-1)], diag_pre_multiply(sigma_shock, S_shock_cor));

  for (k in 1:nPolls)
    y[k,1:nParties] ~ multinomial(alpha[date[k],]'); 

  for (j in 1:(nParties)) 
        for (c in 1:nInst)
          house_effect_raw[c, j] ~ normal(0, 0.001); // Prior prior 1 percent point sd
         
  
}
