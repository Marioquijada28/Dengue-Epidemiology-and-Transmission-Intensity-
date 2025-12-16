
//---Constant-in-time dengue catalytic model by Region---//
// assumes constant endemic FOI 
// assumes complete immunity after 2nd infection
// assumes equal transmissability of 2 serotypes

data {
  int nA; // N age groups
  int nL; // N locations
  array[nL, nA] int cases; // reported case data
  matrix[nL, nA] pop; // population data
  array[2, nA] int ageLims; // lower & upper bounds of age groups
}


transformed data {
  int sum_cases; // Total cases across all age groups
  array[nA] int cases_age;    // Total cases per age group
  

  // Calculate total observed cases per age group (summing across locations)
  for (a in 1:nA) {
    cases_age[a] = sum(cases[, a]); 
  }

  // Calculate total number of observed cases
  sum_cases = sum(cases_age);
}

parameters {

  array[nL] real<lower=0, upper=0.5> lam_H; // FOI
  array[nL] real<lower=0, upper=1> rho; // reporting rate of 2nd infections by regions
  array[nL] real<lower=0, upper=1> gamma; // relative reporting rate of 1st infections
  real<lower=0 > kids; // relative reporting rate of kids under 10
}

transformed parameters {
  array[nL] vector<lower=0, upper=1>[nA] inc1; // incidence of primary infections
  array[nL] vector<lower=0, upper=1>[nA] inc2; // incidence of secondary infections
  array[nL] vector<lower=0, upper=1>[nA] D; // average annual incidence per person
  array[nL] vector<lower=0>[nA] Ecases; // expected reported cases per age group
  array[nL] real<lower=0> Ecases_tot; // expected total reported cases
  array[nL] vector<lower=0>[nA] Ecases_prob; // expected probability of reported cases per age group


  // Calculate incidence of primary and secondary infections by age group
 for (l in 1:nL) {
  for (a in 1:nA) {
    inc1[l,a] = exp(-4 * lam_H [l] * ageLims[1, a]) - exp(-4 * lam_H[l] * ageLims[2, a]);
    inc2[l,a] = 4 * (exp(-3 * lam_H [l] * ageLims[1, a]) - exp(-3 * lam_H [l] * ageLims[2, a])) -
              3 * (exp(-4 * lam_H [l] * ageLims[1, a]) - exp(-4 * lam_H [l] * ageLims[2, a]));
  }

  // Estimated disease probability by age group 
   // Single rho by Region
       for (a in 1:nA) {
      real e_rho;
      if (a == 2) {
        // Apply scaling factor for the 0-9 years old
        e_rho = rho[l] * kids;
      } else {
        // No scaling for other age groups
        e_rho = rho[l];
      }

      D[l, a] = e_rho * (inc2[l, a] + gamma[l] * inc1[l, a]) / (ageLims[2, a] - ageLims[1, a]);
      Ecases[l, a] = D[l, a] * pop[l, a]; // cases for each location and age group
    }


  // Calculate total number of cases (Ecases_tot)
  Ecases_tot [l] = sum(Ecases[l]);  

  // Probability of cases in each age group given total Ecases
  Ecases_prob [l]   = Ecases[l]  / Ecases_tot[l];
}
}

model {
  //--- priors
  lam_H ~ normal(0, 1);
  rho ~ normal(0, 1);
  gamma ~ normal(0, 1);
  kids ~ normal (0, 1); // for young kids

  //--- likelihood by Region
for (l in 1:nL) {
    target += poisson_lpmf(sum(cases[l]) | Ecases_tot[l]);
target += multinomial_lpmf(cases[l] | Ecases_prob[l]);
  }
}


