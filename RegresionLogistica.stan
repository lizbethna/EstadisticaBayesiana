/*
Estadistica Bayesiana 
Regresion Logistica 
*/

data {
   int<lower = 0> n; // tamanio de muestra
   int<lower = 0> defunciones[n]; 
   int<lower = 0> expuestos[n]; 
   int<lower = 0> edad[n]; 
}

parameters {
   real beta[2]; 
}
 
transformed parameters {
   real eta[n]; 
   real<lower=0, upper=1> theta[n]; 
   for(i in 1:n){
      eta[i] = beta[1] + beta[2]*edad[i];
      theta[i] = exp(eta[i])/(1+exp(eta[i]));
   }
}

model {
   for(i in 1:n){
      defunciones[i] ~ binomial(expuestos[i],theta[i]); 
   }
  beta ~ normal(0,100); // dist inicial

}
