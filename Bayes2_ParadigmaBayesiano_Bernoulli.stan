/*
Estadistica Bayesiana 
Distribucion Bernoulli
Prior Beta 
*/

// datos
data {
  int<lower = 0> n; // tamanio de muestra
  int<lower = 0> Xi[n]; // muestra observada
  real<lower=0> a; // inicial theta ~ Beta(a,b)
  real<lower=0> b; 
}

// parametros 
parameters {
  real<lower = 0, upper=1> theta ; // x ~ Beta(theta)
}


model {
  theta ~ beta(a,b); // distribucion inicial
  Xi ~ binomial(1,theta); // funcion de verosimilitud

}

// distribucion predictiva final 
generated quantities {
  int<lower = 0, upper=1> Xnew = binomial_rng(1,theta);

}




