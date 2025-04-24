##################################################
### Paradigma Bayesiano - Bernoulli
##################################################

library(ggplot2)
library(VGAM)
library(rjags)
library(MCMCpack)
library(rstan) #STAN 

##################################################
### Bernoulli
##################################################
theta_true = 0.3 ### valor verdadero

##################################################
### Simular una muestra
### Likelihood   Binomial(y|size=1,theta) = Bernoulli(theta)  
n = 10   ### tamaño de muestra 
k = 1
Xi = rbinom(n=n, size=1, prob=theta_true)

y = sum(Xi)

### Prior   Beta(p|,a,b)
a = 2   ### hiperparametro de Beta
b = 2   ### hiperparametro de Beta 

### Una sola muestra observada
p = seq(0,1,0.01)

##################################################
### Posterior

	plot(p,dbeta(p,y+1,n-y+1), 
	     main=paste0("Muestra Y=",y),xlab=expression(theta), ylab="Densidad", 
	     lty=1, lwd=4, type="l", col="black")
	lines(p, dbeta(p,a,b), 
	      lty=2, lwd=4, col="red")
	lines(p, dbeta(p,a+y,b+n-y), 
	      lty=3, lwd=4, col="blue")
	legend("topright", legend=c("Prior","Verosimilitud","Posterior"), 
	       lty=c(1,2,3), col=c("red","black","blue"), lwd=2,cex=0.7)

##################################################
	### Predictiva  Prior & Posterior

	x = (0:k)
	plot(x,dbetabinom.ab(x, size=1, shape1=a+sum(y), shape2=b+1*n-sum(y)), 
	     main=paste0("Muestra suma Y=",sum(Xi)," media Y=",mean(Xi)),xlab="Y",ylab="Densidad",
	     type="p",col="black",lwd=2)
	points(x,dbetabinom.ab(x, size=1, shape1=a, shape2=b), 
	       type="h",col="blue",pch=19,cex=1.5,lwd=2)
	legend("topright", legend=c("Predictiva Prior","Predictiva Posterior"), 
	       lty=c(1,NA), col=c("black","blue"), pch=c(NA,19), cex=0.7)
	
##################################################
###	JAGS model

jags.model = "
model {  
for(i in 1:n){  
   Xi[i] ~ dbinom(theta,k); ### Likelihood  
}  
theta ~ dbeta(a,b);  ### Prior   
Xnew ~ dbinom(theta,k);  ### Predictive   
}
"
#setup data as list
data = list(Xi=Xi, k=k, n=length(Xi), 
            a=a,b=b)

#run JAGS model
j.model <- jags.model(file = textConnection(jags.model),
                      data=data,
                      n.chains=3)

#sample from the posterior
jags.out  <- coda.samples (model = j.model,
                            variable.names = c('theta','Xnew'),
                            n.iter = 1000)

#grab coefficients, compare fitted vs observed of z to prove this fits. 
summary(jags.out)
plot(jags.out)

################################################## 

##################################################
### Usando paquete de R: MCMCpack
## Not run:
posterior <- MCbinomialbeta(y=sum(Xi), n=n*k, 
                            alpha=a, beta=b, mc=5000)

summary(posterior)
plot(posterior)

grid <- seq(0,1,0.01)
plot(grid, dbeta(grid, a, b), type="l", col="red", lwd=3, ylim=c(0,3.6),
     xlab="pi", ylab="density")
lines(density(posterior), col="blue", lwd=3)
legend("topright", c("prior", "posterior"), lwd=3, col=c("red", "blue"))

##################################################

##################################################
## STAN

datos <- list( "Xi"=Xi, "n"=n,  # muestra
               "a"=a,"b"=b)  # valores iniciales de la distribucion inicial
param  = c("theta","Xnew") # parametros a estimar
fit_dist <- stan("Bayes2_ParadigmaBayesiano_Bernoulli.stan", data=datos,
                 chains=2, warmup=1000, iter=2000, thin=2)

## Resultados 

print(fit_dist, pars=param)
stan_plot(fit_dist,pars=param)
stan_ac(fit_dist,pars=param)
stan_trace(fit_dist,pars=param)

stan_dens(fit_dist,pars="theta", point_est = "mean", show_density = TRUE) + 
  ggtitle(expression(paste("Distribución final de ",theta))) + 
  ylab("Densidad") 

stan_hist(fit_dist,pars="Xnew", point_est = "mean", show_density = TRUE) + 
  ggtitle(expression(paste("Distribución predictiva final de ",X^{new}))) + 
  ylab("Densidad")  

##################################################



