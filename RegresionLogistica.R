##################################################
### TASA DE MORTALIDAD
### REGRESION LOGISTICA
##################################################

setwd("~/Documents/FCiencias_NotasCiencias/CursoCortoBayesiana/Ejemplos/")
datos <- read.csv("MortalidadMujeres.csv",header=TRUE,sep=";",dec=".")  

library(rstan) #STAN 

##################################################
summary(datos)
attach(datos)
head(datos)
##################################################
### Mortalidad
qx <- Defunciones/Expuestos
plot(Edad,qx,
     col=Vigencia,pch=19,cex=0.5)

### Transformar, para conseguir una relacion lineal
z <- log(qx/(1-qx))
plot(Edad,z,
     col=Vigencia,pch=19,cex=0.5)

##################################################
### Estadistica Frecuentista

vivos = Expuestos - Defunciones 
mod <- glm( cbind(Defunciones, vivos) ~ Edad, 
                 family=binomial(link='logit'))
summary(mod)

plot(Edad, qx)
points(Edad,mod$fit, 
       col="red", pch=19, cex=0.5)

yxlogit = log(mod$fit/(1-mod$fit))
plot(x,y)
lines(Edad, yxlogit, 
      col="red", lwd=2)

##################################################
### Estadistica Bayesiana STAN

datos <- list( "n"=length(Defunciones), 
               "defunciones"=Defunciones, 
               "expuestos"=Expuestos,
               "edad"=Edad)  

param  = c("beta") 

fit <- stan("RegresionLogistica.stan", data=datos,
                 chains=2, warmup=500, iter=1000, thin=2)

print(fit, pars=param)
stan_plot(fit,pars=param)
stan_ac(fit,pars=param)
stan_trace(fit,pars=param)

stan_dens(fit,"beta") 
stan_hist(fit,"beta")  

##################################################
