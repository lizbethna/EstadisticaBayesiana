##########
### Regla deBayes

### Una muestra
PBA <- function(p, AdadoB, ACdadoBC){
  (AdadoB*p)/(AdadoB*p+(1-ACdadoBC)*(1-p))
}
p = c(0.0001,0.001,0.01,0.1,0.5)
AdadoB = 0.99
ACdadoBC = 0.98
(p1 = PBA(p, AdadoB, ACdadoBC))

### Dos muestras
PBA2 <- function(p, AdadoB, ACdadoBC){
  (AdadoB*AdadoB*p)/(AdadoB*AdadoB*p+(1-ACdadoBC)*(1-ACdadoBC)*(1-p))
}
PBA2(p, AdadoB, ACdadoBC)
(p2 = PBA(p1, AdadoB, ACdadoBC))

### Tres muestras
PBA3 <- function(p, AdadoB, ACdadoBC){
  (AdadoB*AdadoB*AdadoB*p)/(AdadoB*AdadoB*AdadoB*p+(1-ACdadoBC)*(1-ACdadoBC)*(1-ACdadoBC)*(1-p))
}
PBA3(p, AdadoB, ACdadoBC)
(PBA(p2, AdadoB, ACdadoBC))

