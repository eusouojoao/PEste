set.seed(120)
amostras=900
n=675
lambda=1.61
options(digits=15)
## Calcular extremo da normal de lambda aproximada -----------------------------
gama=0.999
a=qnorm((1+gama)/2)
q=(1-gama)/2
a_tabela= 2.3263 #tabela utilizando o valor de q
mle <- rep(0,amostras)
avec <- rep(0,amostras)
for (i in 1:amostras){
  data <- rexp(n, lambda)
  mle[i] <- mean(data) # Valor esperado
  avec[i] <- (2*a)/(mle[i]*sqrt(n))
}
## Calcular amplitude ----------------------------------------------------------
amplitude = mean(avec); print(amplitude)