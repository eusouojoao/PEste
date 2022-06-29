options(digits=15)
set.seed(1885)
n=502
lambda=0.18
## Valor teórico ---------------------------------------------------------------
teorico=0.486752 # Integral de 4 a +inf de 0.18e^(-0.18x)
## Valor estimado --------------------------------------------------------------
t=0
data <- data.frame(X=rexp(n, lambda))
for(i in 1:n) {
    if(data$X[i]>4)
      t=t+1 # Valores superiores a 4
}
# Valor estimado
estimado=t/n
# Modulo da diferença entre os valores
result = abs(teorico - estimado); print(result)