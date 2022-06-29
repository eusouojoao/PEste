options(digits=15)
set.seed(1666)
amostras=4015
d=77
n=21
p=0.33
## Calcular valor teórico ------------------------------------------------------
teorico=n*p # E(x) = n*p
## Calcular valor estimado -----------------------------------------------------
data=rep(0, amostras)
for (i in 1:amostras){
  data[i]=mean(rbinom(d, n, p))
}
estimado = mean(data)
# Modulo da diferença dos valores ----------------------------------------------
result = abs(teorico - estimado); print(result)