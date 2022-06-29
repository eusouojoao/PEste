pacman::p_load(pacman, rio, tidyverse, datasets, ggplot2, dplyr, data.table, 
               hrbrthemes) # Load req. packages
set.seed(272)
amostras=850
size=5000/100
lambda=1.77
options(digits=15)
## Extremo da normal de lambda approx. -----------------------------------------
gama=0.93
a=qnorm((1+gama)/2)
q=(1-gama)/2

MA <- rep(0, size)
vec <- rep(0, size)
avec <- rep(0, amostras)

for(m in 1:50){
  n=m*100
  mle <- rep(0, amostras)
  # Simular a situação amostras vezes
  for (i in 1:amostras){
    mle[i] <- mean(rexp(n, lambda)) # Valor esperado (1/lambda)
    avec[i] <- (2*a)/(mle[i]*sqrt(n))
  }
  ## Amplitude
  MA[m]= mean(avec)
  vec[m] <- n # Array com no. de amostras utilizado
}
data = data.frame(vec, MA)
## Gráfico
ggplot(data, aes(x=vec, y=MA)) + theme_linedraw() +
  ggtitle("Evolução da amplitude do intervalo de confiança em função \n da dimensão de amostras numa distribuição exponencial") +
  geom_point(size=3, shape=23, color="dark blue") + geom_line(size = 1 , colour = "#007ED9" ) +
  labs(x="Dimensão das amostras", y="Amplitude do intervalo de confiança") +
  theme(plot.title=element_text(hjust=0.5, size=32, face="bold"), 
        axis.title=element_text(size=24),
        axis.text=element_text(size=16))