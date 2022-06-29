pacman::p_load(pacman, rio, tidyverse, datasets, ggplot2, dplyr, data.table, 
               hrbrthemes) # Load req. packages
options(digits=15)
set.seed(50)
amostras=1000
lambda1=4.57
lambda2=0.19
gama=0.98
rc=0.20 # Contaminados
size=2500/100
MA_p <- rep(0, size)
MA_c <- rep(0, size)
vec <- rep(0, size)
mlep <- rep(0,amostras)
mlec <- rep(0,amostras)
avec_p <- rep(0,amostras)
avec_c <- rep(0,amostras)
a=qnorm((1+gama)/2) # Extremo da normal de lambda aproximada
## Variar o valor de dimensão das amostras -------------------------------------
for(m in 1:25){
  n=m*100
  # Simular a situação amostras vezes
  for (i in 1:amostras){
    data_p <- rexp(n, lambda1) #vetor não contaminado
    temp1 <- rexp(floor(n*rc), lambda2) #vetor contaminado
    #mlep é o valor esperado da data não contaminada
    mlep[i] <- mean(data_p)
    avec_p[i] <- (2*a)/(mlep[i]*sqrt(n))
    # Criar o vetor contaminado substituindo os primeiros 20% dos dados puros pelos contaminados
    temp2 = data_p
    for(k in 1:floor(n*rc)){
      temp2[k] <- temp1[k]
    }
    data_c = temp2
    mlec[i] <- mean(data_c) # Valor esperado da data contaminada
    avec_c[i] <- (2*a)/(mlec[i]*sqrt(n))
  }
  MA_p[m]= mean(avec_p) # Amplitude da amostra não contaminada
  MA_c[m]= mean(avec_c)   # Amplitude da amostra não contaminada
  vec[m] <- n # Array com no. de amostras utilizado
}
data_1 = data.frame(vec, MA_p)
data_2 = data.frame(vec, MA_c)
## Gráfico ---------------------------------------------------------------------
ggplot() + theme_linedraw() +
  ggtitle("Evolução da amplitude do intervalo de confiança em função da \n dimensão de amostras numa distribuição exponencial pura e contaminada") + 
  geom_point(data = data_1, aes(vec, MA_p, color = "MA_p - Amplitude \n sem contaminação"),
             shape=1, size=2, alpha = 1)  + geom_line(data=data_1, aes(x=vec, y=MA_p), color="red") +
  geom_point(data = data_2, aes(vec, MA_c, color = "MA_c - Amplitude \n com contaminação"),
             shape=1, size=2, alpha = 1) + geom_line(data=data_2, aes(x=vec, y=MA_c)) +
  labs(x="Dimensão das amostras", y="Amplitude do intervalo de confiança",
       color='Variável',fill='Variável') +
  theme(plot.title=element_text(hjust=0.5, size=32, face="bold"), 
        axis.title=element_text(size=24),
        axis.text=element_text(size=16),
        legend.title=element_text(size=24),
        legend.text=element_text(size=16)) +
  scale_color_manual(values=c('blue', 'red'))