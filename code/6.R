pacman::p_load(pacman, rio, cowplot, tidyverse, datasets, ggplot2, dplyr, 
               data.table, hrbrthemes) # Load req. packages
options(digits=15)
amostras=1940
a=9; b=13
E=(a+b)/2 # Valor teórico para o valor esperado
V=((b-a)^2)/12 # Valor teórico para a variância
## Simulação -------------------------------------------------------------------
sim <- function(n){
  set.seed(731)
  Vn=V/n
  data=rep(0, amostras)
  data_avg=data.frame(rep(0, amostras))
  avg=rep(0, amostras)
  for(i in 1:amostras){
    data[i] = data.frame(X=runif(n, a, b))
    avg[i] = mean(data[[i]])
  }
  data_avg <- data.frame(avg)
  # Plot do gráfico
  plot <- ggplot(data_avg, aes(x = avg)) + 
    geom_histogram(aes(y = ..density..), colour = 1, fill = "white", bins=20) +
    labs(x="Média da amostra", y="Densidade") + 
    stat_function(fun = dnorm, args = list(mean = E, sd = sqrt(Vn))) +
    theme(plot.title=element_text(hjust=0.5, size=32, face="bold"), 
          axis.title=element_text(size=24),
          axis.text=element_text(size=16)) + ggtitle(paste0("n = ", n))
  return(plot)
}
## Histogramas sobrepostos com as curvas com a distribuição normal -------------
plot_grid(sim(2), sim(21), sim(95), nrow=3, ncol=1)