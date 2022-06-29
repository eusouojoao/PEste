pacman::p_load(pacman, rio, tidyverse, ggplot2, hrbrthemes) # Load req. packages
## Dados -----------------------------------------------------------------------
utentes <- import("~/Projects/git/personal/PEst/Code_env/Utentes.xlsx")
result <- utentes[, c(3,4)]
# Covariância e coeficiente de correlação linear
IMC=result$IMC
TAD=result$TAD
cov=cov(IMC, TAD); sprintf("Covariância: %.2f", cov)
corr=cor(IMC, TAD); sprintf("Coeficiente de correlação linear: %.2f", corr)
## Gráfico de dispersão --------------------------------------------------------
ggplot(result, aes(x=IMC, y=TAD)) +
  geom_point(size=2, shape=23, color = 'royalblue3') +
  ggtitle("Gráfico de dispersão entre o TAD e IMC")  +
  theme(plot.title=element_text(hjust=0.5, size=32, face="bold"), 
        axis.title=element_text(size=24),
        axis.text=element_text(size=16))+
  geom_smooth(method=lm, color = 'darkblue')