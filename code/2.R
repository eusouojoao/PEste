pacman::p_load(pacman, rio, tidyverse, ggplot2, hrbrthemes) # Load req. packages
## Dados -----------------------------------------------------------------------
esperanca_vida <- import("~/Projects/git/personal/PEst/Code_env/EsperancaVida.xlsx")
result <- esperanca_vida[c(48:65), c(1, 54, 55, 56, 88, 89, 90)]
result$...54 <- as.numeric(esperanca_vida$...54[48:65])
result$...55 <- as.numeric(esperanca_vida$...55[48:65])
result$...56 <- as.numeric(esperanca_vida$...56[48:65])
result$...88 <- as.numeric(esperanca_vida$...88[48:65])
result$...89 <- as.numeric(esperanca_vida$...89[48:65])
result$...90 <- as.numeric(esperanca_vida$...90[48:65])
# Rename das colunas
names(result)[names(result) == "...1"] <- "Anos"
names(result)[names(result) == "...56"] <- "Letónia Homens"
names(result)[names(result) == "...55"] <- "Itália Homens"
names(result)[names(result) == "...54"] <- "Irlanda Homens"
names(result)[names(result) == "...90"] <- "Letónia Mulheres"
names(result)[names(result) == "...89"] <- "Itália Mulheres"
names(result)[names(result) == "...88"] <- "Irlanda Mulheres"
gtemporal <- gather(result, "Países", "Esperança de vida à nascença",2:7)
## Gráfico temporal ------------------------------------------------------------
ggplot(gtemporal, aes(x=`Anos`, y=`Esperança de vida à nascença`, color=`Países`, 
                      group =`Países`)) +
  geom_point(size=2, shape=23) +
  geom_line() +
  labs(x="Anos", y="Idade") +
  scale_color_manual(values=c('blue', 'dodgerblue2', 'black', 'gray31', 'red', 
                              'tomato1')) + 
  labs(color='Países',fill='Países') + 
  ggtitle("Esperança de vida à nascença entre 2002 e 2019")  +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, size=32, face="bold"), 
        axis.title=element_text(size=24),
        axis.text=element_text(size=16),
        legend.title=element_text(size=24),
        legend.text=element_text(size=16)) 