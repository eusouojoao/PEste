pacman::p_load(pacman, rio, tidyverse, ggplot2, hrbrthemes) # Load req. packages
## Dados -----------------------------------------------------------------------
qualidade_ar <- import("~/Projects/git/personal/PEst/Code_env/QualidadeARO3.xlsx")
result <- qualidade_ar[, c(2,10)]
result$Entrecampos <- as.numeric(qualidade_ar$Entrecampos)
result$`VNTelha-Maia` <- as.numeric(qualidade_ar$`VNTelha-Maia`)
dataframe <- gather(result, "VNTelha-Maia", "Entrecampos",1:2)
## Histograma ------------------------------------------------------------------
ggplot(dataframe, aes(x = `Entrecampos`, fill = `VNTelha-Maia`)) +                    
  geom_histogram(position = "identity", alpha = 0.2, bins = 50) +
  labs(fill="Estações", x="Niveis de ozono (µg/m3)", y="Ocorrências") +
  ggtitle("Qualidade do ar em diferentes estações da QUALAR") +
  theme(plot.title = element_text(hjust = 0.5, size=32, face="bold"), 
        axis.title=element_text(size=24),
        axis.text=element_text(size=16),
        legend.title=element_text(size=24),
        legend.text=element_text(size=16))
## Valor médio da qualidade do ar nas estações ---------------------------------
E_avg = mean(result$Entrecampos); print(E_avg)
V_avg = mean(result$`VNTelha-Maia`); print(V_avg)