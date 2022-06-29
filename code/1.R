pacman::p_load(pacman, rio, tidyverse, ggplot2, cowplot) # Load req. packages
## Dados -----------------------------------------------------------------------
residuos <- import("~/Projects/git/personal/PEst/Code_env/ResiduosPerCapita.xlsx")
req <- c("NL - Países Baixos", "SI - Eslovénia", "LV - Letónia")
temp <- filter(residuos, grepl(paste(req, collapse="|"), residuos$...1))
result <- temp[, c(1,2,3)] # Selecionar as colunas
aux <- data.frame(
  Anos=c(2004,2018),
  NL=as.numeric(c(result[1,2:3])),
  SI=as.numeric(c(result[2,2:3])),
  LV=as.numeric(c(result[3,2:3])),
  stringsAsFactors = FALSE
)
names(result)[names(result) == "...1"] <- "Países"
names(result)[names(result) == "...3"] <- "2018"
names(result)[names(result) == "Produção de resíduos per capita"] <- "2004"
# Formatar os dados para o gráfico de barras
data <- gather(result, "Anos", "Toneladas de resíduos per capita", 2:3)
data[3] = as.numeric(c(data[[3]]))
## Gráfico de barras -----------------------------------------------------------
p1 <- ggplot(data=data, aes(x=Países, y=`Toneladas de resíduos per capita`, fill=Anos)) +
      geom_bar(stat="identity", position=position_dodge()) +
      ggtitle("Produção de resíduos per capita") + 
      theme(plot.title=element_text(hjust=0.5, size=32, face="bold"), 
            axis.title=element_text(size=24),
            axis.text=element_text(size=16),
            legend.title=element_text((size=16)),
            legend.text=element_text(size=12)) +
      geom_text(aes(label=`Toneladas de resíduos per capita`), vjust=1.6, color="white", 
                position=position_dodge(0.9), size=6)
## Gráfico auxiliar ------------------------------------------------------------
p2 <- ggplot(data=aux, aes(x=Anos), fill=NULL) + ggtitle("Observações em gráfico linear") +
      theme(plot.title=element_text(hjust=0.5, size=32, face="bold"),
            axis.title=element_text(size=24),
            axis.text=element_text(size=16),
            aspect.ratio=0.9) +
      scale_x_continuous(breaks=as.numeric(unlist(aux[,1]))) +
      scale_y_continuous(breaks=as.numeric(unlist(aux[,2:4]))) +
      geom_line(aes(y=NL), color="red", arrow = arrow(length=unit(0.30,"cm"), type = "closed")) + 
      geom_line(aes(y=SI), color="green", arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
      geom_line(aes(y=LV), color="blue", arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
      geom_text(aes(x=2011, y=4.0), label="SI - Eslovénia", color="red", size=12) +    
      geom_text(aes(x=2011, y=1.1), label="LV - Letónia", color="green", size=12) +
      geom_text(aes(x=2011, y=6.1), label="NL - Países Baixos", color="blue", size=12) +
      labs(x="Anos", y="Toneladas de resíduos per capita")
## Plot do gráfico de barras e gráfico auxiliar --------------------------------
plot <- plot_grid(p1, p2, nrow=1, ncol= 2, labels=NULL); plot
## Média de todos os países ----------------------------------------------------
mean04 = mean(as.numeric(residuos$`Produção de resíduos per capita`[7:36]))
mean18 = mean(as.numeric(residuos$...3[7:36]))
sprintf("Média prod. resíduos em 2004: %.2f", mean04)
sprintf("Média prod. resíduos em 2018: %.2f", mean18)