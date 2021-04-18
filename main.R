# Libs.
library(ggplot2)
library(dplyr)

csv = read.table(sep = ",", "./survey-result.csv", header=TRUE)

# HIPOTESE 1 ---------------------------------------------------------------------------------------------------

mean_calculo = mean(csv$calculo)
mean_linear = mean(csv$linear)
mean_fmcc = mean(csv$fmcc)
mean_probabilidade = mean(csv$probabilidade)
mean_estatistica = mean(csv$estatistica)
mean_logica = mean(csv$logica)

df <- data.frame(
    disciplinas = c("calculo", "linear", "fmcc", "probabilidade", "estatistica", "logica"),
    mean_importancia = c(mean_calculo, mean_linear, mean_fmcc, mean_probabilidade, mean_estatistica, mean_logica)
  )

print(df)

p = ggplot(df, aes(x=disciplinas, y=mean_importancia, fill = as.factor(mean_importancia)) ) +
    geom_bar(stat = "identity") +
    scale_fill_grey(start = 0.75, end = 0.25) +
    theme(legend.position = "none") +
    labs(x = "Disciplinas", y = "Média da importância") +
    scale_x_discrete(limits=c("calculo", "linear", "estatistica", "fmcc", "probabilidade", "logica"))

ggsave("media_da_importancia.png", p, width = 6, height = 3)

# --------------------------------------------------------------------------------------------------------------


# HIPOTESE 2 ---------------------------------------------------------------------------------------------------

df = subset(csv, select = -c(data_hora, calculo_retirado, mat_util, justificativa, tecnologias) )
df["mean_importance"] = (df$calculo + df$linear +  df$fmcc + df$probabilidade + df$logica + df$estatistica) / 6
print(df)
p = ggplot(df, aes(x=periodo, y=mean_importance, group= periodo) ) +
    geom_boxplot() +
    labs(x = "Periodo", y = "Média da importância das \n6 disciplinas por aluno")

ggsave("importancia_por_periodo.png", p, width = 6, height = 3)

# --------------------------------------------------------------------------------------------------------------
