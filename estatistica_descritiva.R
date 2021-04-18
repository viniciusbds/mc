# Libs.
library(ggplot2)
library(dplyr)

csv = read.table(sep = ",", "./survey-result.csv", header=TRUE)

mean_calculo = mean(csv$calculo)
median_calculo = median(csv$calculo)

mean_linear = mean(csv$linear)
median_linear = median(csv$linear)

mean_fmcc = mean(csv$fmcc)
median_fmcc = median(csv$fmcc)

mean_probabilidade = mean(csv$probabilidade)
median_probabilidade = median(csv$probabilidade)

mean_estatistica = mean(csv$estatistica)
median_estatistica = median(csv$estatistica)

mean_logica = mean(csv$logica)
median_logica = median(csv$logica)

df = subset(csv, select = -c(data_hora, calculo_retirado, mat_util, justificativa, tecnologias) )

# CALCULO ----------------------------------------------------------------------
p = ggplot(df, aes(x=as.factor(calculo)) ) +
    geom_bar() +
    theme(legend.position = "none") +
    labs(x = "Importancia", y = "N° pessoas") +
    scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    geom_vline(xintercept = mean_calculo, color = "red", size=0.5) +
    geom_vline(xintercept = median_calculo, color = "blue", size=0.5) +
    geom_vline(xintercept = 1, color = "green", size=0.5)

ggsave("bar_plot_calculo.png", p, width = 6, height = 3)

# LINEAR ----------------------------------------------------------------------
p = ggplot(df, aes(x=as.factor(linear)) ) +
    geom_bar() +
    theme(legend.position = "none") +
    labs(x = "Importancia", y = "N° pessoas") +
    scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    geom_vline(xintercept = mean_linear, color = "red", size=0.5) +
    geom_vline(xintercept = median_linear, color = "blue", size=0.5) +
    geom_vline(xintercept = 3, color = "green", size=0.5)

ggsave("bar_plot_linear.png", p, width = 6, height = 3)

# FMCC ----------------------------------------------------------------------
p = ggplot(df, aes(x=as.factor(fmcc)) ) +
    geom_bar() +
    theme(legend.position = "none") +
    labs(x = "Importancia", y = "N° pessoas") +
    scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    geom_vline(xintercept = mean_fmcc, color = "red", size=0.5) +
    geom_vline(xintercept = median_fmcc, color = "blue", size=0.5) +
    geom_vline(xintercept = 7.05, color = "green", size=0.5)

ggsave("bar_plot_fmcc.png", p, width = 6, height = 3)

# PROB ----------------------------------------------------------------------
p = ggplot(df, aes(x=as.factor(probabilidade)) ) +
    geom_bar() +
    theme(legend.position = "none") +
    labs(x = "Importancia", y = "N° pessoas") +
    scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    geom_vline(xintercept = mean_probabilidade, color = "red", size=0.5) +
    geom_vline(xintercept = median_probabilidade, color = "blue", size=0.5) +
    geom_vline(xintercept = 10, color = "green", size=0.5)

ggsave("bar_plot_probabilidade.png", p, width = 6, height = 3)

# ESTATISTICA --------------------------------------------------------------------
p = ggplot(df, aes(x=as.factor(estatistica)) ) +
    geom_bar() +
    theme(legend.position = "none") +
    labs(x = "Importancia", y = "N° pessoas") +
    scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    geom_vline(xintercept = mean_estatistica, color = "red", size=0.5) +
    geom_vline(xintercept = median_estatistica, color = "blue", size=0.5)+
    geom_vline(xintercept = 10, color = "green", size=0.5) +
    geom_vline(xintercept = 1, color = "green", size=0.5)
ggsave("bar_plot_estatistica.png", p, width = 6, height = 3)

# LOGICA --------------------------------------------------------------------
p = ggplot(df, aes(x=as.factor(logica)) ) +
    geom_bar() +
    theme(legend.position = "none") +
    labs(x = "Importancia", y = "N° pessoas") +
    scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    geom_vline(xintercept = mean_logica, color = "red", size=0.5) +
    geom_vline(xintercept = median_logica, color = "blue", size=0.5)+
    geom_vline(xintercept = 10, color = "green", size=0.5)
ggsave("bar_plot_logica.png", p, width = 6, height = 3)