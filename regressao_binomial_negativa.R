#######################################
###   REGRESS?O BINOMIAL NEGATIVA   ###
#######################################


library(dplyr) # Manipula??o de dados


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

# Objetivo: Analisar reclama??es em uma nova empresa de internet

#ABRIR ARQUIVO
library(readxl)
reclamacoes <- read_xlsx('reclamacoes.xlsx')
View (reclamacoes)


# CRIA??O DOS MODELOS DE REGRESS?ES BINOMIAL NEGATIVA
library(MASS)

modelo_binom_neg1 <- glm.nb(velocidade ~ dia, data = reclamacoes)
summary(modelo_binom_neg1)

# Equa??o: velocidade = e^(3.767909+0.034464*dia)
reclamacoes$modelo_veloc <- modelo_binom_neg1$fitted.values
View(reclamacoes)

# Valida??o: AIC=96.094





modelo_binom_neg2 <- glm.nb(instabilidade ~ dia, data = reclamacoes)
summary(modelo_binom_neg2)

# Equa??o: instabilidade = e^(3.61161-0.09853*dia)
reclamacoes$modelo_insta <- modelo_binom_neg2$fitted.values
View(reclamacoes)

# Valida??o: AIC=78.245





modelo_binom_neg3 <- glm.nb(conexao ~ dia, data = reclamacoes)
summary(modelo_binom_neg3)

# Equa??o: conexao = e^(2.860358-0.005103*dia)

reclamacoes$modelo_con <- modelo_binom_neg3$fitted.values
View(reclamacoes)

# Valida??o: AIC=82.463





modelo_binom_neg4 <- glm.nb(velocidade ~ dia+instabilidade, data = reclamacoes)
summary(modelo_binom_neg4)

# Equa??o: velocidade = e^(3.7560318+0.0350997*dia+0.0003691*instabilidade)

reclamacoes$modelo_veloc2 <- modelo_binom_neg4$fitted.values
View(reclamacoes)

# Valida??o: AIC=98.093



