###################################
###   REGRESS?O QUASI POISSON   ###
###################################


library(dplyr) # Manipula??o de dados


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

# Objetivo: Analisar reclama??es em uma nova empresa de internet

#ABRIR ARQUIVO
library(readxl)
reclamacoes <- read_xlsx('reclamacoes.xlsx')
View (reclamacoes)


# CRIA??O DOS MODELOS DE QUASI POISSON

modelo_quasi1 <- glm(velocidade ~ dia, data = reclamacoes,
                    family = "quasipoisson")
summary(modelo_quasi1)
# Equa??o: velocidade = e^(3.767909+0.034464*dia)
reclamacoes$modelo_veloc <- modelo_quasi1$fitted.values

# Valida??o: AIC=NA




modelo_quasi2 <- glm(instabilidade ~ dia, data = reclamacoes,
                     family = "quasipoisson")
summary(modelo_quasi2)

# Equa??o: instabilidade = e^(3.61161-0.09853*dia)
reclamacoes$modelo_insta <- modelo_quasi2$fitted.values
View(reclamacoes)





modelo_quasi3 <- glm(conexao ~ dia, data = reclamacoes,
                        family = "quasipoisson")
summary(modelo_quasi3)

# Equa??o: conexao = e^(2.860358+0.005103*dia)

reclamacoes$modelo_con <- modelo_quasi3$fitted.values
View(reclamacoes)





modelo_quasi4 <- glm(velocidade ~ dia+instabilidade, data = reclamacoes,
                     family = "quasipoisson")
summary(modelo_quasi4)

# Equa??o: velocidade = e^(3.7560318+0.0350997*dia+0.0003691*instabilidade)

reclamacoes$modelo_veloc2 <- modelo_quasi4$fitted.values
View(reclamacoes)


