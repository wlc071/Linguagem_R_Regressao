################################
###   REGRESS?O DE POISSON   ###
################################


library(dplyr) # Manipula??o de dados


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

# Objetivo: Analisar reclama??es em uma nova empresa de internet

#ABRIR ARQUIVO
library(readxl)
reclamacoes <- read_xlsx('reclamacoes.xlsx')
View (reclamacoes)


# CRIA??O DOS MODELOS DE POISSON

modelo_poisson1 <- glm(velocidade ~ dia, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson1)

# Equa??o: velocidade = e^(3.767909+0.034464*dia)
reclamacoes$modelo_veloc <- modelo_poisson1$fitted.values

# Valida??o: AIC=94.094


# An?lise da dispers?o (ind?cio quando for maior que 1)
# Ho = N?o h? superdispers?o: p > 0,05
# Ha = H? superdispers?o: p <= 0,05
install.packages("AER")
library(AER)
dispersiontest(modelo_poisson1)



modelo_poisson2 <- glm(instabilidade ~ dia, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson2)
reclamacoes$modelo_insta <- modelo_poisson2$fitted.values

# Equa??o: instabilidade = e^(3.61161-0.09853*dia)

# Valida??o: AIC=76.245


# An?lise da dispers?o
dispersiontest(modelo_poisson2)




modelo_poisson3 <- glm(conexao ~ dia, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson3)
reclamacoes$modelo_con <- modelo_poisson3$fitted.values

# Equa??o: conexao = e^(2.860358-0.005103*dia)

# Valida??o: AIC=80.463

# An?lise da dispers?o
dispersiontest(modelo_poisson3)





modelo_poisson4 <- glm(velocidade ~ dia+instabilidade, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson4)

# Equa??o: velocidade = e^(3.7560316+0.0350997*dia+0.0003691*instabilidade)
reclamacoes$modelo_veloc2 <- modelo_poisson4$fitted.values

# Valida??o: AIC=96.093

# An?lise da dispers?o
dispersiontest(modelo_poisson4)




