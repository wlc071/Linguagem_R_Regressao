#######################################
###   REGRESS?O LOG?STICA BIN?RIA   ###
#######################################


library(dplyr) # Manipula??o de dados


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("~/Linguagem R/arquivos")


#ABRIR ARQUIVO
doenca_pre <- read.csv('casos_obitos_doencas_preexistentes.csv',
                       sep = ";")
View (doenca_pre)

doenca_pre <- read.csv('casos_obitos_doencas_preexistentes.csv',
                       sep = ";", encoding = 'UTF-8')
View (doenca_pre)

# Objetivo: Analisar se existe uma tend?ncia de ?bito entre 
# pessoas do sexo feminino e masculino.

table(doenca_pre$cs_sexo)

# EXCLUIR IGNORADO e INDEFINIDO DA VARI?VEL cs_sexo
relacao <- doenca_pre %>% filter(cs_sexo!="IGNORADO")
relacao <- relacao %>% filter(cs_sexo!="INDEFINIDO")
View(relacao)

# Quantidade de pessoas do sexo masculino e feminino (vari?vel independente)

library(plotly)
plot_ly(relacao, labels = ~ cs_sexo, type = 'pie')

# Quantidade de ?bitos (vari?vel dependente)
table(relacao$obito)

plot_ly(relacao, labels = ~ obito, type = 'pie')



# AN?LISE DOS REGISTROS (LINHAS)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(relacao, function(x) sum(is.na(x)))
sapply(relacao, function(x) sum(is.nan(x)))


# An?lise da classifica??o dos atributos

str(relacao)

# Alterando as vari?veis

relacao$obito[relacao$obito==0] <- "N?o"
relacao$obito[relacao$obito==1] <- "Sim"
str(relacao)

# Transformando string para fator

relacao$cs_sexo <- as.factor(relacao$cs_sexo)
relacao$obito <- as.factor(relacao$obito)
str(relacao)

levels(relacao$cs_sexo)  
levels(relacao$obito)






##### CONSTRU??O DO MODELO 1 (uma vari?vel independente):

# Vari?vel dependente bin?ria (dicot?mica)
# Categorias mutuamente exclusivas (uma pessoa n?o pode estar em duas situa??es)
# Independ?ncia das observa??es (sem medidas repetidas)


modelo1 <- glm(obito ~ cs_sexo, family = binomial,
               data = relacao)



# An?lise do modelo
# Estatisticamente significativo: p <= 0,05
# Estatisticamente n?o ? significativo: p > 0,05
# An?lise da Aus?ncia de outliers e pontos de alavancagem
# Deve estar entre -3 e 3

summary(modelo1)

# Raz?o de chance com Intervalo de confian?a de 95%

exp(coef(modelo1))


# CONCLUS?O:
# Estatisticamente, com intervalo de confian?a de 95%,
# A chance de uma pessoa do sexo masculino ir a ?bito ?
# 1,56 vezes maior do que a chance de uma pessoa do sexo feminino.

# Comprova??o de que a vari?vel depende obito ? o "SIM":
modelo_prova <- glm(cs_sexo ~ obito, family = binomial,
               data = relacao)

summary(modelo_prova)
exp(coef(modelo_prova))







##### MODELO 2 (mais de uma vari?vel independente):

# Diabetes e sexo

# Quantidade de diab?ticos e n?o diab?ticos
table(relacao$diabetes)

library(plotly)
plot_ly(relacao, labels = ~diabetes, type = 'pie')

# EXCLUIR IGNORADO DA VARI?VEL diabetes
relacao2 <- relacao %>% filter(diabetes!="IGNORADO")
table(relacao2$diabetes)
plot_ly(relacao2, labels = ~diabetes, type = 'pie')

# Quantidade de ?bitos
plot_ly(relacao, labels = ~ obito, type = 'pie') # antes
plot_ly(relacao2, labels = ~ obito, type = 'pie') # depois

# Quantidade por sexo
plot_ly(relacao, labels = ~ cs_sexo, type = 'pie') # antes
plot_ly(relacao2, labels = ~ cs_sexo, type = 'pie') # depois


# An?lise da classifica??o das vari?veis
str(relacao2)

# Transformando string para fator

relacao2$diabetes <- as.factor(relacao2$diabetes)
str(relacao2)

levels(relacao2$obito)  
levels(relacao2$diabetes)  
levels(relacao2$cs_sexo)  

# CONSTRU??O DO MODELO 2 (duas vari?veis independentes):

# Vari?vel dependente bin?ria (dicot?mica)
# Categorias mutuamente exclusivas (uma pessoa n?o pode estar em duas situa??es)
# Independ?ncia das observa??es (sem medidas repetidas)


modelo2 <- glm(obito ~ diabetes + cs_sexo, family = binomial, 
               data = relacao2)


# An?lise da Aus?ncia de outliers e pontos de alavancagem
# Deve estar entre -3 e 3
  library(MASS)
  summary(stdres(modelo2)) # res?duos padronizados


# Aus?ncia de multicolinearidade (VIF < 10)
library(car)
vif(modelo2)


# An?lise do modelo
# Estatisticamente significativo: p <= 0,05
# Estatisticamente n?o ? significativo: p > 0,05

summary(modelo2)

# Raz?o de chance com Intervalo de confian?a de 95%
exp(coef(modelo2))

# O resultado da diabetes est? inconsistente devido a presen?a enorme de
# dados ignorados.







###### MODELO 3 (vari?vel independente num?rica):

# Objetivo: ?bito por idade
relacao3 <- doenca_pre %>% filter(nome_munic =="Santos")
View(relacao3)

str(relacao3)


# AN?LISE DOS REGISTROS (LINHAS)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(relacao3, function(x) sum(is.na(x)))
sapply(relacao3, function(x) sum(is.nan(x)))

# Excluir valores missing em idade
library(tidyr)
relacao3 <- drop_na(relacao3, idade)


plot(relacao3$idade, relacao3$obito)


# Modelo de Regress?o Log?stica

modelo3 <- glm(obito ~ idade,data=relacao3, family="binomial")

summary(modelo3)

# Modelo comparado aos dados

plot(relacao3$idade,relacao3$obito,col='red',pch=20)
points(relacao3$idade,modelo3$fitted, pch=4)

# Testar o modelo com os pr?prios candidatos

previsao <- predict(modelo3, newdata=relacao3, type="response")
previsao <- previsao >= 0.3
previsao

# Avalia??o do desempenho (matriz de confus?o)

matriz_confusao = table(previsao,relacao3$obito)
matriz_confusao
acerto = (matriz_confusao[1] + matriz_confusao[4]) / sum(matriz_confusao)
acerto





# Teste com outro dataframe 

jundiai <- doenca_pre %>% filter(nome_munic =="Jundia?")
View(jundiai)

# Compara??o do modelo
jundiai$previsao <- predict(modelo3,newdata=jundiai,type="response")

# Valores missing
sapply(jundiai, function(x) sum(is.na(x)))
jundiai <- drop_na(jundiai, previsao)


jundiai$previsao[jundiai$previsao >= 0.3] <- 1 # previs?o de ?bito
jundiai$previsao[jundiai$previsao < 0.3] <- 0 # previs?o de n?o ?bito

table(jundiai$previsao)



# Comparando com os dados reais de Jundia?

# Reposicionando vari?vel obito
jundiai <- jundiai %>% relocate(obito, .before = previsao) 

# Determinando os acertos e erros
jundiai$acertos <- jundiai$obito + jundiai$previsao

jundiai$acertos[jundiai$acertos != 1] <- "ACERTOU"
jundiai$acertos[jundiai$acertos == 1] <- "ERROU"

table(jundiai$acertos)
library(plotly)
plot_ly(jundiai, labels = ~ acertos, type = 'pie')


# EXPORTAR ARQUIVO
write.table(jundiai, file ="jundiai.csv", sep = ";")
