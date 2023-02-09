#####################################
###   REGRESS?O LINEAR M?LTIPLA   ###
#####################################

if(!require(dplyr)) install.packages("dplyr")
library(dplyr) # Manipula??o de dados


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("~/Linguagem R/arquivos")

#ABRIR ARQUIVO

enem <- read.csv('enem_2019_tratado.csv', sep = ",")
View (enem)

colegiox <- enem %>% filter (CO_ESCOLA=="35132287")


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(colegiox, function(x) sum(is.na(x)))
sapply(colegiox, function(x) sum(is.nan(x)))

# An?lise da classifica??o dos atributos
glimpse(colegiox)
str(colegiox)








## Constru??o do modelo 1:
modelo1 <- lm(NOTA_REDACAO ~ COMP2 + COMP4 + COMP5, colegiox)


# An?lise gr?fica


# Gr?fico 1: Linearidade.
# Gr?fico 2: Normalidade.
# Gr?fico 3: Homocedasticidade.
# Gr?fico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam n?o ter outliers.

## Normalidade dos res?duos:
# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(modelo1$residuals)


## Outliers nos res?duos (limites entre -3 e 3):
summary(rstandard(modelo1))


## Independ?ncia dos res?duos (Durbin-Watson):
# ideal para medidas repetidas (longitudinais: Ex: mesmo aluno)
if(!require(car)) install.packages("car")
library(car)
# Ho = res?duos independentes (n?o est?o correlacionados): p > 0.05
# Ha = res?duos dependentes : p <= 0.05
durbinWatsonTest(modelo1)


## Homocedasticidade (Breusch-Pagan):
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)
# Ho = existe homocedasticidade : p > 0.05
# Ha = n?o existe homocedasticidade : p <= 0.05
bptest(modelo1)


# Aus?ncia de Multicolinearidade (somente entre as vari?veis independentes)

variaveis <- subset(colegiox, select= c(COMP2, COMP4, COMP5))

if(!require(psych)) install.packages("psych")
library(psych)

pairs.panels(variaveis)
# Multicolinearidade:
# Considerada multicolinearidade quando r > 0.9.

vif(modelo1)
# Multicolinearidade ocorre quando VIF > 10

# An?lise do modelo
# Intercept
# p_valor para cada coeficiente < 0,05 (estatisticamente significativos).
# Adjusted R-squared (explica??o do modelo atrav?s dos dados)
# p_valor da estat?stica F < 0.05 (valida o modelo de regress?o)

summary(modelo1)

# Equa??o: Nota Reda??o=116.1987+1.2831*COMP2+1.72983*COMP4+1.19933*COMP5


# Criando uma coluna de previs?o: 
colegiox$previsao <- modelo1$fitted.values


# Coeficientes padronizados para compar?-los
if(!require(QuantPsyc)) install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(modelo1)








## Constru??o do modelo 2:
modelo2 <- lm(NOTA_REDACAO ~ COMP2 + COMP4, colegiox)


# An?lise gr?fica
plot(modelo2)

# Gr?fico 1: Linearidade.
# Gr?fico 2: Normalidade.
# Gr?fico 3: Homocedasticidade.
# Gr?fico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam n?o ter outliers.

## Normalidade dos res?duos:
# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(modelo2$residuals)


## Outliers nos res?duos (limites entre -3 e 3):
summary(rstandard(modelo2))


## Independ?ncia dos res?duos (Durbin-Watson):
# ideal para medidas repetidas (longitudinais: Ex: mesmo aluno)
if(!require(car)) install.packages("car")
library(car)
# Ho = res?duos independentes (n?o est?o correlacionados): p > 0.05
# Ha = res?duos dependentes : p <= 0.05
durbinWatsonTest(modelo2)


## Homocedasticidade (Breusch-Pagan):
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)
# Ho = existe homocedasticidade : p > 0.05
# Ha = n?o existe homocedasticidade : p <= 0.05
bptest(modelo2)


# Aus?ncia de Multicolinearidade (somente entre as vari?veis independentes)

variaveis <- subset(colegiox, select= c(COMP2, COMP4))

if(!require(psych)) install.packages("psych")
library(psych)

pairs.panels(variaveis)
# Multicolinearidade:
# Considerada multicolinearidade quando r > 0.9.

vif(modelo2)
# Multicolinearidade ocorre quando VIF > 10

# An?lise do modelo

# Intercept
# p_valor para cada coeficiente < 0,05 (estatisticamente significativos)
# Adjusted R-squared (explica??o do modelo atrav?s dos dados)
# p_valor da estat?stica F < 0.05 (valida o modelo de regress?o estatisticamente)
# N?o comparar os coeficientes, pois podem ter unidades diferentes.

summary(modelo2)

# Equa??o: Nota Reda??o = 214.0062+1.4391*COMP2 +2.1895*COMP4

# Criando uma coluna de previs?o: 
colegiox$previsao2 <- modelo2$fitted.values


# Coeficientes padronizados para poder compar?-los.
if(!require(QuantPsyc)) install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(modelo2)








# Compara??o dos modelos

# Para qualquer tipo de modelo
# O melhor modelo ? com resultado menor (menor varia??o dos res?duos)

# Crit?rio de Informa??o de Akaike (AIC)
AIC(modelo1, modelo2) 
# Crit?rio de Informa??o Bayesiano (BIC)
BIC(modelo1, modelo2) 


# Teste Anova para modelos aninhados
# (modelo 2 derivado do modelo 1 e mesma vari?vel dependente)
# O melhor ? com o menor valor do RSS (soma dos res?duos ao quadrado)
# Ho = modelos iguais: p>0,05
# Ha = modelos diferentes: p<0,05
anova(modelo1, modelo2)


# Gr?fico de dispers?o 3D
if(!require(scatterplot3d)) install.packages("scatterplot3d")
library(scatterplot3d)

# Modelo 1
grafico_3d <- scatterplot3d(colegiox$NOTA_REDACAO ~ colegiox$COMP2 + colegiox$COMP4 + colegiox$COMP5,
                       pch = 16, angle = 30, color = "steelblue", box = FALSE)

# Modelo 2
grafico_3d <- scatterplot3d(colegiox$NOTA_REDACAO ~ colegiox$COMP2 + colegiox$COMP4,
                            pch = 16, angle = 30, color = "steelblue", box = FALSE,
                            xlab="COMP2", ylab="COMP4", zlab="Notas")
grafico_3d$plane3d(modelo2, col="black", draw_polygon = TRUE)



# Sele??o autom?tica do melhor modelo

if(!require(MASS)) install.packages("MASS")
library(MASS)

modelo_total <- lm(NOTA_REDACAO ~ COMP2+COMP4+COMP5, data = colegiox)
modelo_nulo <- lm(NOTA_REDACAO ~ 1, data = colegiox)

stepAIC(modelo_total, scope = list(upper = modelo_total, lower = modelo_nulo),
                                  direction = "backward")





