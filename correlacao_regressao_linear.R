####################################
###   REGRESS?O LINEAR SIMPLES   ###
####################################

if(!require(dplyr)) install.packages("dplyr")
library(dplyr) # Manipula??o de dados


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("~/Linguagem R/arquivos")

#ABRIR ARQUIVO
mola <- read.csv('rigidez.csv', sep = ";")
View (mola)


# AN?LISE DAS VARI?VEIS (COLUNAS)

# Renomeando vari?veis (colunas)
# Uma coluna
mola <- rename(mola, compr_inicial = Lo)
View(mola)

# V?rias colunas
mola <- rename(mola, compr_final = L,deformacao = x,rigidez = K)
View(mola)

# EXCLUIR UMA COLUNA (POR NOME)
mola2 <- mola
mola2$compr_inicial <- NULL
View (mola2)

# EXCLUIR UMA COLUNA (POR N?MERO)
mola3 <- mola
mola3 <- select(mola3, -c(3))
View (mola3)



# AN?LISE DOS REGISTROS (LINHAS)
# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(mola2, function(x) sum(is.na(x)))
sapply(mola2, function(x) sum(is.nan(x)))

#Substituir e excluir valores missing
if(!require(tidyr)) install.packages("tidyr")
library(tidyr)

#mola2 <- mola2 %>% mutate_all(replace_na, 3422)
#View(mola2)

# EXCLUINDO VALORES MISSING

mola2 <- drop_na(mola2, deformacao)
View(mola2)
# OU
# mola2 <- mola2[!is.na(mola2$deformacao), ]



# AN?LISE DE OUTLIERS
boxplot(mola2$forca)
boxplot(mola2$deformacao)
boxplot(mola2$rigidez)

# Tratando os outliers

# Identificando os outliers
if(!require(rstatix)) install.packages("rstatix") # estat?stica
library(rstatix)

mola2 %>% identify_outliers(deformacao)


# COM PLOTLY
if(!require(plotly)) install.packages("plotly")
library(plotly)

plot_ly(mola2, y = mola2$deformacao, type = "box")

# Excluindo os outliers
outliers <- c(boxplot.stats(mola2$deformacao)$out)
mola3 <- mola2[-c(which(mola2$deformacao %in% outliers)),  ]


boxplot(mola3$deformacao)
plot_ly(mola3, y = mola3$deformacao, type = "box")


# An?lise da classifica??o dos atributos
glimpse(mola3)                              

mola3$cargas <- as.factor(mola3$cargas)









# AN?LISE DA CORRELA??O LINEAR

# AN?LISE GR?FICA DA CORRELA??O

plot(mola3$deformacao, mola3$forca)


# NORMALIDADE
# Existem 4 testes de normalidade principais (num?ricos) e dois testes gr?ficos:
# Shapiro-Wilk (limite de 5000 amostras)
# Anderson-Darling
# Kolmogorov_Smirnov
# Cramer-Von Mises
# Histograma
# QQplot


#Histograma
hist(mola3$forca, probability=T, col="blue")
lines(density(mola3$forca) , col="red")

hist(mola3$deformacao, probability=T, col="yellow")
lines(density(mola3$deformacao) , col="red")


# QQPLOT (GR?FICO DE DISTRIBUI??O NORMAL)
qqnorm(mola3$forca)
qqline(mola3$forca)

qqnorm(mola3$deformacao)
qqline(mola3$deformacao)

# Shapiro-Wilk
# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05

shapiro.test(mola3$forca)
shapiro.test(mola3$deformacao)

if(!require(nortest)) install.packages("nortest")
library(nortest)

# Anderson-Darling
ad.test(mola3$forca)
ad.test(mola3$deformacao)

# Lilliefors (Kolmogorov_Smirnov)
lillie.test(mola3$forca)
lillie.test(mola3$deformacao)

#Cramer-Von Mises
cvm.test(mola3$forca)
cvm.test(mola3$deformacao)


# Correla??o Linear:

# Pearson (distribui??o normal)
# Spearman (distribui??o n?o normal)
# Kendall (distribui??o n?o normal com quantidade pequena de amostras)

# Ho = n?o h? corrrela??o linear: p > 0,05
# Ha = existe correla??o linear: p <= 0,05
cor.test(mola3$deformacao, mola3$forca, method = "pearson")


# Matrizes de correla??o

if(!require(corrplot)) install.packages("corrplot")
library(corrplot) # gr?fico de correla??o

#Alterando posi??o de uma coluna:
mola3 <- mola3 %>% relocate(forca, .after = compr_final)

matriz_corr <- cor(mola3[2:5], method = "pearson")
View(matriz_corr)

corrplot(matriz_corr, method = "color")

corrplot(matriz_corr, method="color", 
         type="full", order="original", 
         addCoef.col = "black", # adiciona valores ? matriz
         tl.col="black", tl.srt=45, # cor e rota??o do nome das vari?veis
)









# MODELO DE REGRESSaO LINEAR:

# Analise dos residuos (valor previsto - valor calculado)

modelo_regressao <- lm(forca ~ deformacao, mola3)

## Analise grafica:
plot(modelo_regressao) # vai plotar 4 graficos (tem que dar enter para passar o gr?fico).

# Grafico 1: Linearidade.
# Grafico 2: Normalidade dos res?duos.
# Grafico 3: Homocedasticidade.
# Grafico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam n?o ter outliers.


# Teste de normalidade dos res?duos
# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(modelo_regressao$residuals)

if(!require(nortest)) install.packages("nortest")
library(nortest)
lillie.test(modelo_regressao$residuals)

# Outliers (Entre -3 e 3)
summary(rstandard(modelo_regressao))


# Homocedasticidade (teste Breuschen-Pagan - somente para res?duos normais)

if(!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Ho = existe homocedasticidade : p > 0.05
# Ha = n?o existe homocedasticidade : p <= 0.05

bptest(modelo_regressao)

# RESUMO E VALIDA??O DO MODELO

summary (modelo_regressao)

# Estat?stica t(vari?vel deformacao): 
# Ho = coeficiente igual a zero : p > 0,05 (coeficiente n?o validado)
# Ha = coeficiente diferente de zero: p <= 0,05 (coeficiente validado)

# Estat?stica F: 
# Ho = modelo previsor ? igual com ou sem vari?vel independente: p > 0,05 (reprovado modelo de regress?o)
# Ha = modelo previsor ? diferente com ou sem vari?vel independente: p <= 0,05 (aprovado modelo de regress?o)

# Equa??o: For?a = 0,0436 + 30,23257.deforma??o


# Representa??o gr?fica
if(!require(ggplot2)) install.packages("ggplot2") #gr?fico 
if(!require(ggpubr)) install.packages("ggpubr") #equa??o da reta no gr?fico

library(ggplot2)
library(ggpubr)

ggplot(data = mola3, mapping = aes(x = deformacao, y = forca)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label= paste(..eq.label..,..adj.rr.label..,sep = "*plain(\",\")~~")),
                        label.x = 0.03, label.y = 0.5) +
  theme_classic()




