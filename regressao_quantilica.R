################################
###   REGRESS?O QUANT?LICA   ###
################################

library(dplyr) 


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

#ABRIR ARQUIVO

library(readxl)

gasto <- read_xlsx("gasto_almoco.xlsx")


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(gasto, function(x) sum(is.na(x)))
sapply(gasto, function(x) sum(is.nan(x)))

# An?lise da classifica??o dos atributos
glimpse(gasto)



# AN?LISE DA CORRELA??O LINEAR

# AN?LISE GR?FICA DA CORRELA??O

plot(gasto$dia, gasto$gasto_acum_reais)


# NORMALIDADE
#Ho = distribui??o normal : p > 0.05
#Ha = distribui??o != normal : p <= 0.05
shapiro.test(gasto$gasto_acum_reais)

# QQPLOT (GR?FICO DE DISTRIBUI??O NORMAL)
qqnorm(gasto$gasto_acum_reais)
qqline(gasto$gasto_acum_reais)


# Correla??o Linear:

# Ho = n?o h? corrrela??o linear: p > 0,05
# Ha = existe correla??o linear: p <= 0,05
cor.test(gasto$gasto_acum_reais, gasto$dia, method = "spearman")




# MODELO DE REGRES?O LINEAR:

# An?lise dos res?duo (valor previsto - valor esperado)

modelo_regressao <- lm(gasto_acum_reais ~ dia, gasto)

## An?lise gr?fica:
plot(modelo_regressao) 

# Gr?fico 1: Linearidade.
# Gr?fico 2: Normalidade.
# Gr?fico 3: Homocedasticidade.
# Gr?fico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam n?o ter outliers.


# Teste de normalidade dos res?duos
# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(modelo_regressao$residuals)


# Outliers (Entre -3 e 3)
summary(rstandard(modelo_regressao))


# Homocedasticidade (teste Breuschen-Pagan - somente para res?duos normais)

library(lmtest)

# Ho = existe homocedasticidade : p > 0.05
# Ha = n?o existe homocedasticidade : p <= 0.05

bptest(modelo_regressao)


###  AN?LISE PELO MODELO DE REGRESS?O LINEAR REPROVADO
###  Presen?a de outliers e heterocedascidade







### REGRESS?O QUANT?LICA

install.packages("quantreg")
library("quantreg")

modelo1 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.25)
summary(modelo1)
# Equa??o: gasto_acum = -21.71429+30.92857*dia



modelo2 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.5)
summary(modelo2)
# Equa??o: gasto_acum = -8.03125+30.78125*dia



modelo3 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.75)
summary(modelo3)
# Equa??o: gasto_acum = 13.40741+30.48148*dia



modelo4 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.85)
summary(modelo4)
# Equa??o: gasto_acum = -5.4+31.4*dia



modelo5 = rq (gasto_acum_reais ~ dia, gasto, tau = seq (0.25, 1, by = 0.25))
summary(modelo5)


# Comparando com a regress?o linear
summary(modelo_regressao)
# Equa??o: gasto_acum = -17.8542+31.1984.dia

# Ocorreram pequenas diferen?as nos coeficientes angulares.




