################################
###   REGRESS?O POLINOMIAL   ###
################################

library(dplyr) # Manipula??o de dados

# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("~/Linguagem R/arquivos")


install.packages("readxl")
library(readxl)

vendas <- read_xlsx("comissao.xlsx")


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(vendas, function(x) sum(is.na(x)))
sapply(vendas, function(x) sum(is.nan(x)))

# An?lise da classifica??o dos atributos (vari?veis)
str(vendas)


# Gr?fico para an?lise inicial



# NORMALIDADE
# QQPLOT (GR?FICO DE DISTRIBUI??O NORMAL)
qqnorm(vendas$comissao)
qqline(vendas$comissao)

qqnorm(vendas$quantidade)
qqline(vendas$quantidade)

# Shapiro-Wilk
# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05

shapiro.test(vendas$comissao)
shapiro.test(vendas$quantidade)


# CORRELA??O LINEAR:

# Pearson (distribui??o normal)
# Spearman (distribui??o n?o normal)
# Kendall (distribui??o n?o normal com quantidade pequena de amostras)

# Ho = n?o h? corrrela??o linear: p > 0,05
# Ha = existe correla??o linear: p <= 0,05
cor.test(vendas$comissao, vendas$quantidade, method = "spearman")



# REGRESS?O LINEAR
modelo1 = lm (vendas$comissao ~ vendas$quantidade)
summary(modelo1)


# Representa??o gr?fica
if(!require(ggplot2)) install.packages("ggplot2") #gr?fico 
if(!require(ggpubr)) install.packages("ggpubr") #equa??o da reta no gr?fico

library(ggplot2)
library(ggpubr)

ggplot(data = vendas, mapping = aes(x = quantidade, y = comissao)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label= paste(..eq.label..,..adj.rr.label..,
                            sep = "*plain(\",\")~~")),label.x = 20, label.y = 100) +
  theme_classic()

# Coluna de previs?o
vendas$previsao1 <- modelo1$fitted.values







# REGRESS?O POLINOMIAL - GRAU 2

# y = b0 + b1.x + b2.x^2

x <- vendas$quantidade

var_indep = cbind (x, x ^ 2)

modelo2 = lm (vendas$comissao ~ var_indep)
summary(modelo2)

# Equa??o: Comiss?o = 700 + 25*quantidade + 3*quantidade^2

ggplot (data = vendas) + geom_point (aes (x = x, y = comissao)) +
  geom_line (aes (x = quantidade, y = modelo1 $ fit), col = "blue") +
  geom_line (aes (x = x, y = modelo2 $ fit), col = "red")


vendas$previsao2 <- modelo2$fitted.values






# REGRESS?O POLINOMIAL - GRAU 3

# y = b0 + b1.x + b2.x^2 + b3.x^3

x <- vendas$quantidade

var_indep_3 = cbind (x, x ^ 2, x ^ 3)

modelo3 = lm (vendas$comissao ~ var_indep_3)
summary(modelo3)

# Equa??o: Comiss?o = 700 + 25*quantidade + 3*quantidade^2 - 0*quantidade^3

ggplot (data = vendas) + geom_point (aes (x = x, y = comissao)) +
  geom_line (aes (x = quantidade, y = modelo1 $ fit), col = "blue") +
  geom_line (aes (x = x, y = modelo2 $ fit), col = "red")+
  geom_line (aes (x = x, y = modelo3 $ fit), col = "yellow")


# Coluna previs?o
vendas$previsao3 <- modelo3$fitted.values


