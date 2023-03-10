#########################################
###   REGRESS?O VETORIAL DE SUPORTE   ###
#########################################

library(dplyr) # Manipula??o de dados

# Dataframe nativo do R
data(iris)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(iris, function(x) sum(is.na(x)))
sapply(iris, function(x) sum(is.nan(x)))

# An?lise da classifica??o dos atributos (vari?veis)
str(iris)

summary(iris)


# Gr?fico de separa??o das esp?cieis
install.packages("scatterplot3d")
library(scatterplot3d)

colors <- c("red", "yellow", "blue") 
colors <- colors[as.numeric(iris$Species)]
# setosa: vermelho
# versicolor: amarelo
# virginica: azul
scatterplot3d(iris[ ,2:4], pch = 15, color=colors)

# SEPARA??O EM DADOS DE TREINO E DADOS DE TESTE

dados_treino <- sample(1:150, 0.7*150)  
dados_teste <- setdiff(1:150, dados_treino)

iris_treino <- iris[dados_treino, ]
iris_teste <- iris[dados_teste, ]




# MODELO DE REGRESS?O VETORIAL DE SUPORTE 
install.packages("e1071")
library (e1071)


# Fun??o de Kernel: radial
modelo1 <- svm(Species ~ . , data = iris_treino)
summary(modelo1)
previsao1 <- predict(modelo1,iris_teste)
table(previsao1,iris_teste$Species)



# Fun??o de Kernel: polinomial
modelo2 <- svm(Species ~ . , data = iris_treino, kernel = "polynomial")
summary(modelo2)
previsao2 <- predict(modelo2,iris_teste)
table(previsao2,iris_teste$Species)



# Fun??o de Kernel: linear
modelo3 <- svm(Species ~ . , data = iris_treino, kernel = "linear")
summary(modelo3)
previsao3 <- predict(modelo3,iris_teste)
table(previsao3,iris_teste$Species)



# Fun??o de Kernel: sigm?ide
modelo4 <- svm(Species ~ . , data = iris_treino, kernel = "sigmoid")
summary(modelo4)
previsao4 <- predict(modelo4,iris_teste)
table(previsao4,iris_teste$Species)

# Conclus?o: O melhor modelo foi o com kernel linear (2 erros)

