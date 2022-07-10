#Pacotes para regressão multinomial
library(stats4) 
library(splines) 
#Para baixar o VGAM é necessário instalar os dois pacotes acima
install.packages("VGAM")
library(VGAM)

#Import dos dados do repositório da UCI com dados de análise de flores
iris.uci <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"),
                     header=FALSE)

#Determina o nome das colunas do dataset, poderia ser feito no comando acima também
colnames(iris.uci)<-c("sepal_length","sepal_width","petal_length","petal_width","iris_species")

#traz algumas informações básicas do dataset como MIN, MAX, MEDIANA, MEDIA, QUARTIS
summary(iris.uci)


############### Análise Exploratória dos dados ###############

#Imprime os 10 primeiros registros (n) 
head(iris.uci, n = 10)

#Checagem da importação e tipo de dados de cada coluna
str(iris.uci)

# Neste caso as 4 colunas estão como numéricas e a ultima como char para categorização
boxplot(iris.uci$sepal_length,iris.uci$sepal_width, iris.uci$petal_length, iris.uci$petal_width,
        names  = c("Sepal Length","Sepal Width", "Petal Length", "Pepal Width"),
        xlab = "Variáveis observadas",
        ylab = "Length",
        border = "black",
        col = c("red", "red", "blue", "blue"),
        main = "Boxplot para distribuição das variáveis"
        )

#Boxplot de todas as variáveis divididos pela espécie observada
layout(matrix(1:4, ncol = 2))
boxplot(iris.uci$petal_length ~ iris.uci$iris_species, 
        main = "Petal Length",
        xlab = 'Espécies observadas',
        ylab = "Tamanho",
        col = c("red","blue","green"))

boxplot(iris.uci$sepal_length ~ iris.uci$iris_species, 
        main = "Sepal Length",
        xlab = 'Espécies observadas',
        ylab = "Tamanho",
        col = c("red","blue","green"))

boxplot(iris.uci$petal_width ~ iris.uci$iris_species, 
        main = "Petal Width",
        xlab = 'Espécies observadas',
        ylab = "Tamanho",
        col = c("red","blue","green"))

boxplot(iris.uci$sepal_width ~ iris.uci$iris_species, 
        main = "Sepal Width",
        xlab = 'Espécies observadas',
        ylab = "Tamanho",
        col = c("red","blue","green"))

####### Preparação para método de ML #######

#Seed aleatória definida --> meu RA
set.seed(1234)

#Separação dos dados de treinamento e teste
idx <- sample(1:nrow(iris.uci), size = round(0.5 * nrow(iris.uci)))
training <- iris.uci[idx,]
test <- iris.uci[-idx,]

test<- test[,1:3]
####### Aplicação da Regressão Logística Multinomial ########

#Aplicando o modelo - Regressão Logística
model  <- vglm(iris.uci$iris_species ~ iris.uci$sepal_length + iris.uci$sepal_width + iris.uci$petal_width + iris.uci$petal_length,
            data = training,
            family = multinomial)

#Resultados principais do modelo
summary(model)

#prevendo novos valores
prob_test <- predictvglm(model, newdata = test, type = 'response')

#Coloca o índice da probabilidade maior como sendo o valor final
predictions <- apply(prob_test, 1, which.max)

#Associa o valor original com a espécie
predictions[which(predictions=="1")] <- levels(iris.uci$iris_species)[1]
predictions[which(predictions=="2")] <- levels(iris.uci$iris_species)[2]
predictions[which(predictions=="3")] <- levels(iris.uci$iris_species)[3]

#Tabela de  confusão final
table(iris.uci$iris_species, predictions) 


