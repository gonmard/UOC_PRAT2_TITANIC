library(dplyr)

library(data.table)

library(ggplot2)

library(DT)

library(plotly)

library(mice)

library(stringi)
library(stringr)



## ----Leemos los ficheros----

fold <- "D:/Documentos/UOC/Master/3-TCVD/Practica2/"
trainF <- str_c(fold, "train.csv")
testF <- str_c(fold, "test.csv")
genderF<- str_c(fold, "gender_submission.csv")

titanicTrain <- read.csv(trainF, header = TRUE)
#Visualizamos la tabla de datos
#View(titanicTrain)

titanicTest <- read.csv(testF, header = TRUE)
#View(titanicTest)
titanicTest.label <- read.csv(genderF, header = TRUE)
titanicTest <- merge(titanicTest, titanicTest.label, by="PassengerId")
titanicTest = titanicTest[,c(1,12,2:11)]
#View(titanicTest)
#Coimprobamos si esxiten valores nulos 
summarise_all(titanicTrain, funs(sum(is.na(.))))
summarise_all(titanicTest, funs(sum(is.na(.))))

#Comprobamos los tipos de los campos 
summary(titanicTrain)
summary(titanicTest)


##Factorizamos las columnas numéricas, Pclass y Survived para Train
titanicTrain$Pclass <- as.factor(titanicTrain$Pclass)
class(titanicTrain$Pclass)

titanicTrain$Survived <- as.factor(titanicTrain$Survived)
class(titanicTrain$Survived)


##Factorizamos las columnas numéricas, Pclass y Survived para Test
titanicTest$Pclass <- as.factor(titanicTest$Pclass)
class(titanicTest$Pclass)

titanicTest$Survived <- as.factor(titanicTest$Survived)
class(titanicTest$Survived)

#Como Ticket y Name no son factores, transformamos en tipo Character para Train

titanicTrain$Ticket <- as.character(titanicTrain$Ticket)
class(titanicTrain$Ticket)
titanicTrain$Name <- as.character(titanicTrain$Name)
class(titanicTrain$Name)

#Como Ticket y Name no son factores, transformamos en tipo Character para Test

titanicTest$Ticket <- as.character(titanicTest$Ticket)
class(titanicTest$Ticket)
titanicTest$Name <- as.character(titanicTest$Name)
class(titanicTest$Name)


## --- Cargamos los ficheros en dataframes
titanicTrain <- as.data.frame(titanicTrain)
titanicTest <- as.data.frame(titanicTest)


##Sustituimos los valores nulos en Age por la media en cada caso
titanicTrain <- titanicTrain %>% mutate(Age = replace(Age, which(is.na(Age)), 29.7))
titanicTest <- titanicTest %>% mutate(Age = replace(Age, which(is.na(Age)), 30.27))

##Eliminamos la columna PassangerId ya que no aporta nada al estudio
titanicTrain <- select(titanicTrain, -PassengerId )
titanicTest <- select(titanicTest, -PassengerId )

#Vemos los datos en pantalla
View(titanicTrain)
View(titanicTest)

#Comprobamos el resultado
summary(titanicTrain)
summary(titanicTest)


#Valores extremos en Age y Fare para Train
ex_Age<-mean(titanicTrain$Age,na.rm=T)+3*sd(titanicTrain$Age,na.rm=T)
ex_Age

ex_Fare<-mean(titanicTrain$Fare,na.rm=T)+3*sd(titanicTrain$Fare,na.rm=T)
ex_Fare

#Valoramos los valores extremos para Age y Train

sum(titanicTrain$Age>ex_Age,na.rm=T)

hist(titanicTrain$Age,main="Histograma para Age", xlab="Age",border="blue", col="white")

#Valoramos los valores extremos para Fare y Train
sum(titanicTrain$Fare>ex_Fare,na.rm=T)

hist(titanicTrain$Fare,main="Histograma para Fare", xlab="Fare",border="blue", col="white")

#Valores extremos en Age y Fare para Test
ex_Age<-mean(titanicTest$Age,na.rm=T)+3*sd(titanicTest$Age,na.rm=T)
ex_Age

ex_Fare<-mean(titanicTest$Fare,na.rm=T)+3*sd(titanicTest$Fare,na.rm=T)
ex_Fare

#Valoramos los valores extremos para Age y Test

sum(titanicTest$Age>ex_Age,na.rm=T)

hist(titanicTest$Age,main="Histograma para Age", xlab="Age",border="blue", col="white")

#Valoramos los valores extremos para Fare y Test
sum(titanicTest$Fare>ex_Fare,na.rm=T)

hist(titanicTest$Fare,main="Histograma para Fare", xlab="Fare",border="blue", col="white")
