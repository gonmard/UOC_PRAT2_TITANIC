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

titanic_train <- read.csv(trainF, header = TRUE)
#Visualizamos la tabla de datos
View(titanic_train)

titanic_test <- read.csv(testF, header = TRUE)
View(titanic_test)
titanic_test.label <- read.csv(genderF, header = TRUE)
titanic_test <- merge(titanic_test, titanic_test.label, by="PassengerId")
View(titanic_test)

titanic_test = titanic_test[,c(1,12,2:11)]
View(titanic_test)


titanic_train <- as.data.table(titanic_train)

titanic_test <- as.data.table(titanic_test)

