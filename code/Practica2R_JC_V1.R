```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r, include=FALSE}
library(dplyr)

library(data.table)

library(ggplot2)

library(grid)
library(gridExtra)

library(DT)

library(plotly)

library(mice)

library(stringi)
library(stringr)
```

```{r}
train <- read.csv("C:/Users/jordi/Desktop/PRA2TIPOLOGIA/train.xls")
test <- read.csv("C:/Users/jordi/Desktop/PRA2TIPOLOGIA/test.xls")
gender <- read.csv("C:/Users/jordi/Desktop/PRA2TIPOLOGIA/gender-submission.xls")
```

```{r}
train
test
gender
```

Podemos ver que los PassengerId del dataset "test" y del fataset "gender" coinciden y tendremos que combinar ambos para que train y test coincidan en el numero de variables; 


```{r}
test1 <- merge(test, gender, by="PassengerId")
test1 = test1[,c(1,12,2:11)]
```

Cargaremos los ficheros en dataframes: 

```{r}
train <- as.data.frame(train)
test <- as.data.frame(test1)
```



```{r}
colnames(train)
str(train)
colnames(test)
str(test)
```

Podemos ver que ahora ya tenemos dos dataframes para empezar a trabajar con ellos: 

Entonces vamos a aplicar la función Summary para los dataframes para tener una idea general. 

```{r}
summary(train)
summary(test)
```

```{r}
train
test
```


Factorizamos variables numericas Pclass y survived para test y para train; 


```{r}
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Survived <- as.factor(test$Survived)

sapply(train, function(x) class(x))
sapply(test, function(x) class(x))
```

Una vez tenemos las clases de las variables procedemos a estudiar los valores nulos antes de hacer el análisi de estas; 

```{r}
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
```
Podemos ver que sólo presentamos valores nulos en la variable Age y en Fare en caso de test1, por lo que procederemos a sustituir estos valores por la media; 



```{r}
summary(train$Age)
summary(test$Age)

```
```{r}
round(mean(train$Age, na.rm = TRUE),2)
```

Podemos ver que las medias de edades son 29.68 Para train y 30.27 para test, entonces las sustituiremos por estas:


```{r}
train <- train %>% mutate(Age = replace(Age, which(is.na(Age)), round(mean(train$Age, na.rm = TRUE),2)))
test <- test %>% mutate(Age = replace(Age, which(is.na(Age)),round(mean(test$Age, na.rm = TRUE),2)))

```

```{r}
train
test
```


```{r}
summary(test$Fare)
test <- test %>% mutate(Fare = replace(Fare, which(is.na(Fare)), round(mean(test$Fare, na.rm = TRUE),2)))
```

Y ahora haremos lo mismo con la variable Fare, para el único nulo que presenta: 

```{r}
summary(test$Fare)
```

```{r}
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
```

Entonces podemos ver que el cambio se ha hecho correctamente. 


Eliminaremos la variable PassengerId ya que no nos aporta nada: 


```{r}
train <- select(train, -PassengerId )
test <- select(test, -PassengerId )
```

Tambien borraremos el ticket y la cabina, ya que sólo aportan información extra que no necesitamos. 

```{r}
train <- select(train, -Ticket )
test <- select(test, -Ticket )
```

```{r}
train <- select(train, -Cabin )
test <- select(test, -Cabin )
```

Procederemos a analizar las variables para ambos datasets: 

```{r}
boxplot( train$Age, main="Age" )
boxplot( test$Age, main="Age" )

```
valores extremos: 
```{r}
valuesX <- boxplot.stats(train$Age)$out
#miramos valores extremos en la variable Age:
cat("Valores extremos en Age de train:", toString(valuesX), "\n" )
valuesY <- boxplot.stats(test$Age)$out
#miramos valores extremos en la variable Age:
cat("Valores extremos en Age de test:", toString(valuesY), "\n" )

```

```{r}

idxAgeX <- which( train$Age %in% valuesX)
idxAgeY <- which( test$Age %in% valuesY)

trainA <- train[-idxAgeX, ]
testA <- test[-idxAgeY,]
#Boxplot final:
boxplot( trainA$Age, main="Age después de eliminar outliers (train)" )
boxplot( testA$Age, main="Age después de eliminar outliers (test)" )


```
Podemos ver que las medias en ambos casos son muy similares. 

```{r}
boxplot( train$Fare, main="Fare (train)" )
boxplot( test1$Fare, main="Fare (test)" )
```
valores extremos: 

```{r}
values1X <- boxplot.stats(train$Fare)$out
#miramos valores extremos en la variable Age:
cat("Valores extremos en Fare (train):", toString(values1X), "\n" )
values1Y <- boxplot.stats(test1$Fare)$out
#miramos valores extremos en la variable Age:
cat("Valores extremos en Fare (test):", toString(values1Y), "\n" )
```

```{r}

idxFareX <- which( data$Fare %in% values1X)
idxFareY <- which( data$Fare %in% values1Y)

FareX <- train[ -idxFareX, ]
FareY <- test1[ -idxFareY, ]

#Boxplot final:
boxplot( FareX$Fare, main="Fare después de eliminar outliers (train)" )
boxplot( FareY$Fare, main="Fare después de eliminar outliers (test)" )
```

Podemos ver que no tiene sentido eliminar los valores extremos dado que por lógica podemos considerar que las tarifas de los pasajeros pueden variar dado que dependiendo de la clase a la que pertenezca puede ser más alto o más bajo...


```{r}
barplot(table(train$Survived), main = "Survived train", ylab = "Frecuency", xlab = "Survived")
barplot(table(test$Survived), main = "Survived test", ylab = "Frecuency", xlab = "Survived")
```

El numero de registros para ambos dataframes es diferente, dado que tenemos más registros en train que en test, pero la clase mayoritaria en ambos, es la que no sobrevivió a la catastrofe del Titanic. Miraremos por clase también, qué clase fue la mayoritaria de pasajeros: 


```{r}
barplot(table(train$Pclass), main = "Pclass train", ylab = "Frecuency", xlab = "Pclass")
barplot(table(test$Pclass), main = "Pclass test", ylab = "Frecuency", xlab = "Pclass")
```

La clase mayoritaria de passajeros en el Titanic, fue la 3ra, como es lógico, la mayoria de clientes que entraron en el titanic lo hicieron mediante la clase más económica. 


Nos interesa ver la relación de la clase survived con el resto de variables, como la de la clase a la que pertenecian, la edad y el sexo: 

Para train; 

```{r}
grid.newpage()
plotbyClass_train<-ggplot(train,aes(Pclass,fill=Survived))+geom_bar() +labs(x="Class", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by PClass(train)")

plotbyAge_train<-ggplot(train,aes(Age,fill=Survived))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Age (train)")

plotbySex_train<-ggplot(train,aes(Sex,fill=Survived))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Sex (train)")

plotbyEmbarked_train<-ggplot(train,aes(Embarked,fill=Survived))+geom_bar() +labs(x="Embarked", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Embarked (train)")

grid.arrange(plotbyClass_train,plotbyAge_train,plotbySex_train, plotbyEmbarked_train,ncol=2)
```

Podemos ver que en Pclass, la clase con más víctimas es la mayoritaria, es decir, la clase más economica, pero también podemos ver que la proporcion de victimas y supervivientes es la que más se ve afectada... ya que más del 50% de los pasajeros de la 3a clase murieron. 
Por lo que hace a la variable Sex, la proporción con más supervivientes es la de mujeres mientras que la proporcion de supervivientes masculinos es todo lo contrario. 
Por puerto de embarcación, podemos ver que pasa lo mismo que con la variable PClass, que tenemos un puerto, concretamente el de Southampton, con la mayor parte de pasajeros, entonces la proporción se acerca a lo que se ha visto anteriormente con la 3ra Clase. 
Finalmente tenemos a la variable Age, que tendremos que agrupar en 3 segmentos, separandola por rangos de Edad, lo cambiaremos por child, adult y senior: 

``` {r}

train$Age[train$Age <=18] = "Child"
train$Age[(train$Age > 18) & (train$Age <=60) & (train$Age != "Child")] = "Adult"
train$Age[(train$Age != "Child") & (train$Age != "Adult")] = "Senior"
train$Age = as.factor(train$Age)
          
```


```{r}
grid.newpage()

NewplotbyAge_train<-ggplot(train,aes(Age,fill=Survived))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Age (train)")


grid.arrange(NewplotbyAge_train,ncol=2)
```


Entonces podemos ver perfectamente que la mayoria de pasajeros eran Adultos y la mayoria de ellos fueron victimas, mientras que en pasajeros entro 0-18 años, la proporción se ve más repartida que en Adult. Por último tenemos a Senior, que son minoria, podemos ver que la proporcion de victimas és mayor a la de supervivientes. 

Una vez mostradas las gráficas, podemos ver las proporciones: 

```{r}
table_SexSurvived <- table(train$Sex, train$Survived)
prop.table(table_SexSurvived, margin = 1)
```

Podemos ver que un 74% de la población de mujeres sobrevivió a la catastrofe mientras que sólo el 18% de la población masculina lo hizo... 

Sguiremos con el estudio de la clase: 

```{r}
table_PclassSurvived <- table(train$Pclass,train$Survived)
prop.table(table_PclassSurvived, margin = 1)
```

Como se ha comentado anteriormente, podemos ver que el  76% de los pasajeros pertenecientes a la 3a clase, fueron victimas. Podemos ver que mientras subimos de clase (de 3a a 1a) vamos encontrando una mayor tasa de supervivéncia que podríamos decir que esta se debe a que cada vez que subimos de clase, nos encontramos con menos pasajeros. 

```{r}
table_AgeSurvived <- table(train$Age,train$Survived)
prop.table(table_AgeSurvived, margin = 1) 
```

Podemos ver que los niños sobrevivieron en un 50% más o menos, debido a que se ha elegido la edad de 18 años como niño también  y podríamos ver, en los siguientes cálculos que si lo cambiamos a niño hasta la edad de 15, que podría cambiar:

```{r}
trainXAge <- read.csv("C:/Users/jordi/Desktop/PRA2TIPOLOGIA/train.xls")
trainXAge$Age[trainXAge$Age <=15] = "Child"
trainXAge$Age[(trainXAge$Age > 15) & (trainXAge$Age <=60) & (trainXAge$Age != "Child")] = "Adult"
trainXAge$Age[(trainXAge$Age != "Child") & (trainXAge$Age != "Adult")] = "Senior"
trainXAge$Age = as.factor(trainXAge$Age)

table_XAgeSurvived <- table(trainXAge$Age,trainXAge$Survived)
prop.table(table_XAgeSurvived, margin = 1) 
```

Podemos ver que éste cambio representa un incremento irrelevante, dado que se pensaba que incrementariamos mucho más la tasa de supervivéncia de la clase niños, por lo que sólo nos fijaremos en lo representado anteriormente. 

Y lo combinaremos con la clase, la edad (child -> <=18) y la supervivéncia. 

```{r}
tablebyAClass <- table(train$Age,train$Survived,train$Pclass)
tablebyAClass
prop.table(tablebyAClass, margin = 1) 
```

Como se ha visto anteriormente, la 3a clase es la que más víctimas tiene, por lo que si nos fijamos en los niños, podemos ver que cada vez que aumentamos la clase, tenemos que la supervivencia de los incrementa de manera significativa. 

Para test: 

```{r}
grid.newpage()
plotbyClass_test<-ggplot(test,aes(Pclass,fill=Survived))+geom_bar() +labs(x="Class", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by PClass(train)")

plotbyAge_test<-ggplot(test,aes(Age,fill=Survived))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Age (train)")

plotbySex_test<-ggplot(test,aes(Sex,fill=Survived))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Sex (train)")

plotbyEmbarked_test<-ggplot(test,aes(Embarked,fill=Survived))+geom_bar() +labs(x="Embarked", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Embarked (train)")

grid.arrange(plotbyClass_test,plotbyAge_test,plotbySex_test, plotbyEmbarked_test,ncol=2)
```

``` {r}

test$Age[test$Age <=18] = "Child"
test$Age[(test$Age > 18) & (test$Age <=60) & (test$Age != "Child")] = "Adult"
test$Age[(test$Age != "Child") & (test$Age != "Adult")] = "Senior"
test$Age = as.factor(test$Age)
          
```

Volvemos a hacer lo mismo ara ver los gráficos, con las Clases de edades "actulizadas": 

```{r}
grid.newpage()
plotbyClass_test<-ggplot(test,aes(Pclass,fill=Survived))+geom_bar() +labs(x="Class", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by PClass(train)")

plotbyAge_test<-ggplot(test,aes(Age,fill=Survived))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Age (train)")

plotbySex_test<-ggplot(test,aes(Sex,fill=Survived))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Sex (train)")

plotbyEmbarked_test<-ggplot(test,aes(Embarked,fill=Survived))+geom_bar() +labs(x="Embarked", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Embarked (train)")

grid.arrange(plotbyClass_test,plotbyAge_test,plotbySex_test, plotbyEmbarked_test,ncol=2)
```

Podemos ver que es similar al dataset de train, pero con la diferencia de que los hombres fueron víctima en su totalidad... Veremos ahora las probabilidades: 

```{r}
table_SexSurvivedTest <- table(test$Sex, test$Survived)
prop.table(table_SexSurvivedTest, margin = 1)
```

Podemos ver que la totalidad de mujeres sobrevivió en el dataset de test y la totalidad de hombres murio. 

```{r}
table_PclassSurvivedTest <- table(test$Pclass,test$Survived)
prop.table(table_PclassSurvivedTest, margin = 1)
```

En test, podemos ver que siempre tenemos mayoria de victimas y una proporción muy similar entre la 3a y 2a clase. Pero siempre tenemos mayoria de victimas, exceptuando la primera clase, que la tasa de víctimas es muy poco superior a la de supervivientes. 

```{r}
table_AgeSurvivedTest <- table(test$Age,test$Survived)
prop.table(table_AgeSurvivedTest, margin = 1) 
```

Podemos ver que en test las proporciones son similares a lo que podemos ver en train

```{r}
tablebyAClassTest <- table(test$Age,test$Survived,test$Pclass)

prop.table(tablebyAClassTest, margin = 1) 
```
