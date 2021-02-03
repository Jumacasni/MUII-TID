# Cargo el archivo .xls con los datos de subastas
library(readxl)
ebayActions <- read_excel("~/Documentos/master_ingenieria_informatica/TID/practicas/practica3_clasificacion/eBayAuctions.xlsx")

###############################################################################
# PREPROCESAMIENTO                                                            #
###############################################################################

# Convierto la primera columna en numérica
ebayActions$Category<-as.factor(ebayActions$Category)
ebayActions$Category<-as.numeric(ebayActions$Category)

# Convierto la cuarta columna en numérica
# El lunes es 1 (Mon) y el domingo es 7 (Sun)
ebayActions$endDay<-sapply(as.character(ebayActions$endDay), switch, "Mon" = 1,
                           "Tue" = 2, "Wed" = 3, "Thu" = 4, "Fri" = 5, "Sat" = 6,
                           "Sun" = 7, USE.NAMES = F)

# Discretización de la variable endDay
barplot(table(ebayActions$endDay))
ebayActions$endDay <- cut(ebayActions$endDay, 
                                        breaks = c(-Inf, 2, 4, +Inf), 
                                        labels = c("Alto", "Bajo", "Normal"), 
                                        right = FALSE)
ebayActions$endDay<-sapply(as.character(ebayActions$endDay), switch,
                           "Alto" = 1, "Normal" = 2, "Bajo" = 3, USE.NAMES = F)

# Discretización de la variable Duration
barplot(table(ebayActions$Duration))
ebayActions$Duration <- cut(ebayActions$Duration, 
                          breaks = c(-Inf, 2, 6, 8, +Inf), 
                          labels = c("Baja", "Normal", "Alta", "Normal"), 
                          right = FALSE)
ebayActions$Duration<-sapply(as.character(ebayActions$Duration), switch,
                           "Alta" = 1, "Normal" = 2, "Baja" = 3, USE.NAMES = F)

# Discretización de la variable OpenPriceUS
library(arules)
table(discretize(ebayActions$OpenPriceUS, breaks = 5))
ebayActions <- discretizeDF(ebayActions, methods = list(
  OpenPriceUS = list(method = "frequency", breaks = 5, 
                  labels = c("0.01-1.5", "1.5-3.11", "3.11-7", "7-13.5", "13.5-999"))
),
default = list(method = "none")
)
ebayActions$OpenPriceUS<-as.numeric(ebayActions$OpenPriceUS)

# Selección de características
# Se seleccionan las características 1,3,4,5,10,11 de acuerdo al criterio
# de la sección 2.7 del Informe.pdf entregado en esta práctica
ebay = ebayActions[c(1,3,4,5,9,11)]
# Le quito el signo de interrogación a Competitive
colnames(ebay)[colnames(ebay) == 'Competitive?'] <- 'Competitive'
ebay$Competitive<-as.factor(ebay$Competitive)

###############################################################################
# CLASIFICACIÓN                                                               #
###############################################################################

#####################
# ÁRBOL DE DECISIÓN #
#####################
# PARTICIÓN 1
# Genero el conjunto de entrenamiento (80%) y test(20%)
set.seed(1234)
ind=sample(2,nrow(ebay),replace=TRUE,prob=c(0.8,0.2))
entrenamiento=ebay[ind==1,]
test=ebay[ind==2,]

# Utilizo rpart para generar un árbol de decisión
# Formula = Competitive ~ . -> indica que se intenta clasificar Competitive
# a partir de las otras variables
library("rpart")
arbol1<-rpart(formula = Competitive ~ ., data = entrenamiento)
plot(arbol1) ; text(arbol1)
testpred=predict(arbol1,newdata=test,type="class")
tablepred=table(testpred,test$Competitive)
tablepred
# Genero el vector diagonal y lo sumo
diag=diag(tablepred)
bien=sum(diag)
bien_clasificados=(bien/nrow(test))*100
# Bien y mal clasificados
bien_clasificados
100-bien_clasificados
#Calculo de la curva ROC
m1=predict(arbol1, newdata=test,type = "prob")[,2] 
pred1=prediction(m1,test$Competitive)
perf1=performance(pred1,"tpr","fpr")
plot(perf1, main="Árbol de decisión")
# AUC
as.numeric(performance(pred1, "auc")@y.values)

# PARTCIÓN 2
# Genero el conjunto de entrenamiento (80%) y test(20%)
set.seed(8453)
ind=sample(2,nrow(ebay),replace=TRUE,prob=c(0.8,0.2))
entrenamiento2=ebay[ind==1,]
test2=ebay[ind==2,]

# Utilizo rpart para generar un árbol de decisión
# Formula = Competitive ~ . -> indica que se intenta clasificar Competitive
# a partir de las otras variables
library("rpart")
arbol2<-rpart(formula = Competitive ~ ., data = entrenamiento2)
plot(arbol2) ; text(arbol2)
testpred=predict(arbol2,newdata=test2,type="class")
tablepred=table(testpred,test2$Competitive)
tablepred
# Genero el vector diagonal y lo sumo
diag=diag(tablepred)
bien=sum(diag)
bien_clasificados=(bien/nrow(test))*100
# Bien y mal clasificados
bien_clasificados
100-bien_clasificados
#Calculo de la curva ROC
m2=predict(arbol2, newdata=test2,type = "prob")[,2] 
pred2=prediction(m2,test2$Competitive)
perf2=performance(pred2,"tpr","fpr")
plot(perf2, main="Árbol de decisión")
# AUC
as.numeric(performance(pred2, "auc")@y.values)

# PARTICIÓN 3
# Genero el conjunto de entrenamiento (80%) y test(20%)
set.seed(5555)
ind=sample(2,nrow(ebay),replace=TRUE,prob=c(0.8,0.2))
entrenamiento3=ebay[ind==1,]
test3=ebay[ind==2,]

# Utilizo rpart para generar un árbol de decisión
# Formula = Competitive ~ . -> indica que se intenta clasificar Competitive
# a partir de las otras variables
library("rpart")
arbol3<-rpart(formula = Competitive ~ ., data = entrenamiento3)
plot(arbol3) ; text(arbol3)
testpred=predict(arbol3,newdata=test3,type="class")
tablepred=table(testpred,test3$Competitive)
tablepred
# Genero el vector diagonal y lo sumo
diag=diag(tablepred)
bien=sum(diag)
bien_clasificados=(bien/nrow(test3))*100
# Bien y mal clasificados
bien_clasificados
100-bien_clasificados
#Calculo de la curva ROC
m3=predict(arbol3, newdata=test3,type = "prob")[,2] 
pred3=prediction(m3,test3$Competitive)
perf3=performance(pred3,"tpr","fpr")
plot(perf3, main="Árbol de decisión")
# AUC
as.numeric(performance(pred3, "auc")@y.values)

# Todas las curvas ROC
plot(perf1)
lines(perf2@x.values[[1]], perf2@y.values[[1]], col = 2)
lines(perf3@x.values[[1]], perf3@y.values[[1]], col = 4)

#####################
# KNN               #
#####################
library("kknn")
# PARTICIÓN 3 CON K=3
knn1=kknn(formula=Competitive ~ .,entrenamiento3,test3,k=3)
table1=table(knn1$fit,test3$Competitive)
table1
#Genero el vector diagonal y lo sumo
diag1=diag(table1)
bien1=sum(diag1)
bien_clasificados1=(bien1/nrow(test3))*100
#Bien y mal clasificados
bien_clasificados1
100-bien_clasificados1
#Calculo de la curva ROC
library("ROCR")
m1=knn1$prob[,2]
pred1=prediction(m1,test3$Competitive)
perf1=performance(pred1,"tpr","fpr")
plot(perf1, main="Knn")
# AUC
as.numeric(performance(pred1, "auc")@y.values)

# PARTICIÓN 3 CON K=5
knn2=kknn(formula=Competitive ~ .,entrenamiento3,test3,k=5)
table2=table(knn2$fit,test3$Competitive)
table2
#Genero el vector diagonal y lo sumo
diag2=diag(table2)
bien2=sum(diag2)
bien_clasificados2=(bien2/nrow(test3))*100
#Bien y mal clasificados
bien_clasificados2
100-bien_clasificados2
#Calculo de la curva ROC
m2=knn2$prob[,2]
pred2=prediction(m2,test3$Competitive)
perf2=performance(pred2,"tpr","fpr")
plot(perf2, main="Knn")
# AUC
as.numeric(performance(pred2, "auc")@y.values)

# PARTICIÓN 3 CON K=7
knn3=kknn(formula=Competitive ~ .,entrenamiento3,test3,k=7)
table3=table(knn3$fit,test3$Competitive)
table3
#Genero el vector diagonal y lo sumo
diag3=diag(table3)
bien3=sum(diag3)
bien_clasificados3=(bien3/nrow(test3))*100
#Bien y mal clasificados
bien_clasificados3
100-bien_clasificados3
#Calculo de la curva ROC
m3=knn3$prob[,2]
pred3=prediction(m3,test3$Competitive)
perf3=performance(pred3,"tpr","fpr")
plot(perf3, main="Knn")
# AUC
as.numeric(performance(pred3, "auc")@y.values)

# Todas las curvas ROC
plot(perf1)
lines(perf2@x.values[[1]], perf2@y.values[[1]], col = 2)
lines(perf3@x.values[[1]], perf3@y.values[[1]], col = 4)

#####################
# NAIVE-BAYES       #
#####################
library("e1071")
# PARTICIÓN 1
bayes1=naiveBayes(Competitive ~ .,entrenamiento)
print(bayes1)
predict(bayes1,test)
table1=table(predict(bayes1,test),test$Competitive)
table1
#Genero el vector diagonal y lo sumo
diag1=diag(table1)
bien1=sum(diag1)
bien_clasificados1=(bien1/nrow(test))*100
#Bien y mal clasificados
bien_clasificados1
100-bien_clasificados1
#Calculo de la curva ROC
m1=predict(bayes1, newdata=test,type = "raw")[,2] 
pred1=prediction(m1,test$Competitive)
perf1=performance(pred1,"tpr","fpr")
plot(perf1, main="Naïve Bayes")
# AUC
as.numeric(performance(pred1, "auc")@y.values)

# PARTICIÓN 2
bayes2=naiveBayes(Competitive ~ .,entrenamiento2)
print(bayes2)
predict(bayes2,test2)
table2=table(predict(bayes2,test2),test2$Competitive)
table2
#Genero el vector diagonal y lo sumo
diag2=diag(table2)
bien2=sum(diag2)
bien_clasificados2=(bien2/nrow(test2))*100
#Bien y mal clasificados
bien_clasificados2
100-bien_clasificados2
#Calculo de la curva ROC
m2=predict(bayes2, newdata=test2,type = "raw")[,2] 
pred2=prediction(m2,test2$Competitive)
perf2=performance(pred2,"tpr","fpr")
plot(perf2, main="Naïve Bayes")
# AUC
as.numeric(performance(pred2, "auc")@y.values)

# PARTICIÓN 3
bayes3=naiveBayes(Competitive ~ .,entrenamiento3)
print(bayes3)
predict(bayes3,test3)
table3=table(predict(bayes3,test3),test3$Competitive)
table3
#Genero el vector diagonal y lo sumo
diag3=diag(table3)
bien3=sum(diag3)
bien_clasificados3=(bien3/nrow(test3))*100
#Bien y mal clasificados
bien_clasificados3
100-bien_clasificados3
#Calculo de la curva ROC
m3=predict(bayes3, newdata=test3,type = "raw")[,2] 
pred3=prediction(m3,test3$Competitive)
perf3=performance(pred3,"tpr","fpr")
plot(perf3, main="Naïve Bayes")
# AUC
as.numeric(performance(pred3, "auc")@y.values)

# Todas las curvas ROC
plot(perf1)
lines(perf2@x.values[[1]], perf2@y.values[[1]], col = 2)
lines(perf3@x.values[[1]], perf3@y.values[[1]], col = 4)
