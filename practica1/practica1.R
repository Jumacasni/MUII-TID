# Cargo el archivo .xls con los datos de accidentes
library(readxl)
accidentes <- read_excel("~/Documentos/master_ingenieria_informatica/TID/practicas/practica1_preparacion/accidentes.xlsx")
View(accidentes)
accidentes_sin_imputar=accidentes[c(1,2,4,6:9,11,12,14:17,19,21,23,25,27,29,31:33,35,36,40)]
accidentes_imputados=accidentes[c(1,3,5,6:8,10,11,13,14:16,18,20,22,24,26,28,30,31:32,34,35,36,40)]
# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
set.seed(1234)
ind=sample(2,nrow(accidentes),replace=TRUE,prob=c(0.8,0.2))
train=accidentes[ind==1,]
test=accidentes[ind==2,]

# Cargo la librería C5.0
library(C50)
# Hago un subset de entrenamiento y test con las variables deseadas (sin imputar)
train_frame=train[c(1,2,4,6:9,11,12,14:17,19,21,23,25,27,29,31:33,35,36,40)]
test_frame=test[c(1,2,4,6:9,11,12,14:17,19,21,23,25,27,29,31:33,35,36,40)]

# Hago otro subset pero con las variables que acaban en _I (imputadas)
train_frame_i=train[c(1,3,5,6:8,10,11,13,14:16,18,20,22,24,26,28,30,31:32,34,35,36,40)]
test_frame_i=test[c(1,3,5,6:8,10,11,13,14:16,18,20,22,24,26,28,30,31:32,34,35,36,40)]

##############################################################################
# APARTADO 1.a)                                                              #
# Ejecutar el algoritmo de prueba para clasificación (C4.5) y estudiar cómo  #
# divide las características numéricas en los árboles de decisión aprendidos.#
##############################################################################
train_frame_i$ACCIDENTE<-as.factor(train_frame_i$ACCIDENTE)
# Se crea el modelo
model <- C5.0(train_frame_i[-25],train_frame_i$ACCIDENTE)
summary(model)
predictions <- predict(model, test_frame_i, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_frame_i$ACCIDENTE)

##############################################################################
# APARTADO 1.b)                                                              #
# Aplicar el algoritmo de discretización top-down CAIM sobre las             #
# características numéricas                                                  #
##############################################################################

library(discretization)
# accidentes_imputados tiene como clases "tbl_df tbl data.frame",
# lo convierto a solo "data.frame
accidentes_imputados_data_frame <- as.data.frame.data.frame(accidentes_imputados)
# Aplico el algoritmo top-down CAIM sobre las variables numéricas
caim=disc.Topdown(accidentes_imputados_data_frame, method=1)
# Intervalos
caim$cutp
# Datos discretizados
accidentes_imputados_disc = caim$Disc.data

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_imputados_disc),replace=TRUE,prob=c(0.8,0.2))
train_imputados_disc=accidentes_imputados_disc[ind==1,]
test_imputados_disc=accidentes_imputados_disc[ind==2,]

# Se crea el modelo
train_imputados_disc$ACCIDENTE<-as.factor(train_imputados_disc$ACCIDENTE)
model <- C5.0(train_imputados_disc[-25],train_imputados_disc$ACCIDENTE)
summary(model)
predictions <- predict(model, test_imputados_disc, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_imputados_disc$ACCIDENTE)

##############################################################################
# APARTADO 1.c)                                                              #
# Discretización propia                                                      #
##############################################################################
library(arules)

accidentes_imputados_disc <- accidentes_imputados_data_frame

# HOUR_I
hour_i <- accidentes_imputados_disc[,3]
hist(hour_i, breaks = 5, main = "HOUR_I")
accidentes_imputados_disc$HOUR_I <- cut(accidentes_imputados_disc$HOUR_I, 
                                        breaks = c(-Inf, 5, 20, +Inf), 
                                        labels = c("no", "hora punta", "no"), 
                                        right = FALSE)

# SPDLIM_H
spdlim_h <- accidentes_imputados_data_frame[,17]
hist(spdlim_h, breaks = 4, main = "SPDLIM_H")
table(discretize(spdlim_h, breaks = 4))
accidentes_imputados_disc <- discretizeDF(accidentes_imputados_disc, methods = list(
      SPDLIM_H = list(method = "frequency", breaks = 4, 
                      labels = c("muy baja", "baja", "normal", "alta"))
      ),
      default = list(method = "none")
    )

# WKDY_I
wkdy_i <- accidentes_imputados_disc[,2]
hist(wkdy_i, breaks = 3, main = "WKDY_I")
accidentes_imputados_disc$WKDY_I <- cut(accidentes_imputados_disc$WKDY_I, 
                                        breaks = c(-Inf, 6, +Inf), 
                                        labels = c("semana", "fin de semana"), 
                                        right = FALSE)
# VEH_INVL
accidentes_imputados_disc$VEH_INVL <- cut(accidentes_imputados_disc$VEH_INVL, 
                                        breaks = c(-Inf, 2, +Inf), 
                                        labels = c("individual", "multiple"), 
                                        right = FALSE)

# NON_INVL
accidentes_imputados_disc$NON_INVL <- cut(accidentes_imputados_disc$NON_INVL, 
                                         breaks = c(-Inf, 1, +Inf), 
                                         labels = c("no", "si"), 
                                         right = FALSE)

# PED_ACC
accidentes_imputados_disc$PED_ACC <- cut(accidentes_imputados_disc$PED_ACC, 
                                          breaks = c(-Inf, 1, +Inf), 
                                          labels = c("no", "si"), 
                                          right = FALSE)

# Se convierte a factor la clase variable y a variable numérica las variables
# que se han discretizado
accidentes_imputados_disc$ACCIDENTE<-as.factor(accidentes_imputados_disc$ACCIDENTE)
accidentes_imputados_disc$HOUR_I<-as.numeric(accidentes_imputados_disc$HOUR_I)
accidentes_imputados_disc$SPDLIM_H<-as.numeric(accidentes_imputados_disc$SPDLIM_H)
accidentes_imputados_disc$WKDY_I<-as.numeric(accidentes_imputados_disc$WKDY_I)
accidentes_imputados_disc$NON_INVL<-as.numeric(accidentes_imputados_disc$NON_INVL)
accidentes_imputados_disc$VEH_INVL<-as.numeric(accidentes_imputados_disc$VEH_INVL)
accidentes_imputados_disc$PED_ACC<-as.numeric(accidentes_imputados_disc$PED_ACC)

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_imputados_disc),replace=TRUE,prob=c(0.8,0.2))
train_acc_imp_disc=accidentes_imputados_disc[ind==1,]
test_acc_imp_disc=accidentes_imputados_disc[ind==2,]

# Se crea el modelo
model <- C5.0(train_acc_imp_disc[-25],train_acc_imp_disc$ACCIDENTE)
summary(model)
predictions <- predict(model, test_acc_imp_disc, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_acc_imp_disc$ACCIDENTE)

##############################################################################
# APARTADO 2.a)                                                              #
# Ejecutar el algoritmo de prueba para clasificación (C4.5) sobre los datos  #
# sin imputar y comprobar el comportamiento de este algoritmo para tratar    #
# implı́citamente datos perdidos                                             .#
##############################################################################
train_frame$ACCIDENTE<-as.factor(train_frame$ACCIDENTE)
# Se crea el modelo
model <- C5.0(train_frame[-25],train_frame$ACCIDENTE)
summary(model)
predictions <- predict(model, test_frame, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_frame$ACCIDENTE)

##############################################################################
# APARTADO 2.b)                                                              #
# Imputar valores perdidos con la media o moda, según proceda, y comprobar   #
# el comportamiento del algoritmo de prueba.                                 #
##############################################################################

accidentes_sin_imputar_data_frame <- as.data.frame.data.frame(accidentes_sin_imputar)

# HOUR
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$HOUR))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$HOUR))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$HOUR=ifelse(
  is.na(accidentes_sin_imputar_data_frame$HOUR),
  as.integer(mean(accidentes_sin_imputar_data_frame$HOUR,na.rm=T)),
  as.integer(accidentes_sin_imputar_data_frame$HOUR))

# MAN_COL
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$MAN_COL))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$MAN_COL))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$MAN_COL=ifelse(
  is.na(accidentes_sin_imputar_data_frame$MAN_COL),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$MAN_COL,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$MAN_COL))

# PROFILE
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$PROFILE))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$PROFILE))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$PROFILE=ifelse(
  is.na(accidentes_sin_imputar_data_frame$PROFILE),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$PROFILE,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$PROFILE))

# REL_JCT
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$REL_JCT))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$REL_JCT))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$REL_JCT=ifelse(
  is.na(accidentes_sin_imputar_data_frame$REL_JCT),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$REL_JCT,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$REL_JCT))

# ALIGN
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$ALIGN))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$ALIGN))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$ALIGN=ifelse(
  is.na(accidentes_sin_imputar_data_frame$ALIGN),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$ALIGN,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$ALIGN))

# SUR_COND
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$SUR_COND))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$SUR_COND))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$SUR_COND=ifelse(
  is.na(accidentes_sin_imputar_data_frame$SUR_COND),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$SUR_COND,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$SUR_COND))

# TRAF_CON
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$TRAF_CON))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$TRAF_CON))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$TRAF_CON=ifelse(
  is.na(accidentes_sin_imputar_data_frame$TRAF_CON),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$TRAF_CON,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$TRAF_CON))

# SPD_LIM
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$SPD_LIM))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$SPD_LIM))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$SPD_LIM=ifelse(
  is.na(accidentes_sin_imputar_data_frame$SPD_LIM),
  as.integer(mean(accidentes_sin_imputar_data_frame$SPD_LIM,na.rm=T)),
  as.integer(accidentes_sin_imputar_data_frame$SPD_LIM))

# LGHT\_COND
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$LGHT_CON))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$LGHT_CON))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$LGHT_CON=ifelse(
  is.na(accidentes_sin_imputar_data_frame$LGHT_CON),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$LGHT_CON,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$LGHT_CON))

# WEATHER
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$WEATHER))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$WEATHER))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$WEATHER=ifelse(
  is.na(accidentes_sin_imputar_data_frame$WEATHER),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$WEATHER,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$WEATHER))

# ALCOHOL
# compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame$ALCOHOL))
# gráfico de barras
barplot(table(accidentes_sin_imputar_data_frame$ALCOHOL))
# se reemplazan los valores perdidos
accidentes_sin_imputar_data_frame$ALCOHOL=ifelse(
  is.na(accidentes_sin_imputar_data_frame$ALCOHOL),
  as.integer(names(which.max(table(accidentes_sin_imputar_data_frame$ALCOHOL,useNA="no")))),
  as.integer(accidentes_sin_imputar_data_frame$ALCOHOL))

# CLASIFICACIÓN

# HOUR
accidentes_sin_imputar_data_frame$HOUR <- cut(accidentes_sin_imputar_data_frame$HOUR, 
                                        breaks = c(-Inf, 5, 20, +Inf), 
                                        labels = c("no", "hora punta", "no"), 
                                        right = FALSE)

# SPDLIM_H
accidentes_sin_imputar_data_frame <- discretizeDF(accidentes_sin_imputar_data_frame, methods = list(
  SPD_LIM = list(method = "frequency", breaks = 4, 
                  labels = c("muy baja", "baja", "normal", "alta"))
),
default = list(method = "none")
)

# WKDY_I
accidentes_sin_imputar_data_frame$WEEKDAY <- cut(accidentes_sin_imputar_data_frame$WEEKDAY, 
                                        breaks = c(-Inf, 6, +Inf), 
                                        labels = c("semana", "fin de semana"), 
                                        right = FALSE)
# VEH_INVL
accidentes_sin_imputar_data_frame$VEH_INVL <- cut(accidentes_sin_imputar_data_frame$VEH_INVL, 
                                          breaks = c(-Inf, 2, +Inf), 
                                          labels = c("individual", "multiple"), 
                                          right = FALSE)

# NON_INVL
accidentes_sin_imputar_data_frame$NON_INVL <- cut(accidentes_sin_imputar_data_frame$NON_INVL, 
                                          breaks = c(-Inf, 1, +Inf), 
                                          labels = c("no", "si"), 
                                          right = FALSE)

# PED_ACC
accidentes_sin_imputar_data_frame$PED_ACC <- cut(accidentes_sin_imputar_data_frame$PED_ACC, 
                                         breaks = c(-Inf, 1, +Inf), 
                                         labels = c("no", "si"), 
                                         right = FALSE)

# Se convierte a factor la clase variable y a variable numérica las variables
# que se han discretizado
accidentes_sin_imputar_data_frame$ACCIDENTE<-as.factor(accidentes_sin_imputar_data_frame$ACCIDENTE)
accidentes_sin_imputar_data_frame$HOUR<-as.numeric(accidentes_sin_imputar_data_frame$HOUR)
accidentes_sin_imputar_data_frame$SPD_LIM<-as.numeric(accidentes_sin_imputar_data_frame$SPD_LIM)
accidentes_sin_imputar_data_frame$WEEKDAY<-as.numeric(accidentes_sin_imputar_data_frame$WEEKDAY)
accidentes_sin_imputar_data_frame$NON_INVL<-as.numeric(accidentes_sin_imputar_data_frame$NON_INVL)
accidentes_sin_imputar_data_frame$VEH_INVL<-as.numeric(accidentes_sin_imputar_data_frame$VEH_INVL)
accidentes_sin_imputar_data_frame$PED_ACC<-as.numeric(accidentes_sin_imputar_data_frame$PED_ACC)

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_sin_imputar_data_frame),replace=TRUE,prob=c(0.8,0.2))
train=accidentes_sin_imputar_data_frame[ind==1,]
test=accidentes_sin_imputar_data_frame[ind==2,]

# Se crea el modelo
model <- C5.0(train[-25],train$ACCIDENTE)
summary(model)
predictions <- predict(model, test, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test$ACCIDENTE)

##############################################################################
# APARTADO 2.c)                                                              #
# Eliminar las instancias que contienen algún valor perdido y comprobar el   #
# comportamiento del algoritmo de prueba.                                    #
##############################################################################

accidentes_sin_imputar_data_frame <- as.data.frame.data.frame(accidentes_sin_imputar)

# Compruebo que hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame))
# Elimino filas con valores perdidos
accidentes_sin_imputar_data_frame = na.omit(accidentes_sin_imputar_data_frame)
# Compruebo que ya no hay valores perdidos
any(is.na(accidentes_sin_imputar_data_frame))

# CLASIFICACIÓN

# HOUR
accidentes_sin_imputar_data_frame$HOUR <- cut(accidentes_sin_imputar_data_frame$HOUR, 
                                              breaks = c(-Inf, 5, 20, +Inf), 
                                              labels = c("no", "hora punta", "no"), 
                                              right = FALSE)

# SPDLIM_H
accidentes_sin_imputar_data_frame <- discretizeDF(accidentes_sin_imputar_data_frame, methods = list(
  SPD_LIM = list(method = "frequency", breaks = 4, 
                 labels = c("muy baja", "baja", "normal", "alta"))
),
default = list(method = "none")
)

# WKDY_I
accidentes_sin_imputar_data_frame$WEEKDAY <- cut(accidentes_sin_imputar_data_frame$WEEKDAY, 
                                                 breaks = c(-Inf, 6, +Inf), 
                                                 labels = c("semana", "fin de semana"), 
                                                 right = FALSE)
# VEH_INVL
accidentes_sin_imputar_data_frame$VEH_INVL <- cut(accidentes_sin_imputar_data_frame$VEH_INVL, 
                                                  breaks = c(-Inf, 2, +Inf), 
                                                  labels = c("individual", "multiple"), 
                                                  right = FALSE)

# NON_INVL
accidentes_sin_imputar_data_frame$NON_INVL <- cut(accidentes_sin_imputar_data_frame$NON_INVL, 
                                                  breaks = c(-Inf, 1, +Inf), 
                                                  labels = c("no", "si"), 
                                                  right = FALSE)

# PED_ACC
accidentes_sin_imputar_data_frame$PED_ACC <- cut(accidentes_sin_imputar_data_frame$PED_ACC, 
                                                 breaks = c(-Inf, 1, +Inf), 
                                                 labels = c("no", "si"), 
                                                 right = FALSE)

# Se convierte a factor la clase variable y a variable numérica las variables
# que se han discretizado
accidentes_sin_imputar_data_frame$ACCIDENTE<-as.factor(accidentes_sin_imputar_data_frame$ACCIDENTE)
accidentes_sin_imputar_data_frame$HOUR<-as.numeric(accidentes_sin_imputar_data_frame$HOUR)
accidentes_sin_imputar_data_frame$SPD_LIM<-as.numeric(accidentes_sin_imputar_data_frame$SPD_LIM)
accidentes_sin_imputar_data_frame$WEEKDAY<-as.numeric(accidentes_sin_imputar_data_frame$WEEKDAY)
accidentes_sin_imputar_data_frame$NON_INVL<-as.numeric(accidentes_sin_imputar_data_frame$NON_INVL)
accidentes_sin_imputar_data_frame$VEH_INVL<-as.numeric(accidentes_sin_imputar_data_frame$VEH_INVL)
accidentes_sin_imputar_data_frame$PED_ACC<-as.numeric(accidentes_sin_imputar_data_frame$PED_ACC)

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_sin_imputar_data_frame),replace=TRUE,prob=c(0.8,0.2))
train=accidentes_sin_imputar_data_frame[ind==1,]
test=accidentes_sin_imputar_data_frame[ind==2,]

# Se crea el modelo
model <- C5.0(train[-25],train$ACCIDENTE)
summary(model)
predictions <- predict(model, test, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test$ACCIDENTE)

##############################################################################
# APARTADO 2.d)                                                              #
# Eliminar las caracterı́sticas con valores perdidos y comprobar el           #
# comportamiento del algoritmo de prueba.                                    #
##############################################################################

accidentes_sin_imputar_data_frame <- as.data.frame.data.frame(accidentes_sin_imputar)
# Se eliminan las columnas que tienen valores perdidos
accidentes_sin_imputar_data_frame = 
  accidentes_sin_imputar_data_frame[ , colSums(is.na(accidentes_sin_imputar_data_frame)) == 0]

# CLASIFICACIÓN

# WEEKDAY
accidentes_sin_imputar_data_frame$WEEKDAY <- cut(accidentes_sin_imputar_data_frame$WEEKDAY, 
                                                 breaks = c(-Inf, 6, +Inf), 
                                                 labels = c("semana", "fin de semana"), 
                                                 right = FALSE)
# VEH_INVL
accidentes_sin_imputar_data_frame$VEH_INVL <- cut(accidentes_sin_imputar_data_frame$VEH_INVL, 
                                                  breaks = c(-Inf, 2, +Inf), 
                                                  labels = c("individual", "multiple"), 
                                                  right = FALSE)

# NON_INVL
accidentes_sin_imputar_data_frame$NON_INVL <- cut(accidentes_sin_imputar_data_frame$NON_INVL, 
                                                  breaks = c(-Inf, 1, +Inf), 
                                                  labels = c("no", "si"), 
                                                  right = FALSE)

# PED_ACC
accidentes_sin_imputar_data_frame$PED_ACC <- cut(accidentes_sin_imputar_data_frame$PED_ACC, 
                                                 breaks = c(-Inf, 1, +Inf), 
                                                 labels = c("no", "si"), 
                                                 right = FALSE)

# Se convierte a factor la clase variable y a variable numérica las variables
# que se han discretizado
accidentes_sin_imputar_data_frame$ACCIDENTE<-as.factor(accidentes_sin_imputar_data_frame$ACCIDENTE)
accidentes_sin_imputar_data_frame$WEEKDAY<-as.numeric(accidentes_sin_imputar_data_frame$WEEKDAY)
accidentes_sin_imputar_data_frame$NON_INVL<-as.numeric(accidentes_sin_imputar_data_frame$NON_INVL)
accidentes_sin_imputar_data_frame$VEH_INVL<-as.numeric(accidentes_sin_imputar_data_frame$VEH_INVL)
accidentes_sin_imputar_data_frame$PED_ACC<-as.numeric(accidentes_sin_imputar_data_frame$PED_ACC)

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_sin_imputar_data_frame),replace=TRUE,prob=c(0.8,0.2))
train=accidentes_sin_imputar_data_frame[ind==1,]
test=accidentes_sin_imputar_data_frame[ind==2,]

# Se crea el modelo
model <- C5.0(train[-14],train$ACCIDENTE)
summary(model)
predictions <- predict(model, test, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test$ACCIDENTE)

##############################################################################
# APARTADO 3                                                                 #
# Selección de características                                               #
##############################################################################

accidentes_imputados_disc <- accidentes_imputados_data_frame

# HOUR_I
hour_i <- accidentes_imputados_disc[,3]
hist(hour_i, breaks = 5, main = "HOUR_I")
accidentes_imputados_disc$HOUR_I <- cut(accidentes_imputados_disc$HOUR_I, 
                                        breaks = c(-Inf, 5, 20, +Inf), 
                                        labels = c("no", "hora punta", "no"), 
                                        right = FALSE)

# SPDLIM_H
spdlim_h <- accidentes_imputados_data_frame[,17]
hist(spdlim_h, breaks = 4, main = "SPDLIM_H")
table(discretize(spdlim_h, breaks = 4))
accidentes_imputados_disc <- discretizeDF(accidentes_imputados_disc, methods = list(
  SPDLIM_H = list(method = "frequency", breaks = 4, 
                  labels = c("muy baja", "baja", "normal", "alta"))
),
default = list(method = "none")
)

# WKDY_I
wkdy_i <- accidentes_imputados_disc[,2]
hist(wkdy_i, breaks = 3, main = "WKDY_I")
accidentes_imputados_disc$WKDY_I <- cut(accidentes_imputados_disc$WKDY_I, 
                                        breaks = c(-Inf, 6, +Inf), 
                                        labels = c("semana", "fin de semana"), 
                                        right = FALSE)
# VEH_INVL
accidentes_imputados_disc$VEH_INVL <- cut(accidentes_imputados_disc$VEH_INVL, 
                                          breaks = c(-Inf, 2, +Inf), 
                                          labels = c("individual", "multiple"), 
                                          right = FALSE)

# NON_INVL
accidentes_imputados_disc$NON_INVL <- cut(accidentes_imputados_disc$NON_INVL, 
                                          breaks = c(-Inf, 1, +Inf), 
                                          labels = c("no", "si"), 
                                          right = FALSE)

# PED_ACC
accidentes_imputados_disc$PED_ACC <- cut(accidentes_imputados_disc$PED_ACC, 
                                         breaks = c(-Inf, 1, +Inf), 
                                         labels = c("no", "si"), 
                                         right = FALSE)

# Se convierte a factor la clase variable y a variable numérica las variables
# que se han discretizado
accidentes_imputados_disc$ACCIDENTE<-as.factor(accidentes_imputados_disc$ACCIDENTE)
accidentes_imputados_disc$HOUR_I<-as.numeric(accidentes_imputados_disc$HOUR_I)
accidentes_imputados_disc$SPDLIM_H<-as.numeric(accidentes_imputados_disc$SPDLIM_H)
accidentes_imputados_disc$WKDY_I<-as.numeric(accidentes_imputados_disc$WKDY_I)
accidentes_imputados_disc$NON_INVL<-as.numeric(accidentes_imputados_disc$NON_INVL)
accidentes_imputados_disc$VEH_INVL<-as.numeric(accidentes_imputados_disc$VEH_INVL)
accidentes_imputados_disc$PED_ACC<-as.numeric(accidentes_imputados_disc$PED_ACC)

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_imputados_disc),replace=TRUE,prob=c(0.8,0.2))
train_acc_imp_disc=accidentes_imputados_disc[ind==1,]
test_acc_imp_disc=accidentes_imputados_disc[ind==2,]

# Se crea el modelo
model <- C5.0(train_acc_imp_disc[-25],train_acc_imp_disc$ACCIDENTE)
summary(model)
predictions <- predict(model, test_acc_imp_disc, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_acc_imp_disc$ACCIDENTE)

##############################################################################
# Selección de características usando correlaciones                          #
##############################################################################

# Se quita la variable ACCIDENTE que es la variable de clasificación
cor(accidentes_imputados_disc[-25])
# Visualizar la matriz de correlación
library(corrplot)
corrplot(cor(accidentes_imputados_disc[-25]), method="number", is.corr=FALSE)

# Se crea el modelo
# En el modelo
train_acc_imp_disc <- train_acc_imp_disc[c(-7,-18,-21)]
test_acc_imp_disc <- test_acc_imp_disc[c(-7,-18,-21)]
model <- C5.0(train_acc_imp_disc[-22],train_acc_imp_disc$ACCIDENTE)
summary(model)
predictions <- predict(model, test_acc_imp_disc, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_acc_imp_disc$ACCIDENTE)

##############################################################################
# APARTADO 4                                                                 #
# Selección de instancias                                                    #
##############################################################################

accidentes_imputados_disc <- accidentes_imputados_data_frame
accidentes_imputados_disc$ACCIDENTE<-as.factor(accidentes_imputados_disc$ACCIDENTE)
# muestra sin reemplazo
ind_sin_reemplazo<- sample(1:nrow(accidentes_imputados_disc),size=5000,replace=FALSE)
muestra_sin_reemplazo=accidentes_imputados_disc[ind_sin_reemplazo,]
# muestra con reemplazo
ind_con_reemplazo<- sample(1:nrow(accidentes_imputados_disc),size=5000,replace=FALSE)
muestra_con_reemplazo=accidentes_imputados_disc[ind_con_reemplazo,]

# CLASIFICACIÓN SIN REEMPLAZO
# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(muestra_sin_reemplazo),replace=TRUE,prob=c(0.8,0.2))
train_sin_reemplazo=muestra_sin_reemplazo[ind==1,]
test_sin_reemplazo=muestra_sin_reemplazo[ind==2,]

# Se crea el modelo
model <- C5.0(train_sin_reemplazo[-25],train_sin_reemplazo$ACCIDENTE)
summary(model)
predictions <- predict(model, test_sin_reemplazo, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_sin_reemplazo$ACCIDENTE)

# CLASIFICACIÓN CON REEMPLAZO
# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(muestra_con_reemplazo),replace=TRUE,prob=c(0.8,0.2))
train_con_reemplazo=muestra_con_reemplazo[ind==1,]
test_con_reemplazo=muestra_con_reemplazo[ind==2,]

# Se crea el modelo
model <- C5.0(train_con_reemplazo[-25],train_con_reemplazo$ACCIDENTE)
summary(model)
predictions <- predict(model, test_con_reemplazo, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_con_reemplazo$ACCIDENTE)

# MUESTRA SIN REEMPLAZO Y CON DISCRETIZACIÓN
accidentes_imputados_disc <- accidentes_imputados_data_frame
accidentes_imputados_disc$ACCIDENTE<-as.factor(accidentes_imputados_disc$ACCIDENTE)
# muestra sin reemplazo
ind_sin_reemplazo<- sample(1:nrow(accidentes_imputados_disc),size=5000,replace=FALSE)
accidentes_imputados_disc=accidentes_imputados_disc[ind_sin_reemplazo,]

# HOUR_I
hour_i <- accidentes_imputados_disc[,3]
hist(hour_i, breaks = 5, main = "HOUR_I")
accidentes_imputados_disc$HOUR_I <- cut(accidentes_imputados_disc$HOUR_I, 
                                        breaks = c(-Inf, 5, 20, +Inf), 
                                        labels = c("no", "hora punta", "no"), 
                                        right = FALSE)

# SPDLIM_H
spdlim_h <- accidentes_imputados_data_frame[,17]
hist(spdlim_h, breaks = 4, main = "SPDLIM_H")
table(discretize(spdlim_h, breaks = 4))
accidentes_imputados_disc <- discretizeDF(accidentes_imputados_disc, methods = list(
  SPDLIM_H = list(method = "frequency", breaks = 4, 
                  labels = c("muy baja", "baja", "normal", "alta"))
),
default = list(method = "none")
)

# WKDY_I
wkdy_i <- accidentes_imputados_disc[,2]
hist(wkdy_i, breaks = 3, main = "WKDY_I")
accidentes_imputados_disc$WKDY_I <- cut(accidentes_imputados_disc$WKDY_I, 
                                        breaks = c(-Inf, 6, +Inf), 
                                        labels = c("semana", "fin de semana"), 
                                        right = FALSE)
# VEH_INVL
accidentes_imputados_disc$VEH_INVL <- cut(accidentes_imputados_disc$VEH_INVL, 
                                          breaks = c(-Inf, 2, +Inf), 
                                          labels = c("individual", "multiple"), 
                                          right = FALSE)

# NON_INVL
accidentes_imputados_disc$NON_INVL <- cut(accidentes_imputados_disc$NON_INVL, 
                                          breaks = c(-Inf, 1, +Inf), 
                                          labels = c("no", "si"), 
                                          right = FALSE)

# PED_ACC
accidentes_imputados_disc$PED_ACC <- cut(accidentes_imputados_disc$PED_ACC, 
                                         breaks = c(-Inf, 1, +Inf), 
                                         labels = c("no", "si"), 
                                         right = FALSE)

# Se convierte a factor la clase variable y a variable numérica las variables
# que se han discretizado
accidentes_imputados_disc$ACCIDENTE<-as.factor(accidentes_imputados_disc$ACCIDENTE)
accidentes_imputados_disc$HOUR_I<-as.numeric(accidentes_imputados_disc$HOUR_I)
accidentes_imputados_disc$SPDLIM_H<-as.numeric(accidentes_imputados_disc$SPDLIM_H)
accidentes_imputados_disc$WKDY_I<-as.numeric(accidentes_imputados_disc$WKDY_I)
accidentes_imputados_disc$NON_INVL<-as.numeric(accidentes_imputados_disc$NON_INVL)
accidentes_imputados_disc$VEH_INVL<-as.numeric(accidentes_imputados_disc$VEH_INVL)
accidentes_imputados_disc$PED_ACC<-as.numeric(accidentes_imputados_disc$PED_ACC)

# Separo el dataset en conjunto de entrenamiento y test (80% y 20%)
ind=sample(2,nrow(accidentes_imputados_disc),replace=TRUE,prob=c(0.8,0.2))
train_acc_imp_disc=accidentes_imputados_disc[ind==1,]
test_acc_imp_disc=accidentes_imputados_disc[ind==2,]

# Se crea el modelo
model <- C5.0(train_acc_imp_disc[-25],train_acc_imp_disc$ACCIDENTE)
summary(model)
predictions <- predict(model, test_acc_imp_disc, type="class")

# Los resultados en la diagonal principal
# muestran los aciertos del modelo solamente 
table(predictions, test_acc_imp_disc$ACCIDENTE)
