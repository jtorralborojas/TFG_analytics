###TRABAJO FINAL DE GRADO - BUSINESS ANALYTICS - JUAN TORRALBO ROJAS###

##BIBLIOTECAS UTILIZADAS##

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("lattice")

library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(lattice)


##MANEJO DE DATOS##

#Primero establecemos el directorio
setwd("~/Desktop/E3 Analytics/5to año/TFG Analytics/")

#Importamos los datos
contenedores_previo <- read_excel("tfganalytics_contenedores.xlsx")
historico_contenedores <- read_excel("tfganalytics_historico_contenedores.xlsx")
View(contenedores_previo)
View(historico_contenedores)

#Se unen las tablas a partir del código RFID
contenedores <- inner_join(contenedores_previo, historico_contenedores, by = "RFID")

#Recopilamos la información proporcionada por los datos
str(contenedores)
head(contenedores)
summary(contenedores)

#Borramos la variable RFID ya que no va a ser significativa en nuestro estudio
contenedores$RFID<-NULL
contenedores<-contenedores

contenedores_previo$RFID<-NULL
contenedores_previo<-contenedores_previo

#Se omiten los datos nulos
contenedores<-na.omit(contenedores)

#Realizamos histogramas para entender los datos de cada columna y las matrices de correlación
hist(contenedores$LATITUD)
hist(contenedores$LONGITUD)
hist(contenedores$CAPACIDAD)
hist(contenedores$DENSIDAD_POB)
hist(contenedores$VIVIENDA_PPAL)
hist(contenedores$TIEMPO_MEDIO_RECOGIDA)
hist(contenedores$BASURA_EN_2019)
hist(contenedores$BASURA_JUL_2019)
hist(contenedores$BASURA_EN_2020)
hist(contenedores$BASURA_JUL_2020)
hist(contenedores$BASURA_EN_2021)
hist(contenedores$BASURA_JUL_2021)
hist(contenedores$BASURA_EN_2022)
hist(contenedores$BASURA_JUL_2022)
hist(contenedores$BASURA_EN_2023)

ggcorrplot(contenedores, nbreaks=10)
ggcorr(contenedores_previo, nbreaks=10)

##REGRESIÓN LINEAL DESCRIPTIVA##

reglineal_desc<-lm(formula = TIEMPO_MEDIO_RECOGIDA ~ LATITUD + LONGITUD + CAPACIDAD + DENSIDAD_POB + VIVIENDA_PPAL + BASURA_EN_2019 + BASURA_JUL_2019 + BASURA_EN_2020 + BASURA_JUL_2020 + BASURA_EN_2021 + BASURA_JUL_2021 + BASURA_EN_2022 + BASURA_JUL_2022 + BASURA_EN_2023, data = contenedores)
summary(reglineal_desc)

##REGRESIÓN STEPWISE##
regstepwise<-stepAIC(reglineal_desc, direction = "both", trace = TRUE)
summary(regstepwise)

##PARTICIÓN DE DATOS##

#Partición entre testset y trainset para realizar las predicciones del modelo
set.seed(100)
RNGkind("Super","Inversion")

trainIndex <- createDataPartition(contenedores$TIEMPO_MEDIO_RECOGIDA, 
                                  p = 0.8,    
                                  list = FALSE,
                                  times = 1)    

contenedores_training <- contenedores[trainIndex,]
contenedores_test <- contenedores[-trainIndex,]

#Observamos el resultado de cada una de las particiones
str(contenedores_training)
summary(contenedores_training)

str(contenedores_test)   
summary(contenedores_test)       

##REGRESIÓN LINEAL PREDICTIVA##

ctrl <- trainControl(method = "repeatedcv",
                     number = 10, 
                     repeats= 3)

set.seed(76) 
RNGkind("Super","Inversion")

#Entrenamiento del modelo de regresión lineal
reglineal <- train(form = TIEMPO_MEDIO_RECOGIDA ~., 
                   data = contenedores_training,                
                   method = "glm",                 
                   preProcess = c("center","scale"), 
                   trControl = ctrl,
                   metric= "RMSE")

#Predicciones en el test set
contenedores_test$pred_lineal<-predict(reglineal, newdata=contenedores_test,type="raw")

##KNN##

RNGkind("Super", "Inversion")
set.seed(76)					

ctrl_knn<-trainControl(method = "repeatedcv",		
                 number = 10,		
                 repeats = 5) 

#Entrenamiento del modelo de regresión KNN
knn_training<-train(TIEMPO_MEDIO_RECOGIDA~. , data = contenedores_training, 
                  method = "knn",					              
                  preProcess = c("center","scale"),			
                  trControl = ctrl_knn,				              
                  metric = "RMSE",				            	 
                  tuneLength = 10)				       
knn_training
plot(knn_training)

#Predicciones en el test set
contenedores_test$pred_knn <-predict(knn_training, newdata = contenedores_test)

##DECISION TREES##

RNGkind("Super", "Inversion")
set.seed(76)					

ctrl_arbol<-trainControl(method = "repeatedcv",	
                      number = 10,    
                      repeats = 5,
                      verboseIter = TRUE  )     


arbol_training<-train(x = contenedores_training[,c(1,2,3,4,5,6)], 
                  y = contenedores_training$TIEMPO_MEDIO_RECOGIDA,   
                  method = "rpart",  
                  control = rpart.control(minsplit = 8,minbucket = 8), 
                  parms = list(split = "gini"),      
                  tuneGrid = data.frame(cp = seq(0,0.1,0.005)),
                  trControl = ctrl_arbol,
                  metric="RMSE")

arbol_training
summary(arbol_training)
ggplot(arbol_training)

#Representación del árbol de decisión
plot(arbol_training$finalModel, uniform = TRUE, margin = 0)
text(arbol_training$finalModel, use.n = TRUE, all = TRUE, cex = .8)

##ENSEMBLES##

ctrl_ensemble<-trainControl(method="repeatedcv", number=10, repeats=3, search="random", savePredictions = "final")
algoritmos_lista <- c('glm', 'knn', 'rpart')

RNGkind("Super", "Inversion")
set.seed(76)

ensemble_contenedores <- caretList(TIEMPO_MEDIO_RECOGIDA~., data=contenedores_training, trControl=ctrl_ensemble, preProcess=c("center","scale"),     methodList=algoritmos_lista, tuneLength=10)
resultados_ensemble <- resamples(ensemble_contenedores)
summary(resultados_ensemble)
dotplot(resultados_ensemble)

