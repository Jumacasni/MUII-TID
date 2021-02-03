# Importar dataset wine
wine
# Renombro las columnas con su nombre
colnames(wine)[colnames(wine) == 'V1'] <- 'Alcohol'
colnames(wine)[colnames(wine) == 'V2'] <- 'Malic_acid'
colnames(wine)[colnames(wine) == 'V3'] <- 'Ash'
colnames(wine)[colnames(wine) == 'V4'] <- 'Alcalinity_of_ash'
colnames(wine)[colnames(wine) == 'V5'] <- 'Magnesium'
colnames(wine)[colnames(wine) == 'V6'] <- 'Total_phenols'
colnames(wine)[colnames(wine) == 'V7'] <- 'Flavanoids'
colnames(wine)[colnames(wine) == 'V8'] <- 'Nonflavanoid_phenols'
colnames(wine)[colnames(wine) == 'V9'] <- 'Proanthocyanins'
colnames(wine)[colnames(wine) == 'V10'] <- 'Color_intensity'
colnames(wine)[colnames(wine) == 'V11'] <- 'Hue'
colnames(wine)[colnames(wine) == 'V12'] <- 'OD280/OD315_of_diluted_wines'
colnames(wine)[colnames(wine) == 'V13'] <- 'Proline'
colnames(wine)[colnames(wine) == 'V14'] <- 'Rating'

###############################################################################
# CLUSTERING JERARQUICO                                                       #
###############################################################################

# Elijo las columas con datos numericos (todas las columnas menos la primera
# que es la solución del cluster)
wine_data=data.frame(wine[,2:14])
distancia=dist(wine_data)
# Aplico clustering jerárquico
clustering_jerarquico=hclust(distancia,method="complete")
clustering_jerarquico
plot(clustering_jerarquico)

rect.hclust(clustering_jerarquico,k=3)
rect.hclust(clustering_jerarquico,k=4)
rect.hclust(clustering_jerarquico,k=7)

###############################################################################
# DETECCION DE OUTLIERS                                                       #
###############################################################################
# Para cada variable, hago el boxplot y el plot de sus datos
# Si una variabl tiene posibles outliers, se analiza y se eliminan

# Malic acid
boxplot(wine_data$Malic_acid)
plot(wine_data$Malic_acid)

# Ash: 
boxplot(wine_data$Ash)
plot(wine_data$Ash)

# Alcalinity of ash: 
boxplot(wine_data$Alcalinity_of_ash)
plot(wine_data$Alcalinity_of_ash)
# Veo cuales son las filas de los outliers
wine_data[wine_data$Alcalinity_of_ash > 3, ]
wine_data[wine_data$Alcalinity_of_ash < 1.5, ]

# Magnesium: 
boxplot(wine_data$Magnesium)
plot(wine_data$Magnesium)
# Veo cuales son las filas de los outliers
wine_data[wine_data$Magnesium >= 30, ]

# Total phenols: 
boxplot(wine_data$Total_phenols)
plot(wine_data$Total_phenols)
# Veo cuales son las filas de los outliers
wine_data[wine_data$Total_phenols >= 140, ]

# Flavanoids: 
boxplot(wine_data$Flavanoids)
plot(wine_data$Flavanoids)

# Nonflavanoid phenols: 
boxplot(wine_data$Nonflavanoid_phenols)
plot(wine_data$Nonflavanoid_phenols)

# Proanthocyanins: 
boxplot(wine_data$Proanthocyanins)
plot(wine_data$Proanthocyanins)

# Color intensity: 
boxplot(wine_data$Color_intensity)
plot(wine_data$Color_intensity)
# Veo cuales son las filas de los outliers
wine_data[wine_data$Color_intensity > 3.5, ]

# Hue: 
boxplot(wine_data$Hue)
plot(wine_data$Hue)
# Veo cuales son las filas de los outliers
wine_data[wine_data$Hue >= 12, ]

# OD280.OD315 of diluted wines: 
boxplot(wine_data$OD280.OD315_of_diluted_wines)
plot(wine_data$OD280.OD315_of_diluted_wines)
# Veo cuales son las filas de los outliers
wine_data[wine_data$OD280.OD315_of_diluted_wines >= 1.6, ]

# Proline: 
boxplot(wine_data$Proline)
plot(wine_data$Proline)

# Rating: 
boxplot(wine_data$Rating)
plot(wine_data$Rating)

# CLUSTERING
# Elimino las filas que son outliers
wine_no_outliers <- wine_data[-c(26,122,60,74,70,96,111,159,116),]
# Calculo distancia euclídea
distancia2=dist(wine_no_outliers)
# Aplico clustering jerárquico con metodo complete
clustering_jerarquico2=hclust(distancia2,method="complete")
clustering_jerarquico2
plot(clustering_jerarquico2)
rect.hclust(clustering_jerarquico2,k=3)
rect.hclust(clustering_jerarquico2,k=4)
# Aplico clustering jerárquico con metodo ward.D2
clustering_jerarquico2=hclust(distancia2,method="ward.D2")
clustering_jerarquico2
plot(clustering_jerarquico2)
rect.hclust(clustering_jerarquico2,k=3)
rect.hclust(clustering_jerarquico2,k=4)

###############################################################################
# K-MEDIAS                                                                    #
###############################################################################
set.seed(1234)
distancia=dist(wine_no_outliers)
# K-means con k=3
kmeans3=kmeans(distancia,3)
grupo3=kmeans3$cluster
# K-means con k=4
kmeans4=kmeans(distancia,4)
grupo4=kmeans4$cluster

# Visualizar clusters
library(fpc)
plotcluster(wine_no_outliers,grupo3)
plotcluster(wine_no_outliers,grupo4)
plotcluster(wine_no_outliers,wine[-c(26,122,60,74,70,96,111,159,116),]$Alcohol)
# Visualizar grupo de cada instancia
plot(wine[-c(26,122,60,74,70,96,111,159,116),]$Alcohol)
plot(grupo3)
plot(grupo4)

###############################################################################
# REDUCCIÓN DE DIMENSIONALIDAD K-MEDIAS CON K=3                               #
###############################################################################

# Visualizar la matriz de correlación
library(corrplot)
corrplot(cor(wine_data), method="number", is.corr=FALSE)
# Elimino la columna Flavanoids
wine_reduction <- wine_data[c(-6)]

# Aplico el algoritmo k-means
distancia=dist(wine_reduction)
# K-means con k=3
kmeans3=kmeans(distancia,3)
grupo3=kmeans3$cluster
# Visualización
plotcluster(wine_reduction,grupo3)
plotcluster(wine_reduction,wine$Alcohol)
plot(wine$Alcohol)
plot(grupo3)

###############################################################################
# DBSCAN                                                                      #
###############################################################################

library(dbscan)
# Elijo el radio óptimo
kNNdistplot(wine_data, k = 5)
abline(h = 50, lty = 2)

# Aplico DBSCAN
dbscan<-dbscan(wine_data,eps=50,minPts = 5)
plotcluster(wine_data,dbscan$cluster)
plotcluster(wine_data,wine$Alcohol)
plot(dbscan$cluster)
plot(wine$Alcohol)

