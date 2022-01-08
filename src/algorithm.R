# ==============================================================================
# Nombre: Algorithm
# Propósito: Clase base para los algoritmos no supervisados
# ==============================================================================


if (!require('factoextra')) install.packages('factoextra')
library(factoextra)
library(methods)
library(stringr)

Algorithm <-
  setRefClass('Algorithm', fields = list(data = 'data.frame',
                                         scaleData = 'matrix'))

Algorithm$methods(
  preprocess = function() {
    'Preprocesar la data en caso de ser necesario'
    
    # Creación de nuevas columnas
    columnsToSum <- c('MntWines', 'MntFruits', 'MntMeatProducts', 
                 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')
    data$TotalSpend <<- rowSums(data[,columnsToSum])
    
    # Reemplazar los datos únicos demasiado alejados de la media
    data$Income[data$Income == 666666] <<- 51621
    data$Year_Birth[data$Year_Birth < 1920] <<- 1950
    
    # Remover columnas innecesarias
    rownames(data) <<- data$ID
    columnsToRemove <-
      c(
        'ID',
        'Dt_Customer',
        'AcceptedCmp1',
        'AcceptedCmp2',
        'AcceptedCmp3',
        'AcceptedCmp4',
        'AcceptedCmp5',
        'Complain',
        'Response',
        'Education',
        'Marital_Status'
      )
    data <<- data[,!names(data) %in% columnsToRemove, drop = F]
    
    # Normalizar la data aplicando scale
    scaleData <<- scale(data)
    
  },
  
  getElbowMethod = function() {
    'Obtener el número óptimo de clusters usando el método de elbow'
    
    return (.self$getOptimalCluster('wss'))
  },
  
  getSilhouetteMethod = function() {
    'Obtener el número óptimo de clusters usando el método de silhouette'
    
    return (.self$getOptimalCluster('silhouette'))
  },
  
  getOptimalCluster = function(method) {
    'Obtener el número óptimo de clusters en base al resultado de fviz_nbclust'
    
    cData <- .self$getOptionalClusterPlot(method)$data
    
    return (.self$getClusterNumberFromPlot(cData))
  },
  
  getOptionalClusterPlot = function(method) {
    'Obtener el plot de fviz_nbclust'
    
    return (fviz_nbclust(scaleData, kmeans, method = method) +
              labs(subtitle = str_to_title(paste('Método', method))))
  },
  
  getClusterNumberFromPlot = function(cData) {
    'Extraer el número óptiomo de clusters del resultado fviz_nbclust'
    
    set <- cData[-1, ]
    return (as.numeric(set$clusters[which.max(set$y)]))
  }
)