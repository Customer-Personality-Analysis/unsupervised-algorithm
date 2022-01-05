# ==============================================================================
# Nombre: Algorithm
# Propósito: Clase base para los algoritmos no supervisados
# ==============================================================================


if (!require('factoextra')) install.packages('factoextra')
library(factoextra)
library(methods)

Algorithm <- setRefClass('Algorithm')

Algorithm$methods(
  getElbowMethod = function() {
    'Obtener el número óptimo de clusters usando el método de elbow'
    
    return (self$getOptimalCluster('wss'))
  },
  
  getSilhouetteMethod = function() {
    'Obtener el número óptimo de clusters usando el método de silhouette'
    
    return (self$getOptimalCluster('silhouette'))
  },
  
  .getOptimalCluster = function(method) {
    'Obtener el número óptimo de clusters en base al resultado de fviz_nbclust'
    
    cData <- fviz_nbclust(data, kmeans, method = method)$data
    
    return (as.numeric(cData$clusters[which.max(cData$y)]))
  }
)