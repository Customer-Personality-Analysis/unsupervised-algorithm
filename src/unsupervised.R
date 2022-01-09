# ==============================================================================
# Nombre: Unsupervised
# Propósito: Clase para gestionar los algoritmos no supervisados
# ==============================================================================

library(methods)
source(file = "algorithm.R")
source(file = "kmeans.R")
source(file = "hclust.R")

Unsupervised <- setRefClass("unsupervised", fields = list(data = "data.frame")) # nolint

Unsupervised$methods(
  getAlgorithm = function(name) {
    "Instanciar la clase para procesar el algoritmo en cuestión"
    if (name == "kmeans") {
      return(Kmeans(data = data))
    }else if (name == "hclust") {
      return(Hclust(data = data))
    }
  }
)