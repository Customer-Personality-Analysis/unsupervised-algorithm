# ==============================================================================
# Nombre: Unsupervised
# Propósito: Clase para gestionar los algoritmos no supervisados
# ==============================================================================


library(methods)
source(file = 'algorithm.R')
source(file = 'kmeans.R')
source(file = 'hclust.R')

Unsupervised <-
  setRefClass('unsupervised', fields = list(data = 'data.frame'))

Unsupervised$methods(
  getAlgorithm = function(name) {
    'Instanciar la clase para procesar el algoritmo en cuestión'
    if (name == 'kmeans') {
      return (Kmeans(data = data))
    } else if (name == 'hclust') {
      return (Hclust(data = data))
    }
  },
  
  compare = function(results) {
    'Comparar los clusters en el vector de resultados'
    
    # Ver la similaridad/frequencia entre los clusters
    print(table(map(results, function(algorithm) algorithm$data$cluster)))
  }
)