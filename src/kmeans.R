# ==============================================================================
# Nombre: K-means
# Propósito: Clase para aplicar el algoritmo k-means a los datos
# ==============================================================================


library(methods)

Kmeans <- setRefClass('Kmeans', fields = list(data = 'data.frame'))

Kmeans$methods(
  preprocess = function() {
    'Preprocesar la data en caso de ser necesario'
    print('from kmeans')
  },
  apply = function() {
    'Aplicar el algoritmo kmeans'
  },
  visualize = function() {
    'Visualiar la data'
  }
)