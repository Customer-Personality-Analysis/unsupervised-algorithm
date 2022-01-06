# ==============================================================================
# Nombre: Clustering Jerárquico
# Propósito: Clase para aplicar el algoritmo hclust a los datos
# ==============================================================================


library(methods)

Hclust <- setRefClass('Hclust', 
                      contains = 'Algorithm', 
                      fields = list(data = 'data.frame'))

Hclust$methods(
  preprocess = function() {
    'Preprocesar la data en caso de ser necesario'
    callSuper();
  },
  
  apply = function() {
    'Aplicar el algoritmo kmeans'
  },
  
  visualize = function() {
    'Visualiar la data'
  }
  
)