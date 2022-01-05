# ==============================================================================
# Nombre: K-means
# Propósito: Clase para aplicar el algoritmo k-means a los datos
# ==============================================================================


library(methods)

Kmeans <- setRefClass('Kmeans',
                      contains = 'Algorithm',
                      fields = list(data = 'Data'))

Kmeans$methods(
  preprocess = function() {
    'Preprocesar la data en caso de ser necesario'
    
    # Reemplazar columnas categóricas a numéricas
    
    # Crear nuevas columnas
    
    #data[colSums(!is.na(data)) > 0]
    #data[complete.cases(data), ]
    print('from kmeans')
  },
  
  apply = function() {
    'Aplicar el algoritmo kmeans'
    
    #print(.self$getElbowMethod())
    # Se deberia estudiar la media y la desviacion para saber si hay que "normalizar" o escalar
    #colMeans(puntos)
    #apply(puntos,2,sd)
    
    #puntos_escalado<-scale(puntos)
    #colMeans(puntos_escalado)
    #apply(puntos_escalado, 2, sd)
  },
  
  visualize = function() {
    'Visualiar la data'
  }
)