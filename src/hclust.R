# ==============================================================================
# Nombre: Clustering Jerárquico
# Propósito: Clase para aplicar el algoritmo hclust a los datos
# ==============================================================================


if (!require('purrr')) install.packages('purrr')
if (!require('dendextend')) install.packages('dendextend')
library(dendextend)
library(purrr)
library(methods)

Hclust <- setRefClass('Hclust',
                      contains = 'Algorithm',
                      fields = list(data = 'data.frame'))

Hclust$methods(
  preprocess = function() {
    'Preprocesar la data en caso de ser necesario'
    callSuper()
    
    
    # Validar que nuestros datos están escalados (media de cero en las columnas)
    print(summary(scaleData))
  },
  
  apply = function() {
    'Aplicar el algoritmo kmeans'
    
    # Calcular la matrix de distancia
    distMatrix <- dist(scaleData)
    
    # Aplicar el algoritmo hclust
    hclustResult <- hclust(distMatrix, method = 'complete')
    cutTree <- cutree(hclustResult, k = 5)
    data$cluster <<- cutTree
    
    # Contar el número de clientes en cada cluster
    print(count(data, cluster))
  },
  
  visualize = function() {
    'Visualiar la data'
    
    # Visualizar dondograma del cluster
    hclustResult <- hclust(dist(scaleData), method = 'complete')
    plot(hclustResult, hang = -1)
    plot(color_branches(as.dendrogram(hclustResult), k = 5))
    
    # Visualizar la distribución de ingresos en base al cluster
    clusterIncome <- ggplot(data, aes(Income)) +
      geom_histogram(bins = 30, fill = 'cadetblue2', color = 'blue') +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(Income)),
        color = 'red',
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)
    
    
    grid.arrange(clusterIncome, ncol = 1, nrow = 1)
  }
  
)