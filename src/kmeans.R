# ==============================================================================
# Nombre: K-means
# Propósito: Clase para aplicar el algoritmo k-means a los datos
# ==============================================================================


library(methods)

Kmeans <- setRefClass('Kmeans',
                      contains = 'Algorithm',
                      fields = list(data = 'data.frame'))

Kmeans$methods(
  preprocess = function() {
    'Preprocesar la data en caso de ser necesario'
    callSuper()
    
    # Validar que nuestros datos están escalados (media de cero en las columnas)
    print(summary(scaleData))
  },
  
  apply = function() {
    'Aplicar el algoritmo kmeans'
    
    # Obtener el número óptimo de clusters usando el método elbow
    clustersAmount <- .self$getElbowMethod()
    print(clustersAmount)
    
    # Obtener el número óptimo de clusters usando método silhouette
    print(.self$getSilhouetteMethod())
    
    # Aplicar el algoritmo kmeans
    kmeanResult <- kmeans(scaleData, center = clustersAmount)
    data$cluster <<- kmeanResult$cluster
    
    # Contar el número de clientes en cada cluster
    count(data, cluster)
  },
  
  visualize = function() {
    'Visualiar la data'
    
    # Obtener la gráfica del método de elbow
    wssPlot <- .self$getOptionalClusterPlot('wss')
    
    # Obtener la gráfica del método de silhouette
    silhouette <- .self$getOptionalClusterPlot('silhouette')
    
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
    
    # Visualizar la distribución del total de gastos en base al cluster
    clusterTotalSpend <- ggplot(data, aes(TotalSpend)) +
      geom_histogram(bins = 30, fill = 'cadetblue2', color = 'blue') +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(TotalSpend)),
        color = 'red',
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)
    
    # Visualizar la distribución del gasto en vinos en base al cluster
    clusterWinesSpend <- ggplot(data, aes(MntWines)) +
      geom_histogram(bins = 30, fill = 'cadetblue2', color = 'blue') +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(MntWines)),
        color = 'red',
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)
    
    # Visualizar la distribución de compras onlines en base al cluster
    clusterOnlinePurchase <- ggplot(data, aes(NumWebPurchases)) +
      geom_histogram(bins = 30, fill = 'cadetblue2', color = 'blue') +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(NumWebPurchases)),
        color = 'red',
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)
    
    # Visualizar el resultado de aplicar kmeans
    clustersAmount <- .self$getElbowMethod()
    kmeansPlot <- fviz_cluster(kmeans(scaleData, centers = clustersAmount),
                   geom = 'point',
                   data = scaleData)
    
    grid.arrange(wssPlot, silhouette, ncol = 1, nrow = 2)
    grid.arrange(
      clusterIncome, 
      clusterTotalSpend,
      clusterWinesSpend,
      clusterOnlinePurchase,
      ncol = 1, 
      nrow = 4
    )
    grid.arrange(kmeansPlot, ncol = 1, nrow = 1)
  }
)