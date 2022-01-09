# ==============================================================================
# Nombre: K-means
# Propósito: Clase para aplicar el algoritmo k-means a los datos
# ==============================================================================


if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
library(methods)

Kmeans <- setRefClass("Kmeans", # nolint
                      contains = "Algorithm",
                      fields = list(data = "data.frame"))

Kmeans$methods(
  preprocess = function() {
    "Preprocesar la data en caso de ser necesario"
    callSuper()

    # Validar que nuestros datos están escalados (media de cero en las columnas)
    print(summary(scaleData))
  },

  apply = function() {
    "Aplicar el algoritmo kmeans"

    # Obtener el número óptimo de clusters usando el método elbow
    clusters_amount <- .self$getElbowMethod()
    print(clusters_amount)

    # Obtener el número óptimo de clusters usando método silhouette
    print(.self$getSilhouetteMethod())

    # Aplicar el algoritmo kmeans
    kmean_result <- kmeans(scaleData, center = clusters_amount)
    data$cluster <<- kmean_result$cluster

    # Contar el número de clientes en cada cluster
    print(count(data, cluster))
  },

  visualize = function() {
    "Visualiar la data"

    # Obtener la gráfica del método de elbow
    wssPlot <- .self$getOptionalClusterPlot("wss")

    # Obtener la gráfica del método de silhouette
    silhouette <- .self$getOptionalClusterPlot("silhouette")

    # Visualizar la distribución de ingresos en base al cluster
    cluster_income <- ggplot(data, aes(Income)) +
      geom_histogram(bins = 30, fill = "cadetblue2", color = "blue") +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(Income)),
        color = "red",
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)

    # Visualizar la distribución del total de gastos en base al cluster
    cluster_total_spend <- ggplot(data, aes(TotalSpend)) +
      geom_histogram(bins = 30, fill = "cadetblue2", color = "blue") +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(TotalSpend)),
        color = "red",
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)

    # Visualizar la distribución del gasto en vinos en base al cluster
    cluster_wines_spend <- ggplot(data, aes(MntWines)) +
      geom_histogram(bins = 30, fill = "cadetblue2", color = "blue") +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(MntWines)),
        color = "red",
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)

    # Visualizar la distribución de compras onlines en base al cluster
    cluster_online_purchase <- ggplot(data, aes(NumWebPurchases)) +
      geom_histogram(bins = 30, fill = "cadetblue2", color = "blue") +
      facet_wrap(vars(cluster)) +
      geom_vline(
        aes(xintercept = mean(NumWebPurchases)),
        color = "red",
        linetype = "dashed",
        size = 1
      ) + scale_x_continuous(n.breaks = 15)

    # Visualizar el resultado de aplicar kmeans
    clusters_amount <- .self$getElbowMethod()
    kmeans_plot <- fviz_cluster(kmeans(scaleData, centers = clusters_amount),
                   geom = "point",
                   data = scaleData)

    grid.arrange(wssPlot, silhouette, ncol = 1, nrow = 2)
    grid.arrange(
      cluster_income,
      cluster_total_spend,
      cluster_wines_spend,
      cluster_online_purchase,
      ncol = 1,
      nrow = 4
    )
    grid.arrange(kmeans_plot, ncol = 1, nrow = 1)
  }
)