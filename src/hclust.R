# ==============================================================================
# Nombre: Clustering Jerárquico
# Propósito: Clase para aplicar el algoritmo hclust a los datos
# ==============================================================================


if (!require("purrr")) install.packages("purrr")
if (!require("dendextend")) install.packages("dendextend")
library(dendextend)
library(purrr)
library(methods)

Hclust <- setRefClass("Hclust", # nolint
                      contains = "Algorithm",
                      fields = list(data = "data.frame"))

Hclust$methods(
  preprocess = function() {
    "Preprocesar la data en caso de ser necesario"
    callSuper()


    # Validar que nuestros datos están escalados (media de cero en las columnas)
    print(summary(scaleData))
  },

  apply = function() {
    "Aplicar el algoritmo kmeans"

    # Calcular la matrix de distancia
    dist_matrix <- dist(scaleData)

    # Aplicar el algoritmo hclust
    hclust_result <- hclust(dist_matrix, method = "complete")
    cut_tree <- cutree(hclust_result, k = 5)
    data$cluster <<- cut_tree

    # Contar el número de clientes en cada cluster
    print(count(data, cluster))
  },

  visualize = function() {
    "Visualiar la data"

    # Visualizar dondograma del cluster
    hclust_result <- hclust(dist(scaleData), method = "complete")
    plot(hclust_result, hang = -1)
    plot(color_branches(as.dendrogram(hclust_result), k = 5))

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


    grid.arrange(cluster_income, ncol = 1, nrow = 1)
  }

)