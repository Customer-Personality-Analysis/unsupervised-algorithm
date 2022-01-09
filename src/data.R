# ==============================================================================
# Nombre: Data
# Propósito: Clase para gestionar el data frame
# ==============================================================================

library(methods)
library(gridExtra)
library(lubridate)

Data <- setRefClass("Data", fields = list(data = "data.frame")) # nolint

Data$methods(
  explore = function() {
    "Explorar los datos"

    # Ver dimensiones y columnas
    print(dim(data))
    print(colnames(data))

    # Vision inicial de la data
    print(str(data))
    print(head(data, n = 15))
    print(summary(data))

    # Identificar columnas con valores NA
    columns_margin_type <- 2
    print(colnames(data)[apply(data, columns_margin_type, anyNA)])

    # Revisar si hay filas con duplicadas
    print(data[duplicated(data) |
                 duplicated(data, fromLast = TRUE), ])

    # Vision global de la data
    View(data)
  },

  clean = function(remove_na = FALSE) {
    "Limpiar los datos.
     @param removeNA si queremos remover o remplazar las filas con NA
    "

    # Manejar los valores NA
    if (remove_na) {
      data <<- data[complete.cases(data),]
    } else{
      data[is.na(data)] <<- 0
    }

    # Cambiar tipos de datos
    data$Dt_Customer <<- as.Date(dmy(data$Dt_Customer)) # nolint

    # Remover columnas con valores constantes y que no sabemos su significado
    data[c("Z_CostContact", "Z_Revenue")] <<- NULL

    # Reemplazar columnas categóricas a factores
    data$Marital_Status[data$Marital_Status %in%
                          c("Single", "Divorced", "Alone",
                            "Widow", "Absurd", "YOLO")] <<- "Soltero"
    data$Marital_Status[data$Marital_Status %in%
                          c("Together", "Married")] <<- "Casado"

    data$Education[data$Education %in%
                     c("Basic", "2n Cycle")] <<- "No Profesional"
    data$Education[data$Education %in%
                     c("Graduation", "Master", "PhD")] <<- "Profesional"

    data$Marital_Status <<- as.factor(data$Marital_Status) # nolint
    data$Education <<- as.factor(data$Education) # nolint
  },

  visualize = function() {
    "Visualizar los datos de diferentes formas"

    # Histograma de fecha de nacimiento
    plot_year_birth <- ggplot(data = data, aes(x = Year_Birth)) +
      ggtitle("Histograma de fecha de nacimiento") +
      geom_histogram(bins = 30, fill = "cadetblue2", color = "blue")

    # Histograma de ingresos
    plot_income <- ggplot(data = data, aes(x = Income)) +
      ggtitle("Histograma de ingresos") +
      geom_histogram(bins = 30, fill = "cadetblue2", color = "blue") +
      scale_x_continuous(n.breaks = 15)

    # Diagrama de barras de estado civil
    plot_marital_status <- ggplot(data = data, aes(x = Marital_Status)) +
      ggtitle("Diagramas de barras de estado civil") +
      geom_bar(fill = "cadetblue2", color = "blue")

    # Diagrama de barras de estudios realizados
    plot_education <- ggplot(data = data, aes(x = Education)) +
      ggtitle("Diagramas de barras de estudios realizados") +
      geom_bar(fill = "cadetblue2", color = "blue")

    # Mostrar los gráficos en paralelo
    grid.arrange(
      plot_year_birth,
      plot_income,
      plot_marital_status,
      plot_education,
      nrow = 2,
      ncol = 2
    )
  },

  getRawData = function() {
    return(data)
  }
)