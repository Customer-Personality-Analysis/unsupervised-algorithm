# ==============================================================================
# Nombre: Data
# Propósito: Clase para gestionar el data frame
# ==============================================================================


library(methods)
require(gridExtra)

Data <- setRefClass('Data', fields = list(data = 'data.frame'))

Data$methods(
  explore = function() {
    'Explorar los datos'
    
    # Ver dimensiones y columnas
    print(dim(data))
    print(colnames(data))
    
    # Vision inicial de la data
    print(str(data))
    print(head(data, n = 15))
    print(summary(data))
    
    # Identificar columnas con valores NA
    columnsMarginType <- 2
    print(colnames(data)[apply(data, columnsMarginType, anyNA)])
    
    # Revisar si hay filas con duplicadas
    print(data[duplicated(data) |
                 duplicated(data, fromLast = TRUE),])
    
    # Vision global de la data
    View(data)
  },
  
  clean = function(removeNA = FALSE) {
    'Limpiar los datos.
     @param removeNA si queremos remover o remplazar las filas con NA
    '
    
    # Manejar los valores NA
    if (removeNA) {
      data <<- data[complete.cases(data), ]
    } else{
      data[is.na(data)] <<- 0
    }
    
    # Cambiar tipos de datos
    data$Dt_Customer <<- as.Date(dmy(data$Dt_Customer))
    
    # Remover columnas con valores constantes y que no sabemos su significado
    marketingData[c('Z_CostContact', 'Z_Revenue')] <- NULL
  },
  
  visualize = function() {
    'Visualizar los datos de diferentes formas'
    
    # Histograma de fecha de nacimiento
    plotYearBirth <- ggplot(data = data, aes(x = Year_Birth)) +
      ggtitle('Histograma de fecha de nacimiento') +
      geom_histogram(bins = 30,
                     fill = 'cadetblue2',
                     color = 'blue')
    
    # Histograma de ingresos
    plotIncome <- ggplot(data = data, aes(x = Income)) +
      ggtitle('Histograma de ingresos') +
      geom_histogram(bins = 30,
                     fill = 'cadetblue2',
                     color = 'blue') +
      scale_x_continuous(n.breaks = 15)
    
    # Diagrama de barras de estado civil
    plotMaritalStatus <- ggplot(data = data, aes(x = Marital_Status)) + 
      ggtitle('Diagramas de barras de estado civil') + 
      geom_bar(fill='cadetblue2', color='blue')
    
    # Diagrama de barras de estudios realizados
    plotEducation <- ggplot(data = data, aes(x = Education)) + 
      ggtitle('Diagramas de barras de estudios realizados') + 
      geom_bar(fill='cadetblue2', color='blue')
    
    # Mostrar los gráficos en paralelo
    grid.arrange(plotYearBirth, plotIncome, plotMaritalStatus, plotEducation,
                 nrow = 2, ncol = 2)
  },
  
  getRawData = function() {
    return (data)
  }
)