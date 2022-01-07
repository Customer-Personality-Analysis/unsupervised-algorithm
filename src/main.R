# ==============================================================================
# Nombre: Main
# Propósito: Punto de entrada para aplicar los algoritmos no supervisados
# ==============================================================================


# Cargar de datos y configuración de ambiente
set.seed(1234)
dir <- getwd()
marketingData <- read.delim(file=file.path('data', 'marketing_campaign.csv'), 
                            stringsAsFactors = FALSE)

setwd(paste(dir, '/src', sep=''))
source(file='data.R')
source(file='unsupervised.R')
setwd(dir)

# Análisis exploratorio de datos
data <- Data(data = marketingData)

data$explore()
data$clean(removeNA = FALSE)
data$visualize()

# Ejecución de algoritmos no supervisados
unsupervised <- Unsupervised(data = data$getRawData())
algorithms <- c('kmeans', 'hclust')

for (i in 1:length(algorithms)) { 
  algorithm <- unsupervised$getAlgorithm(algorithms[i])
  algorithm$preprocess()
  algorithm$apply()
  algorithm$visualize()
}

