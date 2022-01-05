# ==============================================================================
# Nombre: Main
# Propósito: Punto de entrada para aplicar los algoritmos no supervisados
# ==============================================================================


dir <- getwd()
marketingData <- read.delim(file=file.path('data', 'marketing_campaign.csv'), 
                            stringsAsFactors = FALSE)

setwd(paste(dir, '/src', sep=''))
source(file='data.R')
source(file='unsupervised.R')
setwd(dir)

data <- Data(data = marketingData)
unsupervised <- Unsupervised(data = data)

data$explore()
data$clean(removeNA = FALSE)
data$visualize()

algorithms <- c('kmeans', 'hclust')

for (i in 1:length(algorithms)) { 
  algorithm <- unsupervised$getAlgorithm(algorithms[i])
  algorithm$preprocess()
  algorithm$apply()
  algorithm$visualize()
}
