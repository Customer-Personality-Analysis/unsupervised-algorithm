# ==============================================================================
# Nombre: Main
# Propósito: Punto de entrada para aplicar los algoritmos no supervisados
# ==============================================================================

dir <- getwd()
data <- read.delim(file=file.path('data', 'marketing_campaign.csv'), stringsAsFactors = FALSE)

setwd(paste(dir, '/src', sep=''))
source(file='unsupervised.R')
setwd(dir)

unsupervised <- Unsupervised(data = data)

algorithm <- unsupervised$getAlgorithm('kmeans')
algorithm$preprocess()
algorithm$apply()
algorithm$visualize()

algorithm <- unsupervised$getAlgorithm('hclust')
algorithm$preprocess()
algorithm$apply()
algorithm$visualize()
