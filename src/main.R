# ==============================================================================
# Nombre: Main
# Propósito: Punto de entrada para aplicar los algoritmos no supervisados
# ==============================================================================


# Carga de datos y configuración de ambiente
set.seed(1234)
dir <- getwd()
marketing_data <- read.delim(file = file.path("data", "marketing_campaign.csv"),
 stringsAsFactors = FALSE)

setwd(paste(dir, "/src", sep = ""))
source(file = "data.R")
source(file = "unsupervised.R")
setwd(dir)

# Análisis exploratorio de datos
data <- Data(data = marketing_data)

data$explore()
data$clean(remove_na = FALSE)
data$visualize()

# Ejecución de algoritmos no supervisados
unsupervised <- Unsupervised(data = data$getRawData())
algorithms <- c("kmeans")

for (i in 1:length(algorithms)) {
  algorithm <- unsupervised$getAlgorithm(algorithms[i])
  algorithm$preprocess()
  algorithm$apply()
  algorithm$visualize()
}
