library(caret)
library(ggmodel)

confusionMatrix(iris$Species, sample(iris$Species))

p <- ggplotConfusionMatrix(conf_matrix)


names(conf_matrix$overall[1])

extract_metric <- function(m, metric = "Accuracy") {
  paste(metric, round(100 * m$overall[metric], 2), "%")
}

extract_metric(conf_matrix)
