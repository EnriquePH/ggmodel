library(caret)
library(ggmodel)

confusionMatrix(iris$Species, sample(iris$Species))

p <- ggplotConfusionMatrix(conf_matrix)

p

names(conf_matrix$overall[1])

extract_metric <- function(m, metric = "Accuracy") {
  paste(metric, round(100 * m$overall[metric], 2), "%")
}


caret:::as.matrix.confusionMatrix(conf_matrix)
print(conf_matrix)
