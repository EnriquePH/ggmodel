#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: ggConfusionMatrix.R
#  (c) 2017 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ----------------------------------------------------------------------------

#' Plot confusion matrix with metrics
#'
#' Draws the confusion matrix as a heat map, with the counts in each cell
#' and the accuracy (as a percentage) and Cohen's kappa (a coefficient
#' without units) in the title.
#'
#' @param m An object of class `confusionMatrix`, as returned by
#'   [caret::confusionMatrix()].
#'
#' @return A `ggplot2` graphic object.
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' @examplesIf requireNamespace("caret", quietly = TRUE) && requireNamespace("randomForest", quietly = TRUE)
#' set.seed(1)
#' model <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50)
#' m <- caret::confusionMatrix(stats::predict(model), iris$Species)
#' ggConfusionMatrix(m)
#' @export
ggConfusionMatrix <- function(m) {
  if (!inherits(m, "confusionMatrix")) {
    stop("`m` must be an object of class 'confusionMatrix'.", call. = FALSE)
  }
  Prediction <- NULL
  Reference <- NULL
  Freq <- NULL
  df <- data.frame(m$table)
  mytitle <- sprintf("Accuracy %.2f %%   Kappa %.3f",
                     100 * m$overall[["Accuracy"]], m$overall[["Kappa"]])

  ggplot2::ggplot(data = df, ggplot2::aes(x = Reference, y = Prediction)) +
    ggplot2::geom_tile(ggplot2::aes(fill = log1p(Freq)), colour = "white") +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::geom_text(size = 8,
                       ggplot2::aes(x = Reference,
                                    y = Prediction,
                                    label = Freq)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(mytitle)
}
