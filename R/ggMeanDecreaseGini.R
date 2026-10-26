#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: ggMeanDecreaseGini.R
#  (c) 2018 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ----------------------------------------------------------------------------

#' ggplot Mean Decrease Gini
#'
#' Bar chart of the features of a random forest ordered by their mean
#' decrease in Gini index.
#'
#' @param model An object of class `randomForest` for classification
#'   (for example `train(...)$finalModel` from 'caret').
#' @param num An integer with the numbers of features to plot.
#' @param ... Other arguments, currently unused.
#'
#' @return A `ggplot2` graphic object with feature mean Gini in
#'  decreasing order.
#' @examplesIf requireNamespace("randomForest", quietly = TRUE)
#' set.seed(1)
#' model <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50)
#' ggMeanDecreaseGini(model)
#' @export
ggMeanDecreaseGini <- function(model, num, ...) {
  UseMethod("ggMeanDecreaseGini")
}

#' @rdname ggMeanDecreaseGini
#' @export
ggMeanDecreaseGini.randomForest <- function(model, num = 25, ...) {
  df <- importance_data(model, "MeanDecreaseGini", num)
  importance_plot(df, "Mean Decrease Gini")
}
