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
#' The mean decrease in Gini index is the total decrease in node impurity
#' from splits on a feature, averaged over all trees. It tends to favour
#' features with many possible split points, and it is not the same measure
#' as the mean decrease in accuracy ([ggMeanDecreaseAccuracy()]). Neither
#' of them is a measure of causal effect.
#'
#' @param model An object of class `randomForest` for classification
#'   (for example `train(...)$finalModel` from 'caret').
#' @param num A positive whole number: how many of the most important
#'   features to plot. If the model has fewer, all of them are plotted.
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

#' @rdname ggMeanDecreaseGini
#' @export
ggMeanDecreaseGini.default <- function(model, num = 25, ...) {
  not_random_forest(model)
}
