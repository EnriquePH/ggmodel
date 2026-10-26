#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: ggMeanDecreaseAccuracy.R
#  (c) 2018 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ----------------------------------------------------------------------------

#' ggplot Mean Decrease Accuracy
#'
#' Bar chart of the features of a random forest ordered by their mean
#' decrease in accuracy.
#'
#' @param model An object of class `randomForest`, fitted with
#'   `importance = TRUE` (for example `train(...)$finalModel` from 'caret').
#' @param num An integer with the numbers of features to plot.
#' @param ... Other arguments, currently unused.
#'
#' @return A `ggplot2` graphic object with feature accuracy in
#'  decreasing order. It is an error if the model was fitted without
#'  `importance = TRUE`.
#' @examplesIf requireNamespace("randomForest", quietly = TRUE)
#' set.seed(1)
#' model <- randomForest::randomForest(Species ~ ., data = iris,
#'                                     ntree = 50, importance = TRUE)
#' ggMeanDecreaseAccuracy(model)
#' @export
ggMeanDecreaseAccuracy <- function(model, num, ...) {
  UseMethod("ggMeanDecreaseAccuracy")
}

#' @rdname ggMeanDecreaseAccuracy
#' @export
ggMeanDecreaseAccuracy.randomForest <- function(model, num = 25, ...) {
  df <- importance_data(model, "MeanDecreaseAccuracy", num)
  importance_plot(df, "Mean Decrease Accuracy")
}
