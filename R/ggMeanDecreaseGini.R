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
#' @param model An object of class \code{randomForest}
#' @param num An integer with the numbers of features to plot
#' @param ... Other arguments
#'
#' @return A \code{ggplot2} graphic object, or a warning if model
#'  \code{importance} is set to \code{FALSE}
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 xlab ylab guides
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ggtitle
#' @importFrom stats reorder
#' @importFrom utils head
#' @examples
#' \dontrun{
#' # model is a randomForest model
#' ggMeanDecreaseGini(model$finalModel)
#' }
#' @export
ggMeanDecreaseGini <- function(model, num, ...) {
  UseMethod("ggMeanDecreaseGini")
}

#' @method ggMeanDecreaseGini randomForest
#' @export
ggMeanDecreaseGini.randomForest <- function(model, num = 25, ...) {
  Variable <- NULL
  MeanDecreaseGini <- NULL
  if (model$param$importance) {
    df <- as.data.frame(model$importance)
    df <- cbind("Variable" = as.factor(row.names(df)), df)
    df <- utils::head(df[with(df, order(-MeanDecreaseGini, Variable)),], num)
    p <- ggplot2::ggplot(data = df, aes(
      x = stats::reorder(Variable, MeanDecreaseGini),
      y = MeanDecreaseGini,
      fill = stats::reorder(Variable, MeanDecreaseGini))) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::ylab("Mean Decrease Gini") +
      ggplot2::guides(fill = FALSE) +
      ggplot2::geom_bar(stat = "identity", width = .90) +
      ggplot2::theme_minimal()
    return(p)
  } else {
    warning("Model importance is set to FALSE")
  }
}
