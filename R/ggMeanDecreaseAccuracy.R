#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: ggMeanDecreaseAccuracy.R
#  (c) 2018 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ----------------------------------------------------------------------------

# PLOT RF MeanDecreaseAccuracy
# Only works if importance = TRUE

#' ggplot Mean Decrease Accuracy
#'
#' @param model An object of class \code{randomForest}
#' @param num An integer with the numbers of features to plot
#' @param ... Other arguments
#'
#' @return A \code{ggplot2} graphic object with feature accuracy in
#'  decrease order, or a warning if model \code{importance} is set
#'  to \code{FALSE}
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
#' ggMeanDecreaseAccuracy(model$finalModel)
#' }
#' @export
ggMeanDecreaseAccuracy <- function(model, num, ...) {
  UseMethod("ggMeanDecreaseAccuracy")
}

#' @method ggMeanDecreaseAccuracy randomForest
#' @export
ggMeanDecreaseAccuracy.randomForest <- function(model, num = 25, ...) {
  Variable <- NULL
  MeanDecreaseAccuracy <- NULL
  if (model$param$importance) {
    df <- as.data.frame(model$importance)
    df <- cbind("Variable" = as.factor(row.names(df)), df)
    df <-
      head(df[with(df, order(-MeanDecreaseAccuracy, Variable)), ], num)
    p <- ggplot2::ggplot(data = df,
                aes(
                  x = stats::reorder(Variable, MeanDecreaseAccuracy),
                  y = MeanDecreaseAccuracy,
                  fill = stats::reorder(Variable, MeanDecreaseAccuracy)
                )) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::guides(fill = FALSE) +
      ggplot2::geom_bar(stat = "identity", width = .90) +
      ggplot2::theme_minimal()
    return(p)
  } else {
    warning("Model importance is set to FALSE")
  }
}
