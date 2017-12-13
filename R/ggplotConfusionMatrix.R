#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: ggplotConfusionMatrix.R
#  (c) 2017 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ---------------------------------------------------------------------------


#' Plot confusion matrix with metrics
#'
#' @param m An object of class confusionMatrix
#'
#' @return a plot
#' @importFrom caret confusionMatrix
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' @examples
#'
#' @export
ggplotConfusionMatrix <- function(m) {
  UseMethod("confusionMatrix")
}

#' @method ggplotConfusionMatrix confusionMatrix
#' @export
ggplotConfusionMatrix.confusionMatrix <- function(m) {
  Prediction <- NULL
  Reference <- NULL
  Freq <- NULL
  df <- data.frame(m$table)
  mytitle <- paste("Accuracy", round(100 * m$overall[1], 2), "% ",
                   "Kappa", round(100 * m$overall[2], 2), "%")

  ggplot2::ggplot(data = df, ggplot2::aes(x = Reference, y = Prediction)) +
    ggplot2::geom_tile(ggplot2::aes(fill = log(Freq)), colour = "white") +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::geom_text(size = 8,
                       ggplot2::aes(x = Reference,
                                    y = Prediction,
                                    label = Freq)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(mytitle)
}
