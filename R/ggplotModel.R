#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: ggplotModel.R
#  (c) 2018 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ----------------------------------------------------------------------------

#' Plot model error and OOB
#'
#' @param model An object of class \code{randomForest}
#' @param ... Optional parameters to be passed
#'
#' @importFrom reshape2 melt
#' @importFrom grDevices rainbow
#' @importFrom ggplot2 ggplot ggtitle aes geom_line
#' @importFrom ggplot2 scale_color_manual ylab theme_minimal
#' @return A \code{ggplot2} graphic object
#'
#' @examples
#' \dontrun{
#' # model is a randomForest model
#' ggplotModel(model$finalModel)
#' }
#' @export
ggplotModel <- function(model, ...) {
  UseMethod("ggplotModel")
}

#' @method ggplotModel randomForest
#' @export
ggplotModel.randomForest <- function(model, ...) {
  Tree <- NULL
  Error <- NULL
  variable <- NULL
  model_data <- as.data.frame(model$err.rate)
  model_data <-
    cbind("Tree" = as.numeric(row.names(model_data)), model_data)
  model_data <- reshape2::melt(model_data,
                               id.vars = "Tree",
                               value.name = "Error")
  num_colors <- ncol(model$err.rate) - 1
  # OOB color
  black <- "#000000"
  color_values <- c(grDevices::rainbow(num_colors), black)

  ggplot2::ggplot() +
    ggplot2::ggtitle("Random Forest Model Error") +
    ggplot2::geom_line(data = model_data[model_data$variable != "OOB", ],
                       ggplot2::aes(x = Tree, y = Error, colour = variable)) +
    ggplot2::geom_line(
      data = model_data[model_data$variable == "OOB", ],
      ggplot2::aes(x = Tree, y = Error, colour = variable),
      size  = 1) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::ylab("") +
    ggplot2::theme_minimal()
}
