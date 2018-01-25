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
#' @param model An object of class `randomForest`
#' @param ... Optional parameters to be passed
#'
#' @return A `ggplot2` graphic object

#'
#' @examples
#' \dontrun{
#' ggplotModel(model$finalModel)
#' }
#' @export
ggplotModel <- function(model, ...) {
  UseMethod("randomForest")
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
  model.data <- reshape2::melt(model.data,
                               id.vars = "Tree",
                               value.name = "Error")
  num_colors <- ncol(model$err.rate) - 1
  color_values <- c(grDevices::rainbow(num_colors),
                    gplots::col2hex("black"))

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