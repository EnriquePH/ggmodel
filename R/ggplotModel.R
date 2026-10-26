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
#' Line chart of the error rate of each class and the out-of-bag (OOB)
#' error, in black, against the number of trees.
#'
#' @param model An object of class `randomForest` for classification.
#' @param ... Optional parameters, currently unused.
#'
#' @importFrom grDevices rainbow
#' @importFrom ggplot2 ggplot ggtitle aes geom_line
#' @importFrom ggplot2 scale_color_manual ylab theme_minimal
#' @return A `ggplot2` graphic object.
#'
#' @examplesIf requireNamespace("randomForest", quietly = TRUE)
#' set.seed(1)
#' model <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50)
#' ggplotModel(model)
#' @export
ggplotModel <- function(model, ...) {
  UseMethod("ggplotModel")
}

#' @rdname ggplotModel
#' @export
ggplotModel.randomForest <- function(model, ...) {
  Tree <- NULL
  Error <- NULL
  variable <- NULL
  err <- model$err.rate
  if (is.null(err)) {
    stop("The model has no error rate: only classification ",
         "random forests are supported.", call. = FALSE)
  }
  model_data <- data.frame(
    Tree = rep(seq_len(nrow(err)), ncol(err)),
    variable = factor(rep(colnames(err), each = nrow(err)),
                      levels = colnames(err)),
    Error = as.vector(err)
  )
  classes <- setdiff(colnames(err), "OOB")
  # OOB color
  black <- "#000000"
  color_values <- c(stats::setNames(grDevices::rainbow(length(classes)),
                                    classes),
                    OOB = black)

  ggplot2::ggplot() +
    ggplot2::ggtitle("Random Forest Model Error") +
    ggplot2::geom_line(data = model_data[model_data$variable != "OOB", ],
                       ggplot2::aes(x = Tree, y = Error, colour = variable)) +
    ggplot2::geom_line(
      data = model_data[model_data$variable == "OOB", ],
      ggplot2::aes(x = Tree, y = Error, colour = variable),
      linewidth = 1) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::ylab("") +
    ggplot2::theme_minimal()
}

#' @rdname ggplotModel
#' @export
ggplotModel.default <- function(model, ...) {
  not_random_forest(model)
}
