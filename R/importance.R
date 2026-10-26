#  ----------------------------------------------------------------------------
#  ggmodel PACKAGE
#  ggplot2 Machine Learning models
#  File: importance.R
#  (c) 2018 - Enrique Pérez Herrero
#  email: eph.project1500@gmail.com
#  The MIT License (MIT)
#  ----------------------------------------------------------------------------

# Shared code for ggMeanDecreaseAccuracy() and ggMeanDecreaseGini()

# Returns the `num` most important features of a randomForest model,
# ordered by `measure`. Stops if the model has no such importance column
# (MeanDecreaseAccuracy needs randomForest(..., importance = TRUE)).
importance_data <- function(model, measure, num) {
  imp <- model$importance
  if (is.null(imp) || !measure %in% colnames(imp)) {
    stop("The model has no '", measure, "' importance. ",
         "Fit it with randomForest(..., importance = TRUE) ",
         "for a classification problem.", call. = FALSE)
  }
  df <- data.frame(Variable = as.factor(row.names(imp)),
                   Value = imp[, measure],
                   row.names = NULL)
  utils::head(df[order(-df$Value, df$Variable), ], num)
}

#' @importFrom ggplot2 ggplot aes geom_bar xlab ylab guides
#' @importFrom ggplot2 coord_flip theme_minimal
#' @importFrom stats reorder
#' @noRd
importance_plot <- function(df, label) {
  Variable <- NULL
  Value <- NULL
  ggplot2::ggplot(data = df, ggplot2::aes(
    x = stats::reorder(Variable, Value),
    y = Value,
    fill = stats::reorder(Variable, Value))) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab(label) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_bar(stat = "identity", width = .90) +
    ggplot2::theme_minimal()
}
