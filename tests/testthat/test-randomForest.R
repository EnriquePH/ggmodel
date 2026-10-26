skip_if_not_installed("randomForest")

set.seed(1)
rf <- randomForest::randomForest(Species ~ ., data = iris,
                                 ntree = 30, importance = TRUE)
rf_noimp <- randomForest::randomForest(Species ~ ., data = iris, ntree = 30)

test_that("ggplotModel draws one line per class plus OOB in black", {
  p <- ggplotModel(rf)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  oob <- b$data[[2]]
  expect_true(all(oob$colour == "#000000"))
  expect_equal(nrow(oob), 30)
})

test_that("ggplotModel rejects regression forests", {
  reg <- randomForest::randomForest(Sepal.Length ~ ., data = iris, ntree = 10)
  expect_error(ggplotModel(reg), "classification")
})

test_that("ggMeanDecreaseGini works with and without importance = TRUE", {
  expect_s3_class(ggMeanDecreaseGini(rf), "ggplot")
  expect_s3_class(ggMeanDecreaseGini(rf_noimp), "ggplot")
})

test_that("ggMeanDecreaseAccuracy needs importance = TRUE", {
  expect_s3_class(ggMeanDecreaseAccuracy(rf), "ggplot")
  expect_error(ggMeanDecreaseAccuracy(rf_noimp), "importance = TRUE")
})

test_that("num limits and orders the features", {
  p <- ggMeanDecreaseGini(rf, num = 2)
  expect_equal(nrow(p$data), 2)
  expect_equal(p$data$Value, sort(rf$importance[, "MeanDecreaseGini"],
                                  decreasing = TRUE)[1:2],
               ignore_attr = TRUE)
})

test_that("ggConfusionMatrix plots a caret confusion matrix", {
  skip_if_not_installed("caret")
  m <- caret::confusionMatrix(stats::predict(rf), iris$Species)
  p <- ggConfusionMatrix(m)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "^Accuracy")
  expect_error(ggConfusionMatrix(table(1:3, 1:3)), "confusionMatrix")
})
