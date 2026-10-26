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

test_that("ggConfusionMatrix shows kappa as a coefficient, not a percentage", {
  skip_if_not_installed("caret")
  m <- caret::confusionMatrix(stats::predict(rf), iris$Species)
  title <- ggConfusionMatrix(m)$labels$title
  expect_match(title, sprintf("Kappa %.3f$", m$overall[["Kappa"]]))
  expect_false(grepl("Kappa.*%", title))
})

test_that("ggConfusionMatrix handles empty cells", {
  skip_if_not_installed("caret")
  lv <- c("a", "b", "c")
  pred <- factor(c("a", "a", "b", "b"), levels = lv)
  ref <- factor(c("a", "a", "b", "c"), levels = lv)
  p <- ggConfusionMatrix(caret::confusionMatrix(pred, ref))
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  expect_false(anyNA(fills))
})

test_that("num is validated", {
  for (bad in list(0, -1, 2.5, NA, "3", c(1, 2))) {
    expect_error(ggMeanDecreaseGini(rf, num = bad), "`num`")
  }
})

test_that("num larger than the number of features plots all of them", {
  p <- ggMeanDecreaseGini(rf, num = 100)
  expect_equal(nrow(p$data), nrow(rf$importance))
})

test_that("non-finite importance values are dropped", {
  rf_na <- rf
  rf_na$importance[1, "MeanDecreaseGini"] <- NA
  p <- ggMeanDecreaseGini(rf_na)
  expect_equal(nrow(p$data), nrow(rf$importance) - 1)
  rf_na$importance[, "MeanDecreaseGini"] <- NaN
  expect_error(ggMeanDecreaseGini(rf_na), "no finite")
})

test_that("objects that are not random forests give a clear error", {
  fit <- stats::lm(Sepal.Length ~ ., data = iris)
  expect_error(ggplotModel(fit), "'randomForest' object, not 'lm'")
  expect_error(ggMeanDecreaseGini(fit), "'randomForest' object")
  expect_error(ggMeanDecreaseAccuracy(fit), "'randomForest' object")
})
