# ggmodel 0.1.0

* First CRAN release.
* `ggMeanDecreaseGini()` and `ggMeanDecreaseAccuracy()` now work with any
  `randomForest` model, not only with `caret`'s `finalModel`.
  `ggMeanDecreaseAccuracy()` gives an error if the model was fitted without
  `importance = TRUE`, and `ggMeanDecreaseGini()` no longer needs it.
* `ggplotModel()` draws the OOB error in black (it got a class colour before)
  and gives an error for regression forests.
* `ggConfusionMatrix()` checks its input and no longer gives `-Inf` fills
  for empty cells.
* Updated to current 'ggplot2' (`linewidth`, `guides(fill = "none")`).
* 'caret' and 'reshape2' are no longer required: 'caret' is suggested.
