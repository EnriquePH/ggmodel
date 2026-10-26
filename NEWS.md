# ggmodel 0.1.0

* First CRAN release.
* `ggMeanDecreaseGini()` and `ggMeanDecreaseAccuracy()` now work with any
  `randomForest` model, not only with `caret`'s `finalModel`.
  `ggMeanDecreaseAccuracy()` gives an error if the model was fitted without
  `importance = TRUE`, and `ggMeanDecreaseGini()` no longer needs it.
* `ggplotModel()` draws the OOB error in black (it got a class colour before)
  and gives an error for regression forests.
* `ggConfusionMatrix()` shows Cohen's kappa as a coefficient (it was
  multiplied by 100 and shown as a percentage), checks its input and no
  longer gives `-Inf` fills for empty cells.
* The importance plots check `num`, plot all features when `num` is larger
  than their number, and drop non-finite importance values.
* All functions give a clear error for objects that are not random forests.
* The documentation explains the difference between the two importance
  measures and that neither is causal.
* Updated to current 'ggplot2' (`linewidth`, `guides(fill = "none")`).
* 'caret' and 'reshape2' are no longer required: 'caret' is suggested.
