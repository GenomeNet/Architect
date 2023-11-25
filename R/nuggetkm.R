
# @title Kriging Model Predictor with Epistemic Uncertainty Prediction
#
# @description
# The default "regr.km" Learner predicts the total (epistemic + aleatoric) uncertainty for `"se"` prediction.
# The `"regr.nuggetkm"` subtracts the nugget (aleatoric uncertainty) variance, predicting only the epistemic
# uncertainty instead.

#' @export
predictLearner.regr.nuggetkm <- function(.learner, .model, ...) {
  res <- NextMethod()
  res[, 2] <- sqrt(pmax(res[, 2]^2 - .model$learner.model@covariance@nugget, 0))
  res
}