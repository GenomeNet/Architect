#' @title Get Search Space for Genome Sequence Models.
#'
#' @description
#' Create a search space over which the model created by
#' [`create_model_genomenet()`] can be tuned.
#'
#' Even though some of the hyperparameters of the network are categorical,
#' this creates a purely numeric search space, which can be optimized over
#' more easily. It is therefore necessary to convert sampled values
#' using [`cleanUpX()`] before calling [`create_model_genomenet()`].
#'
#' @param maxlen (integer `numeric(1)`)\cr
#'   Input sequence length. This value determines the upper bounds of some
#'   of the search intervals.
#' @param type (`character(1)`)\cr
#'   One of `"gap"` or `"recurrent`. Used to include some of the model-type
#'   specific search space components.
#' @return a [`ParamSet`].
#' @export
get_searchspace <- function(maxlen, type) {

  assertCount(maxlen)
  assertChoice(type, c("gap", "recurrent"))

  # not optimized:
  #  - residual_block
  #  - sequence_length
  #  - type ("gap", "recurrent")

  searchspace = pSS(
    learning.rate: numeric[2, 6] [[trafo = function(x) 10^(-x)]],
    dropout: numeric[0, 0.9],
    filters_0: numeric[4, 11] [[trafo = function(x) 2^x]],
    filters_end: numeric[4, 11] [[trafo = function(x) 2^x]],
    kernel_size_0: numeric[1, 6] [[trafo = function(x) 2^x]],
    kernel_size_end: numeric[1, 6] [[trafo = function(x) 2^x]],
    max_pool_end: numeric[0, ceiling(log2(maxlen) / 2)] [[trafo = function(x) 2^x]],
    number_of_cnn_layers: integer[1, 20],
    conv_block_count: integer[1, 10],
    dilation_end: numeric[0, ceiling(log2(maxlen) / 2)] [[trafo = function(x) 2^x]],
    dense_layer_num: integer[0, 5],
    dense_layer_units: numeric[4, 11] [[trafo = function(x) floor(2^x)]],
    batch_norm_momentum: numeric[0, 0.99],
    leaky_relu_alpha: numeric[0, 1],
    dense_activation: integer[0, 1], # 0 is relu, 1 is tanh, sigmoid was removed
    optimizer: integer[0, 1], # 0 is adam and 1 is adagrad
    reverse_encoding: integer[0, 1]
  )

  if (type == "recurrent") {
    searchspace <- c(searchspace, pSS(
      recurrent_type: integer[0, 1], # 0 is lstm and 1 is gru
      recurrent_layers: integer[1, 3],
      recurrent_bidirectional: integer[0, 1],
      recurrent_units: numeric[4, 11] [[trafo = function(x) round(2^x)]]
    ))
  } else {
    searchspace <- c(searchspace, pSS(
      skip_block_fraction: numeric[0, 1]
    ))
  }

  return(searchspace)
}

#' @title Clean up Sampled Values for [`create_model_genomenet()`]
#'
#' @description
#' Even though some of the hyperparameters of the [`create_model_genomenet()`]
#' network are categorical, it is easier to use a urely numeric search space,
#' e.g. the one created by [`get_searchspace()`]. `cleanUpX()` converts
#' numeric values to the right type for [`create_model_genomenet()`].
#'
#' @param x (`list`)\cr
#'   List of numeric hyperparameter values for [`create_model_genomenet()`].
#' @param num_targets (integer `numeric(1)`)\cr
#'   Value to set the `num_targets` entry in the resulting `list` to.
#' @return `list`: the converted hyperparameter values.
#' @export
cleanUpX <- function(x, num_targets) {
  x$walltime <- NULL

  x$num_targets <- num_targets

  # Recoding (avoiding discrete parameters to use the GP)
  x$residual_block <- as.logical(x$residual_block)
  x$optimizer <- ifelse(x$optimizer == 0, "adam", "adagrad")
  x$dense_activation <- ifelse(x$dense_activation == 0, "relu", "tanh")
  x$reverse_encoding <- as.logical(x$reverse_encoding)
  if (!is.null(x$recurrent_type)) x$recurrent_type = ifelse(x$recurrent_type == 0, "lstm", "gru")
  if (!is.null(x$recurrent_layers)) x$recurrent_bidirectional = as.logical(x$recurrent_bidirectional)
  x$model_type <- if (is.null(x$recurrent_type)) "gap" else "recurrent"

  # make these integer
  if (!is.null(x$maxlen)) x$maxlen <- round(x$maxlen)
  x$dense_layer_units = round(x$dense_layer_units)
  if (!is.null(x$recurrent_layers)) x$recurrent_layers = round(x$recurrent_layers)
  x
}
