#' @title Create GenomeNet Model with Given Architecture Parameters
#'
#' @description
#' Create a keras-model that can be trained with
#' [`deepG`][deepG::deepG-package].
#'
#' The arguments of this function should be optimized, for example over the
#' space returned by [`get_searchspace()`].
#'
#' @param maxlen (integer `numeric(1)`)\cr
#'   Input sequence length.
#' @param learning.rate (`numeric(1)`)\cr
#'   Used by the `keras` optimizer that is specified by `optimizer`.
#' @param number_of_cnn_layers (integer `numeric(1)`)\cr
#'   Target number of CNN-layers to use in total. If `number_of_cnn_layers` is
#'   greater than `conv_block_count`, then the effective number of CNN layers
#'   is set to the closest integer that is divisible by `conv_block_count`.
#' @param `conv_block_count` (integer `numeric(1)`)\cr
#'   Number of convolutional blocks, into which the CNN layers are divided.
#'   If this is greater than `number_of_cnn_layers`, then it is set to
#'   `number_of_cnn_layers` (the convolutional block size will then be 1).\cr
#'   Convolutional blocks are used when `model_type` is `"gap"` (the output of
#'   the last `conv_block_count * (1 - skip_block_fraction)` blocks is
#'   fed to global average pooling and then concatenated), and also when
#'   `residual_block` is `TRUE` (the number of filters is held constant within
#'   blocks). If neither of these is the case, `conv_block_count` has little
#'   effect besides the fact that `number_of_cnn_layers` is set to the closest
#'   integer divisible by `conv_block_count`.
#' @param `kernel_size_0` (`numeric(1)`)\cr
#'   Target CNN kernel size of the first CNN-layer. Although CNN kernel size is
#'   always an integer, this value can be non-integer, potentially affecting
#'   the kernel-sizes of intermediate layers (which are geometrically
#'   interpolated between `kernel_size_0` and `kernel_size_end`).
#' @param `kernel_size_end` (`numeric(1)`)\cr
#'   Target CNN kernel size of the last CNN-layer; ignored if only one
#'   CNN-layer is used (i.e. if `number_of_cnn_layers` is 1). Although CNN
#'   kernel size is always an integer, this value can be non-integer,
#'   potentially affecting the kernel-sizes of intermediate layers (which are
#'   geometrically interpolated between `kernel_size_0` and `kernel_size_end`).
#' @param filters_0 (`numeric(1)`)\cr
#'   Target filter number of the first CNN-layer. Although CNN filter number is
#'   always an integer, this value can be non-integer, potentially affecting
#'   the filter-numbers of intermediate layers (which are geometrically
#'   interpolated between `filters_0` and `filters_end`).\cr
#'   Note that filters are constant within convolutional blocks when
#'   `residual_block` is `TRUE`.
#' @param filters_end (`numeric(1)`)\cr
#'   Target filter number of the last CNN-layer; ignored if only one CNN-layer
#'   is used (i.e. if `number_of_cnn_layers` is 1). Although CNN filter number
#'   is always an integer, this value can be non-integer, potentially affecting
#'   the filter-numbers of intermediatdilation_ratese layers (which are geometrically
#'   interpolated between `kernel_size_0` and `kernel_size_end`).\cr
#'   Note that filters are constant within convolutional blocks when
#'   `residual_block` is `TRUE`.
#' @param dilation_end (`numeric(1)`)\cr
#'   Dilation of the last CNN-layer *within each block*. Dilation rates within
#'   each convolutional block grows exponentially from 1 (no dilation) for the
#'   first CNN-layer to each block, to this value. Set to 1 (default) to
#'   disable dilation.
#' @param max_pool_end (`numeric(1)`)\cr
#'   Target total effective pooling of CNN part of the network. "Effective
#'   pooling" here is the product of the pooling rates of all previous
#'   CNN-layers. A network with three CNN-layers, all of which are followed
#'   by pooling layers of size 2, therefore has effective pooling of 8, with
#'   the effecive pooling at imtermediate positions being 1 (beginning), 2,
#'   and 4. Effecive pooling after each layer is set to the power of 2 that is,
#'   on a logarithmic scale, closest to
#'   `max_pool_end ^ (<CNN layer number> / <total number of CNN layers>)`.
#'   Therefore, even though the total effective pooling size of teh whole
#'   CNN part of the network will always be a power of 2, having different,
#'   possibly non-integer values of `max_pool_end`, will still lead to
#'   different networks.
#' @param dense_layer_num (integer `numeric(1)`)\cr
#'   number of dense layers at the end of the network, not counting the output
#'   layer.
#' @param dense_layer_units (integer `numeric(1)`)\cr
#'   Number of units in each dense layer, except for the output layer.
#' @param dropout (`numeric(1)`)\cr
#'   Dropout rate of dense layers, except for the output layer.
#' @param batch_norm_momentum (`numeric(1)`)\cr
#'   `momentum`-parameter of `layer_batch_normalization` layers used in the
#'   convolutional part of the network.
#' @param leaky_relu_alpha (`numeric(1)`)\cr
#'   `alpha`-parameter of the `layer_activation_leaky_relu` activation layers
#'   used in the convolutional part of the network.
#' @param dense_activation (`character(1)`)\cr
#'   Which activation function to use for dense layers. Should be one of
#'   `"relu"`, `"sigmoid"`, or `"tanh"`.
#' @param skip_block_fraction (`numeric(1)`)\cr
#'   What fraction of the first convolutional blocks to skip.
#'   Only used when `model_type` is `"gap"`.
#' @param residual_block (`logical(1)`)\cr
#'   Whether to use residual layers in the convolutional part of the network.
#' @param reverse_encoding (`logical(1)`)\cr
#'   Whether the network should have a second input for reverse-complement
#'   sequences.
#' @param optimizer (`character(1)`)\cr
#'   Which optimizer to use. One of `"adam"`, `"adagrad"`, `"rmsprop"`, or `"sgd"`.
#' @param model_type (`character(1)`)\cr
#'   Whether to use the global average pooling (`"gap"`) or recurrent
#'   (`"recurrent"`) model type.
#' @param recurrent_type (`character(1)` | `function`)\cr
#'   Which recurrent network type to use. One of `"lstm"` or `"gru"`.
#'   Alternatively a function similar to `keras::layer_lstm` with arguments `units`
#'   and `return_sequences` that constructs a `keras` layer.
#'   Only used when `model_type` is `"recurrent"`.
#' @param recurrent_layers (integer `numeric(1)`)\cr
#'   Number of recurrent layers.
#'   Only used when `model_type` is `"recurrent"`.
#' @param recurrent_bidirectional (`logical(1)`)\cr
#'   Whether to use bidirectional recurrent layers.
#'   Only used when `model_type` is `"recurrent"`.
#' @param recurrent_units (integer `numeric(1)`)\cr
#'   Number of units in each recurrent layer.
#'   Only used when `model_type` is `"recurrent"`.
#' @param vocabulary.size (integer `numeric(1)`)\cr
#'   Vocabulary size of (one-hot encoded) input strings. This determines the
#'   input tensor shape, together with `maxlen`.
#' @param num_targets (integer `numeric(1)`)\cr
#'   Number of output units to create.
#' @param output_activation (`character(1)`)\cr
#'   Which activation function to use for the output layer. This is passed to
#'   `keras::layer_dense` as the `activation` argument.
#'   This is usually `"softmax"` for classification, and `"linear"` for
#'   regression.
#' @param metrics (`character(1)`)\cr
#'   Which metrics to use for training. This is passed to `keras::compile` as
#'   the `metrics` argument.
#' @param loss (`character(1)`)\cr
#'   Which loss function to use for training. This is passed to `keras::compile`
#'   as the `loss` argument.
#' @return A keras model.
#' @export
create_model_genomenet <- function(
  maxlen = 300,
  learning.rate = 0.001,
  number_of_cnn_layers = 1,
  conv_block_count = 1,
  kernel_size_0 = 16,
  kernel_size_end = 16,
  filters_0 = 256,
  filters_end = 512,
  dilation_end = 1,
  max_pool_end = 1,
  dense_layer_num = 1,
  dense_layer_units = 100,
  dropout = 0,
  batch_norm_momentum = 0.8,
  leaky_relu_alpha = 0,
  dense_activation = "relu",
  skip_block_fraction = 0,
  residual_block = FALSE,
  reverse_encoding = FALSE,
  optimizer = "adam",
  model_type = "gap",
  recurrent_type = "lstm",
  recurrent_layers = 1,
  recurrent_bidirectional = FALSE,
  recurrent_units = 100,
  vocabulary.size = 4,
  num_targets = 2,
  output_activation = "softmax",
  metrics = "acc", # TODO: NULL for regression? or smth else? DO WE NEED THIS?
  loss = "categorical_crossentropy" # TODO: "mse" for regression?
) {

  assertCount(maxlen, positive = TRUE, tol = 0)
  assertNumber(learning.rate, lower = .Machine$double.xmin, finite = TRUE)
  assertCount(number_of_cnn_layers, positive = TRUE, tol = 0)
  assertCount(conv_block_count, positive = TRUE, tol = 0)
  assertNumber(kernel_size_0, lower = .Machine$double.xmin, finite = TRUE)
  assertNumber(kernel_size_end, lower = .Machine$double.xmin, finite = TRUE)
  assertNumber(filters_0, lower = .Machine$double.xmin, finite = TRUE)
  assertNumber(filters_end, lower = .Machine$double.xmin, finite = TRUE)
  assertNumber(dilation_end, lower = .Machine$double.xmin, finite = TRUE)
  assertNumber(max_pool_end, lower = .Machine$double.xmin, finite = TRUE)
  assertCount(dense_layer_num, tol = 0)
  assertCount(dense_layer_units, tol = 0)
  assertNumber(dropout, lower = 0, upper = 1)
  assertNumber(batch_norm_momentum, lower = 0, upper = 1)
  assertNumber(leaky_relu_alpha, lower = 0, upper = 1)
  assertChoice(dense_activation, c("relu", "sigmoid", "tanh"))
  assertNumber(skip_block_fraction, lower = 0, upper = 1)
  assertFlag(residual_block)
  assertFlag(reverse_encoding)
  assertChoice(optimizer, c("adam", "adagrad", "rmsprop", "sgd"))
  assertChoice(model_type, c("gap", "recurrent"))
  assert(checkChoice(recurrent_type, c("lstm", "gru")), checkFunction(recurrent_type))
  assertCount(recurrent_layers, positive = TRUE, tol = 0)
  assertFlag(recurrent_bidirectional)
  assertCount(recurrent_units, positive = TRUE, tol = 0)
  assertInt(vocabulary.size, lower = 2, tol = 0)
  assertCount(num_targets, positive = TRUE, tol = 0)
  assertString(output_activation)

  if (number_of_cnn_layers < conv_block_count){
    conv_block_size <- 1
    conv_block_count <- number_of_cnn_layers
  } else {
    conv_block_size <- round(number_of_cnn_layers / conv_block_count)
    number_of_cnn_layers <- conv_block_size * conv_block_count
  }

  if (residual_block) {
    filters_exponent <- rep(seq(from = 0, to = 1, length.out = conv_block_count), each = conv_block_size)
  } else {
    filters_exponent <- seq(from = 0, to = 1, length.out = number_of_cnn_layers)
  }

  filters <- ceiling(filters_0 * (filters_end / filters_0) ^ filters_exponent)

  kernel_size <- ceiling(kernel_size_0 * (kernel_size_end / kernel_size_0) ^ seq(from = 0, to = 1, length.out = number_of_cnn_layers))

  dilation_rates <- round(dilation_end ^ seq(0, 1, length.out = conv_block_size))
  dilation_rates <- rep(dilation_rates, conv_block_count)

  max_pool_divider <- round(log2(max_pool_end) * seq(0, 1, length.out = number_of_cnn_layers + 1))
  max_pool_array <- 2 ^ diff(max_pool_divider)

  input_tensor <- keras::layer_input(shape = c(maxlen, vocabulary.size))
  output_tensor <- input_tensor

  output_collection <- list()

  for (i in seq_len(number_of_cnn_layers)) {
    layer <- keras::layer_conv_1d(kernel_size = kernel_size[i],
                                  padding = "same",
                                  activation = "linear",
                                  filters = filters[i],
                                  dilation_rate = dilation_rates[i])

    output_tensor <- output_tensor %>% layer

    if (model_type == "gap" && i %% conv_block_size == 0) {
      output_collection[[length(output_collection) + 1]] <- keras::layer_global_average_pooling_1d(output_tensor)
    }

    if (max_pool_array[i] > 1) {
      layer <- keras::layer_max_pooling_1d(pool_size = max_pool_array[i], padding = "same")
      output_tensor <- output_tensor %>% layer
    }

    if (residual_block) {
      if (i > 1) {
        if (max_pool_array[i] > 1) {
          layer <- keras::layer_average_pooling_1d(pool_size = max_pool_array[i], padding = "same")
          residual_layer <- residual_layer %>% layer
        }
        if (filters[i - 1] != filters[i]) {
          layer <- keras::layer_conv_1d(kernel_size = 1,
                                        padding = "same",
                                        activation = "linear",
                                        filters = filters[i]
                                        )
          residual_layer <- residual_layer %>% layer
        }

        output_tensor <- keras::layer_add(list(output_tensor, residual_layer))
      }

      residual_layer <- output_tensor
    }

    layer <- keras::layer_batch_normalization(momentum = batch_norm_momentum)
    output_tensor <- output_tensor %>% layer

    layer <- keras::layer_activation_leaky_relu(alpha = leaky_relu_alpha)

    output_tensor <- output_tensor %>% layer
  }

  if (model_type == "gap") {
    # skip 'skip_block_fraction' of outputs we collected --> use the last (1 - skip_block_fraction) part of them
    use_blocks <- ceiling((1 - skip_block_fraction) * length(output_collection))
    use_blocks <- max(use_blocks, 1)

    output_collection <- tail(output_collection, use_blocks)

    # concatenate outputs from blocks (that we are using)
    if (length(output_collection) > 1) {
      output_tensor <- keras::layer_concatenate(output_collection)
    } else {
      output_tensor <- output_collection[[1]]
    }
  } else {
    # recurrent model

    recurrent_layer_constructor = switch(recurrent_type,
      lstm = keras::layer_lstm,
      gru = keras::layer_gru
    )

    for (i in seq_len(recurrent_layers)) {
      if (recurrent_bidirectional) {
        layer <- keras::bidirectional(
            layer = recurrent_layer_constructor(units = recurrent_units, return_sequences = (i != recurrent_layers)))
      } else {
        layer <- recurrent_layer_constructor(units = recurrent_units, return_sequences = (i != recurrent_layers))
      }
      output_tensor <- output_tensor %>% layer
    }
  }

  for (i in seq_len(dense_layer_num)) {
    layer <- keras::layer_dropout(rate = dropout)
    output_tensor <- output_tensor %>% layer
    layer <- keras::layer_dense(units = dense_layer_units, activation = dense_activation)
    output_tensor <- output_tensor %>% layer
  }

  output_tensor <- output_tensor %>%
    keras::layer_dense(units = num_targets, activation = output_activation)

  # define "model" as the mapping from input_tensor to output_tensor
  model <- keras::keras_model(inputs = input_tensor, outputs = output_tensor)

  if (reverse_encoding) {
    input_tensor_reversed <- keras::layer_input(shape = c(maxlen, vocabulary.size))

    # define "output_tensor_reversed" as what comes out of input_tensor_reversed when model() is applied to it
    output_tensor_reversed <- model(input_tensor_reversed)

    # define a new model: model from above (with input_tensor, output_tensor), and
    model <- keras::keras_model(
      inputs = c(input_tensor, input_tensor_reversed),
      outputs = keras::layer_average(c(output_tensor, output_tensor_reversed))
    )
  }
  # assign optimization method

  keras_optimizer <- switch(optimizer,
    adam = keras::optimizer_adam(learning_rate = learning.rate),
    adagrad = keras::optimizer_adagrad(learning_rate = learning.rate),
    rmsprop = keras::optimizer_rmsprop(learning_rate = learning.rate),
    sgd = keras::optimizer_sgd(learning_rate = learning.rate)
  )

  model %>% keras::compile(loss = loss, optimizer = keras_optimizer, metrics = metrics)

  args <- formals()
  given_args <- as.list(match.call()[-1])
  args[names(given_args)] <- given_args

  args$trainable_params <- model$count_params()

  model$hparam <- args

  model
}
