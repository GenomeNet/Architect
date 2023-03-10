% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_model_genomenet.R
\name{create_model_genomenet}
\alias{create_model_genomenet}
\title{Create GenomeNet Model with Given Architecture Parameters}
\usage{
create_model_genomenet(
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
  num_targets = 2
)
}
\arguments{
\item{maxlen}{(integer `numeric(1)`)\cr
Input sequence length.}

\item{learning.rate}{(`numeric(1)`)\cr
Used by the `keras` optimizer that is specified by `optimizer`.}

\item{number_of_cnn_layers}{(integer `numeric(1)`)\cr
Target number of CNN-layers to use in total. If `number_of_cnn_layers` is
greater than `conv_block_count`, then the effective number of CNN layers
is set to the closest integer that is divisible by `conv_block_count`.}

\item{filters_0}{(`numeric(1)`)\cr
Target filter number of the first CNN-layer. Although CNN filter number is
always an integer, this value can be non-integer, potentially affecting
the filter-numbers of intermediate layers (which are geometrically
interpolated between `filters_0` and `filters_end`).\cr
Note that filters are constant within convolutional blocks when
`residual_block` is `TRUE`.}

\item{filters_end}{(`numeric(1)`)\cr
Target filter number of the last CNN-layer; ignored if only one CNN-layer
is used (i.e. if `number_of_cnn_layers` is 1). Although CNN filter number
is always an integer, this value can be non-integer, potentially affecting
the filter-numbers of intermediatdilation_ratese layers (which are geometrically
interpolated between `kernel_size_0` and `kernel_size_end`).\cr
Note that filters are constant within convolutional blocks when
`residual_block` is `TRUE`.}

\item{dilation_end}{(`numeric(1)`)\cr
Dilation of the last CNN-layer *within each block*. Dilation rates within
each convolutional block grows exponentially from 1 (no dilation) for the
first CNN-layer to each block, to this value. Set to 1 (default) to
disable dilation.}

\item{max_pool_end}{(`numeric(1)`)\cr
Target total effective pooling of CNN part of the network. "Effective
pooling" here is the product of the pooling rates of all previous
CNN-layers. A network with three CNN-layers, all of which are followed
by pooling layers of size 2, therefore has effective pooling of 8, with
the effecive pooling at imtermediate positions being 1 (beginning), 2,
and 4. Effecive pooling after each layer is set to the power of 2 that is,
on a logarithmic scale, closest to
`max_pool_end ^ (<CNN layer number> / <total number of CNN layers>)`.
Therefore, even though the total effective pooling size of teh whole
CNN part of the network will always be a power of 2, having different,
possibly non-integer values of `max_pool_end`, will still lead to
different networks.}

\item{dense_layer_num}{(integer `numeric(1)`)\cr
number of dense layers at the end of the network, not counting the output
layer.}

\item{dense_layer_units}{(integer `numeric(1)`)\cr
Number of units in each dense layer, except for the output layer.}

\item{dropout}{(`numeric(1)`)\cr
Dropout rate of dense layers, except for the output layer.}

\item{batch_norm_momentum}{(`numeric(1)`)\cr
`momentum`-parameter of `layer_batch_normalization` layers used in the
convolutional part of the network.}

\item{leaky_relu_alpha}{(`numeric(1)`)\cr
`alpha`-parameter of the `layer_activation_leaky_relu` activation layers
used in the convolutional part of the network.}

\item{dense_activation}{(`character(1)`)\cr
Which activation function to use for dense layers. Should be one of
`"relu"`, `"sigmoid"`, or `"tanh"`.}

\item{skip_block_fraction}{(`numeric(1)`)\cr
What fraction of the first convolutional blocks to skip.
Only used when `model_type` is `"gap"`.}

\item{residual_block}{(`logical(1)`)\cr
Whether to use residual layers in the convolutional part of the network.}

\item{reverse_encoding}{(`logical(1)`)\cr
Whether the network should have a second input for reverse-complement
sequences.}

\item{optimizer}{(`character(1)`)\cr
Which optimizer to use. One of `"adam"`, `"adagrad"`, `"rmsprop"`, or `"sgd"`.}

\item{model_type}{(`character(1)`)\cr
Whether to use the global average pooling (`"gap"`) or recurrent
(`"recurrent"`) model type.}

\item{recurrent_type}{(`character(1)`)\cr
Which recurrent network type to use. One of `"lstm"` or `"gru"`.
Only used when `model_type` is `"recurrent"`.}

\item{recurrent_layers}{(integer `numeric(1)`)\cr
Number of recurrent layers.
Only used when `model_type` is `"recurrent"`.}

\item{recurrent_bidirectional}{(`logical(1)`)\cr
Whether to use bidirectional recurrent layers.
Only used when `model_type` is `"recurrent"`.}

\item{recurrent_units}{(integer `numeric(1)`)\cr
Number of units in each recurrent layer.
Only used when `model_type` is `"recurrent"`.}

\item{vocabulary.size}{(integer `numeric(1)`)\cr
Vocabulary size of (one-hot encoded) input strings. This determines the
input tensor shape, together with `maxlen`.}

\item{num_targets}{(integer `numeric(1)`)\cr
Number of output units to create.}

\item{`conv_block_count`}{(integer `numeric(1)`)\cr
Number of convolutional blocks, into which the CNN layers are divided.
If this is greater than `number_of_cnn_layers`, then it is set to
`number_of_cnn_layers` (the convolutional block size will then be 1).\cr
Convolutional blocks are used when `model_type` is `"gap"` (the output of
the last `conv_block_count * (1 - skip_block_fraction)` blocks is
fed to global average pooling and then concatenated), and also when
`residual_block` is `TRUE` (the number of filters is held constant within
blocks). If neither of these is the case, `conv_block_count` has little
effect besides the fact that `number_of_cnn_layers` is set to the closest
integer divisible by `conv_block_count`.}

\item{`kernel_size_0`}{(`numeric(1)`)\cr
Target CNN kernel size of the first CNN-layer. Although CNN kernel size is
always an integer, this value can be non-integer, potentially affecting
the kernel-sizes of intermediate layers (which are geometrically
interpolated between `kernel_size_0` and `kernel_size_end`).}

\item{`kernel_size_end`}{(`numeric(1)`)\cr
Target CNN kernel size of the last CNN-layer; ignored if only one
CNN-layer is used (i.e. if `number_of_cnn_layers` is 1). Although CNN
kernel size is always an integer, this value can be non-integer,
potentially affecting the kernel-sizes of intermediate layers (which are
geometrically interpolated between `kernel_size_0` and `kernel_size_end`).}
}
\value{
A keras model.
}
\description{
Create a keras-model that can be trained with
[`deepG`][deepG::deepG-package].

The arguments of this function should be optimized, for example over the
space returned by [`get_searchspace()`].
}
