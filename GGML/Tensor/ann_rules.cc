/*
 * GGML/Tensor/ann_rules.cc
 *
 * GGML Tensor-based ANN rule implementations
 */

#include "ann_rules.h"
#include <cmath>
#include <algorithm>

namespace ggml {
namespace tensor {

// Explicit template instantiations
template void activate_tensor<float>(float*, const float*, size_t, ActivationType, float);
template void activate_tensor<double>(double*, const double*, size_t, ActivationType, double);

template void neural_matmul<float>(float*, const float*, const float*, size_t, size_t, size_t);
template void neural_matmul<double>(double*, const double*, const double*, size_t, size_t, size_t);

template void neural_dense_forward<float>(float*, const float*, const float*, const float*,
                                         size_t, size_t, size_t, ActivationType);
template void neural_dense_forward<double>(double*, const double*, const double*, const double*,
                                          size_t, size_t, size_t, ActivationType);

} // namespace tensor
} // namespace ggml
