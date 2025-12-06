/*
 * GGML/Tensor/fold_rules.cc
 *
 * GGML Tensor-based fold operation implementations
 */

#include "fold_rules.h"
#include <cmath>

namespace ggml {
namespace tensor {

// Explicit template instantiations
template float tensor_fold_op<float>(const float*, size_t, FoldType);
template double tensor_fold_op<double>(const double*, size_t, FoldType);
template int tensor_fold_op<int>(const int*, size_t, FoldType);

template float tensor_foldl<float>(float, const float*, size_t, float (*)(float, float));
template double tensor_foldl<double>(double, const double*, size_t, double (*)(double, double));

template float tensor_foldr<float>(float, const float*, size_t, float (*)(float, float));
template double tensor_foldr<double>(double, const double*, size_t, double (*)(double, double));

} // namespace tensor
} // namespace ggml
