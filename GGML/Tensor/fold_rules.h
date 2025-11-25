/*
 * GGML/Tensor/fold_rules.h
 *
 * GGML Tensor-based fold operation implementations
 * Maps OpenCog fold operations to GGML tensor operations
 */

#ifndef _GGML_TENSOR_FOLD_RULES_H
#define _GGML_TENSOR_FOLD_RULES_H

#include <cstddef>
#include <cstdint>
#include <vector>

namespace ggml {
namespace tensor {

/**
 * @brief Tensor fold operation types
 */
enum class FoldType {
    FOLD_SUM,      // Sum reduction across tensor dimension
    FOLD_PRODUCT,  // Product reduction across tensor dimension
    FOLD_MAX,      // Max reduction across tensor dimension
    FOLD_MIN,      // Min reduction across tensor dimension
    FOLD_MEAN      // Mean reduction across tensor dimension
};

/**
 * @brief Tensor structure for fold operations
 */
template<typename T>
struct Tensor {
    T* data;              // Tensor data pointer
    size_t* shape;        // Tensor shape dimensions
    size_t ndim;          // Number of dimensions
    size_t total_size;    // Total number of elements
    
    Tensor() : data(nullptr), shape(nullptr), ndim(0), total_size(0) {}
    
    Tensor(size_t dims[], size_t num_dims) : ndim(num_dims) {
        shape = new size_t[ndim];
        total_size = 1;
        for (size_t i = 0; i < ndim; ++i) {
            shape[i] = dims[i];
            total_size *= dims[i];
        }
        data = new T[total_size];
        // Initialize to zero
        for (size_t i = 0; i < total_size; ++i) {
            data[i] = T();
        }
    }
    
    ~Tensor() {
        delete[] data;
        delete[] shape;
    }
};

/**
 * @brief Tensor fold operation - reduces tensor across specified dimension
 * @tparam T Data type (float, double, int, etc.)
 * @param input Input tensor data
 * @param len Length of input tensor
 * @param fold_type Type of fold operation
 * @return Folded result
 */
template<typename T>
T tensor_fold_op(const T* input, size_t len, FoldType fold_type) {
    if (len == 0) return T();
    
    T result;
    switch (fold_type) {
        case FoldType::FOLD_SUM:
            result = T();
            for (size_t i = 0; i < len; ++i) {
                result += input[i];
            }
            break;
            
        case FoldType::FOLD_PRODUCT:
            result = static_cast<T>(1);
            for (size_t i = 0; i < len; ++i) {
                result *= input[i];
            }
            break;
            
        case FoldType::FOLD_MAX:
            result = input[0];
            for (size_t i = 1; i < len; ++i) {
                if (input[i] > result) result = input[i];
            }
            break;
            
        case FoldType::FOLD_MIN:
            result = input[0];
            for (size_t i = 1; i < len; ++i) {
                if (input[i] < result) result = input[i];
            }
            break;
            
        case FoldType::FOLD_MEAN:
            result = T();
            for (size_t i = 0; i < len; ++i) {
                result += input[i];
            }
            result /= static_cast<T>(len);
            break;
            
        default:
            result = T();
    }
    
    return result;
}

/**
 * @brief Left fold (foldl) operation on tensor
 * Performs: f(f(f(v, a), b), c) for list [a, b, c]
 * @tparam T Data type
 * @param init Initial value
 * @param data Input tensor data
 * @param len Length of tensor
 * @param fold_fn Folding function pointer
 * @return Folded result
 */
template<typename T>
T tensor_foldl(T init, const T* data, size_t len, T (*fold_fn)(T, T)) {
    T result = init;
    for (size_t i = 0; i < len; ++i) {
        result = fold_fn(result, data[i]);
    }
    return result;
}

/**
 * @brief Right fold (foldr) operation on tensor
 * Performs: f(a, f(b, f(c, v))) for list [a, b, c]
 * @tparam T Data type
 * @param init Initial value
 * @param data Input tensor data
 * @param len Length of tensor
 * @param fold_fn Folding function pointer
 * @return Folded result
 */
template<typename T>
T tensor_foldr(T init, const T* data, size_t len, T (*fold_fn)(T, T)) {
    T result = init;
    for (size_t i = len; i > 0; --i) {
        result = fold_fn(data[i-1], result);
    }
    return result;
}

/**
 * @brief Fold tensor along specified axis
 * @tparam T Data type
 * @param input Input tensor
 * @param axis Axis to fold along (0-indexed)
 * @param fold_type Type of fold operation
 * @return Output tensor with reduced dimension
 */
template<typename T>
Tensor<T>* tensor_fold_axis(const Tensor<T>* input, size_t axis, FoldType fold_type) {
    if (axis >= input->ndim) return nullptr;
    
    // Calculate output tensor dimensions
    size_t* out_shape = new size_t[input->ndim - 1];
    size_t out_idx = 0;
    for (size_t i = 0; i < input->ndim; ++i) {
        if (i != axis) {
            out_shape[out_idx++] = input->shape[i];
        }
    }
    
    Tensor<T>* output = new Tensor<T>(out_shape, input->ndim - 1);
    delete[] out_shape;
    
    // Perform fold operation along specified axis
    size_t stride = 1;
    for (size_t i = axis + 1; i < input->ndim; ++i) {
        stride *= input->shape[i];
    }
    
    size_t fold_dim = input->shape[axis];
    size_t outer_stride = stride * fold_dim;
    
    for (size_t outer = 0; outer < input->total_size; outer += outer_stride) {
        for (size_t inner = 0; inner < stride; ++inner) {
            size_t out_idx = (outer / outer_stride) * stride + inner;
            
            // Collect elements to fold
            std::vector<T> fold_data;
            for (size_t i = 0; i < fold_dim; ++i) {
                fold_data.push_back(input->data[outer + i * stride + inner]);
            }
            
            output->data[out_idx] = tensor_fold_op(fold_data.data(), fold_data.size(), fold_type);
        }
    }
    
    return output;
}

/**
 * @brief Unroll fold operation into explicit tensor operations
 * Converts fold into a sequence of tensor operations for optimization
 * @tparam T Data type
 * @param input Input tensor
 * @param fold_type Type of fold operation
 * @return Unrolled tensor operation result
 */
template<typename T>
T tensor_fold_unroll(const Tensor<T>* input, FoldType fold_type) {
    return tensor_fold_op(input->data, input->total_size, fold_type);
}

// Explicit template instantiations for common types
extern template float tensor_fold_op<float>(const float*, size_t, FoldType);
extern template double tensor_fold_op<double>(const double*, size_t, FoldType);
extern template int tensor_fold_op<int>(const int*, size_t, FoldType);

extern template float tensor_foldl<float>(float, const float*, size_t, float (*)(float, float));
extern template double tensor_foldl<double>(double, const double*, size_t, double (*)(double, double));

extern template float tensor_foldr<float>(float, const float*, size_t, float (*)(float, float));
extern template double tensor_foldr<double>(double, const double*, size_t, double (*)(double, double));

} // namespace tensor
} // namespace ggml

#endif // _GGML_TENSOR_FOLD_RULES_H
