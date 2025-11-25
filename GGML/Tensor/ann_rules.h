/*
 * GGML/Tensor/ann_rules.h
 *
 * GGML Tensor-based Artificial Neural Network (ANN) rule implementations
 * Neural-symbolic integration for OpenCog cognitive architecture
 */

#ifndef _GGML_TENSOR_ANN_RULES_H
#define _GGML_TENSOR_ANN_RULES_H

#include <cstddef>
#include <cstdint>
#include <cmath>
#include <vector>

namespace ggml {
namespace tensor {

/**
 * @brief Neural activation function types
 */
enum class ActivationType {
    SIGMOID,      // Sigmoid: 1 / (1 + exp(-x))
    TANH,         // Hyperbolic tangent: tanh(x)
    RELU,         // Rectified Linear Unit: max(0, x)
    LEAKY_RELU,   // Leaky ReLU: x if x > 0 else alpha*x
    ELU,          // Exponential Linear Unit
    SOFTMAX,      // Softmax: exp(x_i) / sum(exp(x_j))
    LINEAR        // Linear (identity): x
};

/**
 * @brief Neural network layer types
 */
enum class LayerType {
    DENSE,        // Fully connected layer
    CONV,         // Convolutional layer
    POOL,         // Pooling layer
    RECURRENT,    // Recurrent (RNN) layer
    ATTENTION     // Attention mechanism layer
};

/**
 * @brief Tensor structure for neural operations
 */
template<typename T>
struct NeuralTensor {
    T* data;              // Tensor data
    size_t* shape;        // Tensor shape
    size_t ndim;          // Number of dimensions
    size_t total_size;    // Total elements
    
    NeuralTensor() : data(nullptr), shape(nullptr), ndim(0), total_size(0) {}
    
    NeuralTensor(size_t dims[], size_t num_dims) : ndim(num_dims) {
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
    
    ~NeuralTensor() {
        delete[] data;
        delete[] shape;
    }
};

/**
 * @brief Apply neural activation function to tensor
 * @tparam T Data type (float, double)
 * @param output Output tensor (pre-allocated)
 * @param input Input tensor
 * @param len Length of tensor
 * @param activation Activation function type
 * @param alpha Parameter for leaky ReLU (default 0.01)
 */
template<typename T>
void activate_tensor(T* output, const T* input, size_t len, 
                    ActivationType activation, T alpha = static_cast<T>(0.01)) {
    switch (activation) {
        case ActivationType::SIGMOID:
            for (size_t i = 0; i < len; ++i) {
                output[i] = static_cast<T>(1) / (static_cast<T>(1) + std::exp(-input[i]));
            }
            break;
            
        case ActivationType::TANH:
            for (size_t i = 0; i < len; ++i) {
                output[i] = std::tanh(input[i]);
            }
            break;
            
        case ActivationType::RELU:
            for (size_t i = 0; i < len; ++i) {
                output[i] = (input[i] > static_cast<T>(0)) ? input[i] : static_cast<T>(0);
            }
            break;
            
        case ActivationType::LEAKY_RELU:
            for (size_t i = 0; i < len; ++i) {
                output[i] = (input[i] > static_cast<T>(0)) ? input[i] : alpha * input[i];
            }
            break;
            
        case ActivationType::ELU:
            for (size_t i = 0; i < len; ++i) {
                output[i] = (input[i] > static_cast<T>(0)) ? 
                           input[i] : alpha * (std::exp(input[i]) - static_cast<T>(1));
            }
            break;
            
        case ActivationType::SOFTMAX: {
            // Find max for numerical stability
            T max_val = input[0];
            for (size_t i = 1; i < len; ++i) {
                if (input[i] > max_val) max_val = input[i];
            }
            
            // Compute exp and sum
            T sum = static_cast<T>(0);
            for (size_t i = 0; i < len; ++i) {
                output[i] = std::exp(input[i] - max_val);
                sum += output[i];
            }
            
            // Normalize
            for (size_t i = 0; i < len; ++i) {
                output[i] /= sum;
            }
            break;
        }
            
        case ActivationType::LINEAR:
        default:
            for (size_t i = 0; i < len; ++i) {
                output[i] = input[i];
            }
            break;
    }
}

/**
 * @brief Compute activation derivative for backpropagation
 * @tparam T Data type
 * @param output Output derivative tensor
 * @param input Input tensor (or activation output for some functions)
 * @param len Length of tensor
 * @param activation Activation function type
 * @param alpha Parameter for leaky ReLU
 */
template<typename T>
void activate_tensor_derivative(T* output, const T* input, size_t len,
                               ActivationType activation, T alpha = static_cast<T>(0.01)) {
    switch (activation) {
        case ActivationType::SIGMOID:
            // sigmoid'(x) = sigmoid(x) * (1 - sigmoid(x))
            for (size_t i = 0; i < len; ++i) {
                T sig = static_cast<T>(1) / (static_cast<T>(1) + std::exp(-input[i]));
                output[i] = sig * (static_cast<T>(1) - sig);
            }
            break;
            
        case ActivationType::TANH:
            // tanh'(x) = 1 - tanh^2(x)
            for (size_t i = 0; i < len; ++i) {
                T tanh_val = std::tanh(input[i]);
                output[i] = static_cast<T>(1) - tanh_val * tanh_val;
            }
            break;
            
        case ActivationType::RELU:
            for (size_t i = 0; i < len; ++i) {
                output[i] = (input[i] > static_cast<T>(0)) ? static_cast<T>(1) : static_cast<T>(0);
            }
            break;
            
        case ActivationType::LEAKY_RELU:
            for (size_t i = 0; i < len; ++i) {
                output[i] = (input[i] > static_cast<T>(0)) ? static_cast<T>(1) : alpha;
            }
            break;
            
        case ActivationType::ELU:
            for (size_t i = 0; i < len; ++i) {
                output[i] = (input[i] > static_cast<T>(0)) ? 
                           static_cast<T>(1) : alpha * std::exp(input[i]);
            }
            break;
            
        case ActivationType::LINEAR:
        default:
            for (size_t i = 0; i < len; ++i) {
                output[i] = static_cast<T>(1);
            }
            break;
    }
}

/**
 * @brief Matrix multiplication for neural network layers
 * C = A * B where A is [m x k] and B is [k x n]
 * @tparam T Data type
 * @param C Output matrix [m x n]
 * @param A Input matrix [m x k]
 * @param B Weight matrix [k x n]
 * @param m Number of rows in A
 * @param k Number of columns in A (rows in B)
 * @param n Number of columns in B
 */
template<typename T>
void neural_matmul(T* C, const T* A, const T* B, size_t m, size_t k, size_t n) {
    for (size_t i = 0; i < m; ++i) {
        for (size_t j = 0; j < n; ++j) {
            T sum = static_cast<T>(0);
            for (size_t p = 0; p < k; ++p) {
                sum += A[i * k + p] * B[p * n + j];
            }
            C[i * n + j] = sum;
        }
    }
}

/**
 * @brief Add bias to neural network layer output
 * @tparam T Data type
 * @param output Output tensor (modified in place)
 * @param bias Bias vector
 * @param m Number of samples
 * @param n Number of features per sample
 */
template<typename T>
void neural_add_bias(T* output, const T* bias, size_t m, size_t n) {
    for (size_t i = 0; i < m; ++i) {
        for (size_t j = 0; j < n; ++j) {
            output[i * n + j] += bias[j];
        }
    }
}

/**
 * @brief Forward pass through a dense neural network layer
 * @tparam T Data type
 * @param output Output tensor [batch_size x output_dim]
 * @param input Input tensor [batch_size x input_dim]
 * @param weights Weight matrix [input_dim x output_dim]
 * @param bias Bias vector [output_dim]
 * @param batch_size Number of samples in batch
 * @param input_dim Input dimension
 * @param output_dim Output dimension
 * @param activation Activation function to apply
 */
template<typename T>
void neural_dense_forward(T* output, const T* input, const T* weights,
                         const T* bias, size_t batch_size, size_t input_dim,
                         size_t output_dim, ActivationType activation) {
    // Matrix multiplication: output = input * weights
    neural_matmul(output, input, weights, batch_size, input_dim, output_dim);
    
    // Add bias
    if (bias != nullptr) {
        neural_add_bias(output, bias, batch_size, output_dim);
    }
    
    // Apply activation
    activate_tensor(output, output, batch_size * output_dim, activation);
}

/**
 * @brief Apply batch normalization to tensor
 * @tparam T Data type
 * @param output Normalized output tensor
 * @param input Input tensor
 * @param mean Batch mean
 * @param variance Batch variance
 * @param gamma Scale parameter
 * @param beta Shift parameter
 * @param len Length of tensor
 * @param epsilon Small constant for numerical stability
 */
template<typename T>
void neural_batch_norm(T* output, const T* input, const T* mean,
                      const T* variance, const T* gamma, const T* beta,
                      size_t len, T epsilon = static_cast<T>(1e-5)) {
    for (size_t i = 0; i < len; ++i) {
        T normalized = (input[i] - mean[i]) / std::sqrt(variance[i] + epsilon);
        output[i] = gamma[i] * normalized + beta[i];
    }
}

/**
 * @brief Dropout operation for regularization
 * @tparam T Data type
 * @param output Output tensor
 * @param input Input tensor
 * @param mask Dropout mask (0 or 1)
 * @param len Length of tensor
 * @param keep_prob Probability of keeping a neuron
 */
template<typename T>
void neural_dropout(T* output, const T* input, const uint8_t* mask,
                   size_t len, T keep_prob) {
    T scale = static_cast<T>(1) / keep_prob;
    for (size_t i = 0; i < len; ++i) {
        output[i] = mask[i] ? input[i] * scale : static_cast<T>(0);
    }
}

/**
 * @brief Compute cross-entropy loss
 * @tparam T Data type
 * @param predictions Predicted probabilities [batch_size x num_classes]
 * @param targets One-hot encoded targets [batch_size x num_classes]
 * @param batch_size Number of samples
 * @param num_classes Number of classes
 * @return Average cross-entropy loss
 */
template<typename T>
T neural_cross_entropy_loss(const T* predictions, const T* targets,
                            size_t batch_size, size_t num_classes) {
    T total_loss = static_cast<T>(0);
    T epsilon = static_cast<T>(1e-10);  // For numerical stability
    
    for (size_t i = 0; i < batch_size; ++i) {
        for (size_t j = 0; j < num_classes; ++j) {
            size_t idx = i * num_classes + j;
            // Clip predictions to avoid log(0)
            T pred = predictions[idx];
            if (pred < epsilon) pred = epsilon;
            if (pred > static_cast<T>(1) - epsilon) pred = static_cast<T>(1) - epsilon;
            
            total_loss -= targets[idx] * std::log(pred);
        }
    }
    
    return total_loss / static_cast<T>(batch_size);
}

// Explicit template instantiations for common types
extern template void activate_tensor<float>(float*, const float*, size_t, ActivationType, float);
extern template void activate_tensor<double>(double*, const double*, size_t, ActivationType, double);

extern template void neural_matmul<float>(float*, const float*, const float*, size_t, size_t, size_t);
extern template void neural_matmul<double>(double*, const double*, const double*, size_t, size_t, size_t);

extern template void neural_dense_forward<float>(float*, const float*, const float*, const float*,
                                                 size_t, size_t, size_t, ActivationType);
extern template void neural_dense_forward<double>(double*, const double*, const double*, const double*,
                                                  size_t, size_t, size_t, ActivationType);

} // namespace tensor
} // namespace ggml

#endif // _GGML_TENSOR_ANN_RULES_H
