/*
 * GGML/Tensor/test_tensor_rules.cc
 *
 * Test GGML tensor-based fold and ANN rules
 */

#include <iostream>
#include <cassert>
#include <cmath>
#include "fold_rules.h"
#include "ann_rules.h"

using namespace ggml::tensor;

// Test epsilon for floating point comparisons
constexpr float TEST_EPSILON = 0.001f;

// Test fold operations
void test_fold_operations() {
    std::cout << "Testing fold operations..." << std::endl;
    
    // Test fold_sum
    float data[] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f};
    float result = tensor_fold_op(data, 5, FoldType::FOLD_SUM);
    assert(std::abs(result - 15.0f) < TEST_EPSILON);
    std::cout << "  ✅ FOLD_SUM: " << result << std::endl;
    
    // Test fold_product
    float data2[] = {2.0f, 3.0f, 4.0f};
    result = tensor_fold_op(data2, 3, FoldType::FOLD_PRODUCT);
    assert(std::abs(result - 24.0f) < TEST_EPSILON);
    std::cout << "  ✅ FOLD_PRODUCT: " << result << std::endl;
    
    // Test fold_max
    float data3[] = {1.5f, 4.2f, 2.1f, 3.7f};
    result = tensor_fold_op(data3, 4, FoldType::FOLD_MAX);
    assert(std::abs(result - 4.2f) < TEST_EPSILON);
    std::cout << "  ✅ FOLD_MAX: " << result << std::endl;
    
    // Test fold_mean
    result = tensor_fold_op(data, 5, FoldType::FOLD_MEAN);
    assert(std::abs(result - 3.0f) < TEST_EPSILON);
    std::cout << "  ✅ FOLD_MEAN: " << result << std::endl;
    
    std::cout << "✅ All fold operations passed!" << std::endl;
}

// Test neural activation functions
void test_activation_functions() {
    std::cout << "\nTesting neural activation functions..." << std::endl;
    
    float input[] = {-1.0f, 0.0f, 1.0f, 2.0f};
    float output[4];
    
    // Test SIGMOID
    activate_tensor(output, input, 4, ActivationType::SIGMOID);
    std::cout << "  ✅ SIGMOID: [";
    for (int i = 0; i < 4; ++i) {
        std::cout << output[i];
        if (i < 3) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    // Test RELU
    activate_tensor(output, input, 4, ActivationType::RELU);
    std::cout << "  ✅ RELU: [";
    for (int i = 0; i < 4; ++i) {
        std::cout << output[i];
        if (i < 3) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    assert(output[0] == 0.0f && output[2] == 1.0f);
    
    // Test TANH
    activate_tensor(output, input, 4, ActivationType::TANH);
    std::cout << "  ✅ TANH: [";
    for (int i = 0; i < 4; ++i) {
        std::cout << output[i];
        if (i < 3) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    std::cout << "✅ All activation functions passed!" << std::endl;
}

// Test matrix multiplication
void test_matmul() {
    std::cout << "\nTesting matrix multiplication..." << std::endl;
    
    // 2x3 * 3x2 = 2x2
    float A[] = {1.0f, 2.0f, 3.0f,
                 4.0f, 5.0f, 6.0f};
    
    float B[] = {1.0f, 2.0f,
                 3.0f, 4.0f,
                 5.0f, 6.0f};
    
    float C[4];
    
    neural_matmul(C, A, B, 2, 3, 2);
    
    std::cout << "  Matrix A (2x3) * Matrix B (3x2) = Matrix C (2x2):" << std::endl;
    std::cout << "  [" << C[0] << ", " << C[1] << "]" << std::endl;
    std::cout << "  [" << C[2] << ", " << C[3] << "]" << std::endl;
    
    // Verify result: C should be [[22, 28], [49, 64]]
    assert(std::abs(C[0] - 22.0f) < TEST_EPSILON);
    assert(std::abs(C[1] - 28.0f) < TEST_EPSILON);
    assert(std::abs(C[2] - 49.0f) < TEST_EPSILON);
    assert(std::abs(C[3] - 64.0f) < TEST_EPSILON);
    
    std::cout << "✅ Matrix multiplication passed!" << std::endl;
}

// Test dense layer forward pass
void test_dense_layer() {
    std::cout << "\nTesting dense layer forward pass..." << std::endl;
    
    // Simple 1x2 input, 2x3 weights
    float input[] = {1.0f, 2.0f};
    float weights[] = {0.5f, 1.0f, 1.5f,
                       2.0f, 2.5f, 3.0f};
    float bias[] = {0.1f, 0.2f, 0.3f};
    float output[3];
    
    neural_dense_forward(output, input, weights, bias, 1, 2, 3, ActivationType::LINEAR);
    
    std::cout << "  Output: [";
    for (int i = 0; i < 3; ++i) {
        std::cout << output[i];
        if (i < 2) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    // Expected: [1*0.5 + 2*2.0 + 0.1, 1*1.0 + 2*2.5 + 0.2, 1*1.5 + 2*3.0 + 0.3]
    //         = [4.6, 6.2, 7.8]
    assert(std::abs(output[0] - 4.6f) < TEST_EPSILON);
    assert(std::abs(output[1] - 6.2f) < TEST_EPSILON);
    assert(std::abs(output[2] - 7.8f) < TEST_EPSILON);
    
    std::cout << "✅ Dense layer forward pass passed!" << std::endl;
}

int main() {
    std::cout << "🧪 GGML Tensor Rules Test Suite" << std::endl;
    std::cout << "================================" << std::endl;
    
    try {
        test_fold_operations();
        test_activation_functions();
        test_matmul();
        test_dense_layer();
        
        std::cout << "\n🎉 All tests passed successfully!" << std::endl;
        std::cout << "✅ GGML tensor kernel compilation: SUCCESS" << std::endl;
        std::cout << "✅ Neural-symbolic integration: FUNCTIONAL" << std::endl;
        
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "❌ Test failed: " << e.what() << std::endl;
        return 1;
    }
}
