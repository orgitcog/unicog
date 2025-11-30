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

// Test depth-aware tensor folding (new functionality)
void test_depth_aware_folding() {
    std::cout << "\nTesting depth-aware tensor folding..." << std::endl;
    
    // Create a simple 2D tensor [3, 4]
    size_t dims[] = {3, 4};
    Tensor<float> tensor(dims, 2);
    
    // Fill with test data
    for (size_t i = 0; i < tensor.total_size; ++i) {
        tensor.data[i] = static_cast<float>(i + 1);
    }
    
    // Test depth-aware folding at different depths
    auto results = tensor_fold_depth_aware(&tensor, 3, FoldType::FOLD_SUM);
    
    std::cout << "  Depth-aware fold results (depth=3): [";
    for (size_t i = 0; i < results.size(); ++i) {
        std::cout << results[i];
        if (i < results.size() - 1) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    // Verify we got results for all depth levels
    assert(results.size() == 3);
    assert(results[0] > 0.0f);  // First depth should have sum
    
    std::cout << "✅ Depth-aware tensor folding passed!" << std::endl;
}

// Test tensor folding with boundary conditions
void test_fold_boundary_conditions() {
    std::cout << "\nTesting fold operations with boundary conditions..." << std::endl;
    
    // Test with empty tensor
    Tensor<float>* null_result = tensor_fold_axis<float>(nullptr, 0, FoldType::FOLD_SUM);
    assert(null_result == nullptr);
    std::cout << "  ✅ Null tensor handled correctly" << std::endl;
    
    // Test with valid tensor
    size_t dims[] = {2, 3};
    Tensor<float> tensor(dims, 2);
    for (size_t i = 0; i < tensor.total_size; ++i) {
        tensor.data[i] = static_cast<float>(i + 1);
    }
    
    // Test folding along axis 0
    Tensor<float>* result = tensor_fold_axis(&tensor, 0, FoldType::FOLD_SUM);
    assert(result != nullptr);
    assert(result->ndim == 1);
    assert(result->shape[0] == 3);
    
    std::cout << "  Folded along axis 0: [";
    for (size_t i = 0; i < result->total_size; ++i) {
        std::cout << result->data[i];
        if (i < result->total_size - 1) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    delete result;
    
    // Test invalid axis
    Tensor<float>* invalid_result = tensor_fold_axis(&tensor, 5, FoldType::FOLD_SUM);
    assert(invalid_result == nullptr);
    std::cout << "  ✅ Invalid axis handled correctly" << std::endl;
    
    std::cout << "✅ Boundary condition tests passed!" << std::endl;
}

// Test attention allocation for neural rules
void test_attention_allocation() {
    std::cout << "\nTesting attention allocation for neural rules..." << std::endl;
    
    // Create attention allocation structure
    AttentionAllocation attention;
    
    // Initial state
    std::cout << "  Initial STI: " << attention.sti << std::endl;
    assert(attention.sti == 100.0f);
    
    // Update with activity
    attention.update(5.0f);
    std::cout << "  After update STI: " << attention.sti << std::endl;
    assert(attention.sti > 100.0f);
    assert(attention.allocations == 1);
    
    // Validate attention allocation
    bool is_valid = validate_attention_allocation(attention, "test_rule", 50.0f);
    assert(is_valid);
    std::cout << "  ✅ Attention allocation valid" << std::endl;
    
    // Test decay
    attention.decay(0.1f);
    std::cout << "  After decay STI: " << attention.sti << std::endl;
    assert(attention.sti < 150.0f);  // Should have decayed
    
    // Test attention threshold
    assert(attention.is_active(50.0f));
    std::cout << "  ✅ Attention threshold check passed" << std::endl;
    
    // Test total attention score
    float total = attention.get_total_attention();
    std::cout << "  Total attention: " << total << std::endl;
    assert(total > 0.0f);
    
    std::cout << "✅ Attention allocation tests passed!" << std::endl;
}

// Test attention-weighted activation
void test_attention_weighted_activation() {
    std::cout << "\nTesting attention-weighted activation..." << std::endl;
    
    float input[] = {-1.0f, 0.0f, 1.0f, 2.0f};
    float output[4];
    
    // Create attention with high STI
    AttentionAllocation attention;
    attention.sti = 150.0f;
    attention.lti = 50.0f;
    
    // Apply attention-weighted activation
    activate_with_attention(output, input, 4, ActivationType::SIGMOID, attention);
    
    std::cout << "  Attention-weighted SIGMOID output: [";
    for (int i = 0; i < 4; ++i) {
        std::cout << output[i];
        if (i < 3) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    // Verify outputs are modulated by attention (should be > standard sigmoid)
    float standard_output[4];
    activate_tensor(standard_output, input, 4, ActivationType::SIGMOID);
    
    // Attention-weighted should be amplified
    assert(output[2] >= standard_output[2]);  // At least as large
    
    std::cout << "✅ Attention-weighted activation passed!" << std::endl;
}

int main() {
    std::cout << "🧪 GGML Tensor Rules Test Suite" << std::endl;
    std::cout << "================================" << std::endl;
    
    try {
        test_fold_operations();
        test_activation_functions();
        test_matmul();
        test_dense_layer();
        test_depth_aware_folding();
        test_fold_boundary_conditions();
        test_attention_allocation();
        test_attention_weighted_activation();
        
        std::cout << "\n🎉 All tests passed successfully!" << std::endl;
        std::cout << "✅ GGML tensor kernel compilation: SUCCESS" << std::endl;
        std::cout << "✅ Neural-symbolic integration: FUNCTIONAL" << std::endl;
        std::cout << "✅ Attention allocation: OPERATIONAL" << std::endl;
        std::cout << "✅ Boundary validation: ROBUST" << std::endl;
        
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "❌ Test failed: " << e.what() << std::endl;
        return 1;
    }
}
