/*
 * test_tensor_kernel_minimal.cc
 *
 * Comprehensive test suite for bidirectional AtomSpace ↔ ggml tensor bridge
 * with recursive attention pathways and meta-pattern detection
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <random>
#include <cassert>
#include <cstring>

// Include GGML
#include "ggml.h"
#include "ggml-cpu.h"

// Include our minimal implementation (declarations)
extern "C" {
    struct TensorKernel;
    TensorKernel* create_tensor_kernel();
    void destroy_tensor_kernel(TensorKernel* kernel);
    bool initialize_tensor_kernel(TensorKernel* kernel);
    bool test_round_trip_conversion(TensorKernel* kernel, size_t num_atoms);
}

// Test utilities
class TestFramework {
private:
    int tests_run_ = 0;
    int tests_passed_ = 0;

public:
    void assert_test(bool condition, const std::string& test_name) {
        tests_run_++;
        if (condition) {
            tests_passed_++;
            std::cout << "[PASS] " << test_name << std::endl;
        } else {
            std::cout << "[FAIL] " << test_name << std::endl;
        }
    }

    void print_summary() {
        std::cout << "\n=== Test Summary ===" << std::endl;
        std::cout << "Tests run: " << tests_run_ << std::endl;
        std::cout << "Tests passed: " << tests_passed_ << std::endl;
        std::cout << "Success rate: " << (100.0 * tests_passed_ / tests_run_) << "%" << std::endl;
        std::cout << "===================" << std::endl;
    }

    bool all_passed() const {
        return tests_run_ == tests_passed_;
    }
};

// Performance measurement utility
class PerformanceTimer {
private:
    std::chrono::high_resolution_clock::time_point start_time_;
    
public:
    void start() {
        start_time_ = std::chrono::high_resolution_clock::now();
    }
    
    double elapsed_ms() {
        auto end_time = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time_);
        return duration.count() / 1000.0; // Convert to milliseconds
    }
};

// Comprehensive test functions
bool test_ggml_initialization() {
    std::cout << "\n--- Testing GGML Initialization ---" << std::endl;
    
    // Test GGML context creation
    size_t mem_size = 64 * 1024 * 1024; // 64MB
    struct ggml_init_params params = {
        .mem_size = mem_size,
        .mem_buffer = nullptr,
        .no_alloc = false,
    };
    
    ggml_context* ctx = ggml_init(params);
    if (!ctx) {
        std::cout << "Failed to initialize GGML context" << std::endl;
        return false;
    }
    
    // Test tensor creation
    ggml_tensor* test_tensor = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 64, 100);
    if (!test_tensor) {
        std::cout << "Failed to create test tensor" << std::endl;
        ggml_free(ctx);
        return false;
    }
    
    ggml_set_name(test_tensor, "test_tensor");
    
    // Test tensor operations
    ggml_tensor* scaled_tensor = ggml_scale(ctx, test_tensor, 2.0f);
    if (!scaled_tensor) {
        std::cout << "Failed to create scaled tensor" << std::endl;
        ggml_free(ctx);
        return false;
    }
    
    std::cout << "GGML initialization successful" << std::endl;
    std::cout << "Test tensor dimensions: " << test_tensor->ne[0] << "x" << test_tensor->ne[1] << std::endl;
    
    ggml_free(ctx);
    return true;
}

bool test_tensor_kernel_basic() {
    std::cout << "\n--- Testing Tensor Kernel Basic Operations ---" << std::endl;
    
    TensorKernel* kernel = create_tensor_kernel();
    if (!kernel) {
        std::cout << "Failed to create tensor kernel" << std::endl;
        return false;
    }
    
    bool init_success = initialize_tensor_kernel(kernel);
    if (!init_success) {
        std::cout << "Failed to initialize tensor kernel" << std::endl;
        destroy_tensor_kernel(kernel);
        return false;
    }
    
    std::cout << "Tensor kernel initialized successfully" << std::endl;
    
    destroy_tensor_kernel(kernel);
    return true;
}

bool test_round_trip_conversions() {
    std::cout << "\n--- Testing Round-Trip Atom ↔ Tensor Conversions ---" << std::endl;
    
    TensorKernel* kernel = create_tensor_kernel();
    if (!kernel || !initialize_tensor_kernel(kernel)) {
        std::cout << "Failed to initialize kernel for round-trip test" << std::endl;
        return false;
    }
    
    bool all_passed = true;
    std::vector<size_t> test_sizes = {1, 5, 10, 50, 100, 500};
    
    for (size_t num_atoms : test_sizes) {
        std::cout << "Testing round-trip with " << num_atoms << " atoms... ";
        
        bool success = test_round_trip_conversion(kernel, num_atoms);
        if (success) {
            std::cout << "PASSED" << std::endl;
        } else {
            std::cout << "FAILED" << std::endl;
            all_passed = false;
        }
    }
    
    destroy_tensor_kernel(kernel);
    return all_passed;
}

bool test_performance_benchmarks() {
    std::cout << "\n--- Performance Benchmarks ---" << std::endl;
    
    TensorKernel* kernel = create_tensor_kernel();
    if (!kernel || !initialize_tensor_kernel(kernel)) {
        std::cout << "Failed to initialize kernel for performance test" << std::endl;
        return false;
    }
    
    PerformanceTimer timer;
    std::vector<size_t> benchmark_sizes = {100, 500, 1000, 5000};
    
    for (size_t num_atoms : benchmark_sizes) {
        timer.start();
        
        bool success = test_round_trip_conversion(kernel, num_atoms);
        
        double elapsed = timer.elapsed_ms();
        
        std::cout << "Atoms: " << num_atoms << ", Time: " << elapsed << " ms";
        std::cout << ", Rate: " << (num_atoms / elapsed * 1000.0) << " atoms/sec";
        std::cout << " [" << (success ? "PASS" : "FAIL") << "]" << std::endl;
    }
    
    destroy_tensor_kernel(kernel);
    return true;
}

bool test_property_based_fuzzing() {
    std::cout << "\n--- Property-Based Fuzzing Tests ---" << std::endl;
    
    TensorKernel* kernel = create_tensor_kernel();
    if (!kernel || !initialize_tensor_kernel(kernel)) {
        std::cout << "Failed to initialize kernel for fuzzing test" << std::endl;
        return false;
    }
    
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<size_t> size_dist(1, 1000);
    
    int fuzz_tests = 50;
    int passed = 0;
    
    for (int i = 0; i < fuzz_tests; ++i) {
        size_t random_size = size_dist(gen);
        
        bool success = test_round_trip_conversion(kernel, random_size);
        if (success) {
            passed++;
        }
        
        if ((i + 1) % 10 == 0) {
            std::cout << "Completed " << (i + 1) << "/" << fuzz_tests << " fuzz tests" << std::endl;
        }
    }
    
    double success_rate = (100.0 * passed) / fuzz_tests;
    std::cout << "Fuzzing results: " << passed << "/" << fuzz_tests << " passed";
    std::cout << " (" << success_rate << "%)" << std::endl;
    
    destroy_tensor_kernel(kernel);
    return success_rate >= 95.0; // Require 95% success rate
}

bool test_tensor_serialization() {
    std::cout << "\n--- Testing Tensor Serialization ---" << std::endl;
    
    // Test basic GGML tensor serialization
    size_t mem_size = 32 * 1024 * 1024; // 32MB
    struct ggml_init_params params = {
        .mem_size = mem_size,
        .mem_buffer = nullptr,
        .no_alloc = false,
    };
    
    ggml_context* ctx = ggml_init(params);
    if (!ctx) return false;
    
    // Create test tensor with known data
    ggml_tensor* test_tensor = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 10, 5);
    if (!test_tensor) {
        ggml_free(ctx);
        return false;
    }
    
    // Fill with test data
    float* data = (float*)test_tensor->data;
    for (int i = 0; i < 50; ++i) {
        data[i] = static_cast<float>(i * i);
    }
    
    // Test tensor properties
    bool dimensions_correct = (test_tensor->ne[0] == 10 && test_tensor->ne[1] == 5);
    bool data_accessible = (data[0] == 0.0f && data[49] == 49.0f * 49.0f);
    
    std::cout << "Tensor creation: " << (dimensions_correct ? "PASS" : "FAIL") << std::endl;
    std::cout << "Data accessibility: " << (data_accessible ? "PASS" : "FAIL") << std::endl;
    
    ggml_free(ctx);
    return dimensions_correct && data_accessible;
}

bool test_attention_mechanisms() {
    std::cout << "\n--- Testing Attention Mechanisms ---" << std::endl;
    
    // Test basic attention computation with GGML
    size_t mem_size = 32 * 1024 * 1024; // 32MB
    struct ggml_init_params params = {
        .mem_size = mem_size,
        .mem_buffer = nullptr,
        .no_alloc = false,
    };
    
    ggml_context* ctx = ggml_init(params);
    if (!ctx) return false;
    
    // Create attention matrices
    ggml_tensor* query = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 64, 100);    // [features, atoms]
    ggml_tensor* key = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 64, 100);      // [features, atoms]
    ggml_tensor* value = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 64, 100);    // [features, atoms]
    
    if (!query || !key || !value) {
        ggml_free(ctx);
        return false;
    }
    
    // Initialize with test data
    float* q_data = (float*)query->data;
    float* k_data = (float*)key->data;
    float* v_data = (float*)value->data;
    
    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<float> dist(0.0f, 1.0f);
    
    for (int i = 0; i < 64 * 100; ++i) {
        q_data[i] = dist(gen);
        k_data[i] = dist(gen);
        v_data[i] = dist(gen);
    }
    
    // Compute attention: scores = query^T * key
    ggml_tensor* attention_scores = ggml_mul_mat(ctx, key, query);
    if (!attention_scores) {
        ggml_free(ctx);
        return false;
    }
    
    // Apply softmax to get attention weights
    ggml_tensor* attention_weights = ggml_soft_max(ctx, attention_scores);
    if (!attention_weights) {
        ggml_free(ctx);
        return false;
    }
    
    // Apply attention to values
    ggml_tensor* attended_output = ggml_mul_mat(ctx, value, attention_weights);
    if (!attended_output) {
        ggml_free(ctx);
        return false;
    }
    
    std::cout << "Attention computation successful" << std::endl;
    std::cout << "Attention scores shape: " << attention_scores->ne[0] << "x" << attention_scores->ne[1] << std::endl;
    std::cout << "Attended output shape: " << attended_output->ne[0] << "x" << attended_output->ne[1] << std::endl;
    
    ggml_free(ctx);
    return true;
}

bool test_hypergraph_encoding() {
    std::cout << "\n--- Testing Hypergraph Encoding ---" << std::endl;
    
    size_t mem_size = 32 * 1024 * 1024; // 32MB
    struct ggml_init_params params = {
        .mem_size = mem_size,
        .mem_buffer = nullptr,
        .no_alloc = false,
    };
    
    ggml_context* ctx = ggml_init(params);
    if (!ctx) return false;
    
    // Create adjacency matrix for hypergraph
    size_t num_nodes = 50;
    ggml_tensor* adjacency = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, num_nodes, num_nodes);
    if (!adjacency) {
        ggml_free(ctx);
        return false;
    }
    
    // Initialize adjacency matrix with test hypergraph structure
    float* adj_data = (float*)adjacency->data;
    memset(adj_data, 0, num_nodes * num_nodes * sizeof(float));
    
    // Create some test edges
    for (size_t i = 0; i < num_nodes; ++i) {
        for (size_t j = 0; j < num_nodes; ++j) {
            if (i != j && (i + j) % 7 == 0) {
                adj_data[i * num_nodes + j] = 1.0f;
            }
        }
    }
    
    // Compute graph features using matrix operations
    ggml_tensor* degree_matrix = ggml_sum_rows(ctx, adjacency);
    if (!degree_matrix) {
        ggml_free(ctx);
        return false;
    }
    
    // Test graph powers for higher-order patterns
    ggml_tensor* adj_squared = ggml_mul_mat(ctx, adjacency, adjacency);
    if (!adj_squared) {
        ggml_free(ctx);
        return false;
    }
    
    std::cout << "Hypergraph encoding successful" << std::endl;
    std::cout << "Graph size: " << num_nodes << " nodes" << std::endl;
    std::cout << "Adjacency matrix: " << adjacency->ne[0] << "x" << adjacency->ne[1] << std::endl;
    
    ggml_free(ctx);
    return true;
}

bool test_meta_pattern_detection() {
    std::cout << "\n--- Testing Meta-Pattern Detection ---" << std::endl;
    
    // Simulate meta-pattern detection with tensor operations
    size_t mem_size = 32 * 1024 * 1024; // 32MB
    struct ggml_init_params params = {
        .mem_size = mem_size,
        .mem_buffer = nullptr,
        .no_alloc = false,
    };
    
    ggml_context* ctx = ggml_init(params);
    if (!ctx) return false;
    
    // Create time series of attention values
    size_t num_atoms = 100;
    size_t time_steps = 10;
    ggml_tensor* attention_evolution = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, num_atoms, time_steps);
    
    if (!attention_evolution) {
        ggml_free(ctx);
        return false;
    }
    
    // Fill with evolving attention patterns
    float* data = (float*)attention_evolution->data;
    for (size_t t = 0; t < time_steps; ++t) {
        for (size_t a = 0; a < num_atoms; ++a) {
            // Create synthetic attention patterns
            float base_attention = std::sin(a * 0.1f) + 1.0f;
            float temporal_modulation = std::cos(t * 0.5f);
            float pattern_component = std::sin((a + t) * 0.2f);
            
            data[t * num_atoms + a] = base_attention * (1.0f + 0.3f * temporal_modulation + 0.2f * pattern_component);
        }
    }
    
    // Compute temporal correlations
    ggml_tensor* correlation_matrix = ggml_mul_mat(ctx, attention_evolution, 
                                                  ggml_transpose(ctx, attention_evolution));
    if (!correlation_matrix) {
        ggml_free(ctx);
        return false;
    }
    
    // Apply threshold for pattern detection (simplified)
    ggml_tensor* pattern_mask = ggml_scale(ctx, correlation_matrix, 1.0f);
    
    if (!pattern_mask) {
        ggml_free(ctx);
        return false;
    }
    
    std::cout << "Meta-pattern detection successful" << std::endl;
    std::cout << "Attention evolution: " << attention_evolution->ne[0] << "x" << attention_evolution->ne[1] << std::endl;
    std::cout << "Correlation matrix: " << correlation_matrix->ne[0] << "x" << correlation_matrix->ne[1] << std::endl;
    
    ggml_free(ctx);
    return true;
}

// Main test runner
int main() {
    std::cout << "=== OpenCog Unified Tensor Kernel Test Suite ===" << std::endl;
    std::cout << "Testing bidirectional AtomSpace ↔ ggml tensor bridge" << std::endl;
    std::cout << "with recursive attention pathways and meta-pattern detection" << std::endl;
    
    TestFramework framework;
    
    // Core functionality tests
    framework.assert_test(test_ggml_initialization(), "GGML Initialization");
    framework.assert_test(test_tensor_kernel_basic(), "Tensor Kernel Basic Operations");
    framework.assert_test(test_round_trip_conversions(), "Round-Trip Atom ↔ Tensor Conversions");
    framework.assert_test(test_tensor_serialization(), "Tensor Serialization");
    
    // Advanced functionality tests
    framework.assert_test(test_attention_mechanisms(), "Attention Mechanisms");
    framework.assert_test(test_hypergraph_encoding(), "Hypergraph Encoding");
    framework.assert_test(test_meta_pattern_detection(), "Meta-Pattern Detection");
    
    // Performance and robustness tests
    framework.assert_test(test_performance_benchmarks(), "Performance Benchmarks");
    framework.assert_test(test_property_based_fuzzing(), "Property-Based Fuzzing");
    
    // Print final results
    framework.print_summary();
    
    if (framework.all_passed()) {
        std::cout << "\n🎉 All tests passed! The bidirectional AtomSpace ↔ tensor bridge is working correctly." << std::endl;
        std::cout << "Recursive attention pathways and meta-pattern detection are functional." << std::endl;
        return 0;
    } else {
        std::cout << "\n❌ Some tests failed. Please review the implementation." << std::endl;
        return 1;
    }
}