/*
 * test_phase3_neural_symbolic_kernels.cc
 *
 * Comprehensive test suite for Phase 3: Neural-Symbolic Synthesis via Custom ggml Kernels
 * Validates all custom kernels, tensor signatures, and inference pipeline
 */

#include <iostream>
#include <iomanip>
#include <vector>
#include <cassert>
#include <cmath>

#include "include/ggml.h"
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/util/Logger.h>
#include "include/TensorBenchmark.h"

using namespace opencog;

// Forward declaration of pipeline validation function
extern "C" bool validate_neural_symbolic_inference_pipeline(AtomSpace* atomspace, ggml_context* context);

/**
 * Test framework for Phase 3 validation
 */
class Phase3TestFramework {
private:
    ggml_context* context_;
    AtomSpace* atomspace_;
    int tests_run_;
    int tests_passed_;
    
public:
    Phase3TestFramework() : context_(nullptr), atomspace_(nullptr), tests_run_(0), tests_passed_(0) {
        // Initialize ggml context
        struct ggml_init_params params = {
            .mem_size = 32 * 1024 * 1024,  // 32MB
            .mem_buffer = nullptr,
            .no_alloc = false,
        };
        context_ = ggml_init(params);
        
        // Initialize AtomSpace
        atomspace_ = new AtomSpace();
    }
    
    ~Phase3TestFramework() {
        if (context_) {
            ggml_free(context_);
        }
        if (atomspace_) {
            delete atomspace_;
        }
    }
    
    void run_test(const std::string& test_name, std::function<bool()> test_func) {
        tests_run_++;
        std::cout << "Running: " << test_name << "..." << std::flush;
        
        bool result = test_func();
        if (result) {
            tests_passed_++;
            std::cout << " PASSED" << std::endl;
        } else {
            std::cout << " FAILED" << std::endl;
        }
    }
    
    void print_summary() {
        std::cout << "\n=== Phase 3 Test Summary ===" << std::endl;
        std::cout << "Tests run: " << tests_run_ << std::endl;
        std::cout << "Tests passed: " << tests_passed_ << std::endl;
        std::cout << "Tests failed: " << (tests_run_ - tests_passed_) << std::endl;
        std::cout << "Success rate: " << std::fixed << std::setprecision(1) 
                  << (100.0 * tests_passed_ / tests_run_) << "%" << std::endl;
        
        if (tests_passed_ == tests_run_) {
            std::cout << "\n🎉 ALL PHASE 3 TESTS PASSED! Neural-Symbolic Synthesis Complete!" << std::endl;
        } else {
            std::cout << "\n⚠️  Some tests failed. Review implementation." << std::endl;
        }
    }
    
    bool all_tests_passed() const {
        return tests_passed_ == tests_run_;
    }
    
    ggml_context* get_context() { return context_; }
    AtomSpace* get_atomspace() { return atomspace_; }
};

// Test custom ggml kernel operations
bool test_custom_ggml_scale_kernel() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Create test tensor
    ggml_tensor* input = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 4, 3);
    if (!input) return false;
    
    // Initialize input data
    float* input_data = ggml_get_data_f32(input);
    for (int i = 0; i < 12; i++) {
        input_data[i] = (float)i + 1.0f;  // 1, 2, 3, ..., 12
    }
    
    // Test ggml_scale operation
    ggml_tensor* scaled = ggml_scale(ctx, input, 2.0f);
    if (!scaled) return false;
    
    float* scaled_data = ggml_get_data_f32(scaled);
    
    // Verify scaling
    for (int i = 0; i < 12; i++) {
        float expected = ((float)i + 1.0f) * 2.0f;
        if (fabs(scaled_data[i] - expected) > 1e-6) {
            return false;
        }
    }
    
    return true;
}

bool test_custom_ggml_sum_rows_kernel() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Create 3x4 test matrix
    ggml_tensor* matrix = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 4, 3);
    if (!matrix) return false;
    
    float* matrix_data = ggml_get_data_f32(matrix);
    // Row 0: [1, 2, 3, 4] -> sum = 10
    // Row 1: [5, 6, 7, 8] -> sum = 26  
    // Row 2: [9, 10, 11, 12] -> sum = 42
    for (int i = 0; i < 12; i++) {
        matrix_data[i] = (float)i + 1.0f;
    }
    
    ggml_tensor* row_sums = ggml_sum_rows(ctx, matrix);
    if (!row_sums) return false;
    
    float* sums_data = ggml_get_data_f32(row_sums);
    
    // Verify row sums
    float expected_sums[] = {10.0f, 26.0f, 42.0f};
    for (int i = 0; i < 3; i++) {
        if (fabs(sums_data[i] - expected_sums[i]) > 1e-6) {
            return false;
        }
    }
    
    return true;
}

bool test_hypergraph_convolution_kernel() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Create test hypergraph data
    ggml_tensor* nodes = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 5, 4);  // 5 nodes, 4 features each
    ggml_tensor* hyperedges = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 5, 3);  // 5 hyperedges, 3 features each
    
    if (!nodes || !hyperedges) return false;
    
    // Initialize with test data
    float* node_data = ggml_get_data_f32(nodes);
    float* edge_data = ggml_get_data_f32(hyperedges);
    
    for (size_t i = 0; i < ggml_nelements(nodes); i++) {
        node_data[i] = 1.0f;
    }
    for (size_t i = 0; i < ggml_nelements(hyperedges); i++) {
        edge_data[i] = 0.5f;
    }
    
    // Apply hypergraph convolution
    ggml_tensor* result = ggml_hypergraph_conv(ctx, nodes, hyperedges);
    if (!result) return false;
    
    // Verify result has same shape as nodes
    if (result->ne[0] != nodes->ne[0] || result->ne[1] != nodes->ne[1]) {
        return false;
    }
    
    float* result_data = ggml_get_data_f32(result);
    
    // Basic sanity check: result should be different from input
    bool has_changes = false;
    for (size_t i = 0; i < ggml_nelements(result); i++) {
        if (fabs(result_data[i] - node_data[i]) > 1e-6) {
            has_changes = true;
            break;
        }
    }
    
    return has_changes;
}

bool test_symbolic_attention_kernel() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Create attention tensors
    size_t seq_len = 4;
    size_t d_model = 8;
    
    ggml_tensor* query = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, seq_len, d_model);
    ggml_tensor* key = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, seq_len, d_model);
    ggml_tensor* value = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, seq_len, d_model);
    
    if (!query || !key || !value) return false;
    
    // Initialize with test patterns
    float* q_data = ggml_get_data_f32(query);
    float* k_data = ggml_get_data_f32(key);
    float* v_data = ggml_get_data_f32(value);
    
    for (size_t i = 0; i < seq_len * d_model; i++) {
        q_data[i] = sinf((float)i * 0.1f);
        k_data[i] = cosf((float)i * 0.1f);
        v_data[i] = (float)i / (seq_len * d_model);
    }
    
    // Apply symbolic attention
    ggml_tensor* attended = ggml_symbolic_attention(ctx, query, key, value);
    if (!attended) return false;
    
    // Verify output dimensions
    if (attended->ne[0] != seq_len || attended->ne[1] != d_model) {
        return false;
    }
    
    float* attended_data = ggml_get_data_f32(attended);
    
    // Check that attention produces valid output (not NaN, finite values)
    for (size_t i = 0; i < ggml_nelements(attended); i++) {
        if (!std::isfinite(attended_data[i])) {
            return false;
        }
    }
    
    return true;
}

bool test_cognitive_fusion_kernel() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Create neural and symbolic tensors
    size_t dim = 10;
    ggml_tensor* neural = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, dim);
    ggml_tensor* symbolic = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, dim);
    
    if (!neural || !symbolic) return false;
    
    // Initialize with distinct patterns
    float* neural_data = ggml_get_data_f32(neural);
    float* symbolic_data = ggml_get_data_f32(symbolic);
    
    for (size_t i = 0; i < dim; i++) {
        neural_data[i] = 1.0f;     // All ones
        symbolic_data[i] = 2.0f;   // All twos
    }
    
    // Test different fusion weights
    float fusion_weights[] = {0.0f, 0.25f, 0.5f, 0.75f, 1.0f};
    
    for (float weight : fusion_weights) {
        ggml_tensor* fused = ggml_cognitive_fusion(ctx, neural, symbolic, weight);
        if (!fused) return false;
        
        float* fused_data = ggml_get_data_f32(fused);
        
        // Verify fusion formula: (1-weight)*neural + weight*symbolic
        float expected = (1.0f - weight) * 1.0f + weight * 2.0f;
        
        for (size_t i = 0; i < dim; i++) {
            if (fabs(fused_data[i] - expected) > 1e-6) {
                return false;
            }
        }
    }
    
    return true;
}

bool test_neural_symbolic_utility_functions() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Test truth value encoding
    ggml_tensor* tv = ggml_truth_value_encode(ctx, 0.8f, 0.9f);
    if (!tv) return false;
    
    float* tv_data = ggml_get_data_f32(tv);
    if (fabs(tv_data[0] - 0.8f) > 1e-6 || fabs(tv_data[1] - 0.9f) > 1e-6) {
        return false;
    }
    
    // Test pattern matching score
    ggml_tensor* pattern = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, 5);
    ggml_tensor* target = ggml_new_tensor_1d(ctx, GGML_TYPE_F32, 5);
    
    if (!pattern || !target) return false;
    
    float* pattern_data = ggml_get_data_f32(pattern);
    float* target_data = ggml_get_data_f32(target);
    
    // Create identical patterns for perfect match
    for (int i = 0; i < 5; i++) {
        pattern_data[i] = target_data[i] = 1.0f;
    }
    
    ggml_tensor* score = ggml_pattern_match_score(ctx, pattern, target);
    if (!score) return false;
    
    float* score_data = ggml_get_data_f32(score);
    
    // Should be close to 1.0 for identical patterns
    if (fabs(score_data[0] - 1.0f) > 1e-5) {
        return false;
    }
    
    // Test graph embeddings
    ggml_tensor* adjacency = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, 6, 6);
    if (!adjacency) return false;
    
    ggml_tensor* embeddings = ggml_compute_graph_embeddings(ctx, adjacency);
    if (!embeddings) return false;
    
    // Verify embedding dimensions (should be [n_nodes, 64])
    if (embeddings->ne[0] != 6 || embeddings->ne[1] != 64) {
        return false;
    }
    
    return true;
}

bool test_tensor_signature_validation() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    AtomSpace* as = framework.get_atomspace();
    
    if (!ctx || !as) return false;
    
    TensorBenchmark benchmark(ctx, as);
    
    // Test Neural_Symbolic_Tensor[7] signature compliance
    NeuralSymbolicTensorSignature signature;
    signature.symbolic_representation = NeuralSymbolicTensorSignature::RepresentationType::HYBRID;
    signature.neural_embedding_dim = 128;
    signature.confidence_score = 0.75f;
    signature.gradient_flow = NeuralSymbolicTensorSignature::GradientFlow::BIDIRECTIONAL;
    signature.fusion_weight = 0.6f;
    signature.computation_cost = 1.5f;
    signature.inference_depth = 3;
    
    bool signature_valid = benchmark.validate_tensor_signature_compliance(signature);
    
    return signature_valid;
}

bool test_comprehensive_benchmarking() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    AtomSpace* as = framework.get_atomspace();
    
    if (!ctx || !as) return false;
    
    // Add some test data to AtomSpace
    Handle concept1 = as->add_node(CONCEPT_NODE, "neural");
    Handle concept2 = as->add_node(CONCEPT_NODE, "symbolic");
    Handle link1 = as->add_link(INHERITANCE_LINK, {concept1, concept2});
    
    TensorBenchmark benchmark(ctx, as);
    benchmark.set_max_tensor_dimension(256);
    benchmark.set_max_inference_depth(5);
    
    // Run comprehensive benchmark suite
    std::vector<BenchmarkResult> results = benchmark.run_comprehensive_benchmark();
    
    if (results.empty()) return false;
    
    // Verify all benchmark operations completed
    bool all_operations_successful = true;
    for (const auto& result : results) {
        if (result.execution_time_ms <= 0.0 || result.accuracy_score < 0.0) {
            all_operations_successful = false;
            break;
        }
    }
    
    return all_operations_successful;
}

bool test_end_to_end_inference_pipeline() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    AtomSpace* as = framework.get_atomspace();
    
    if (!ctx || !as) return false;
    
    // Use external pipeline validation function
    return validate_neural_symbolic_inference_pipeline(as, ctx);
}

bool test_performance_under_stress() {
    Phase3TestFramework framework;
    ggml_context* ctx = framework.get_context();
    
    if (!ctx) return false;
    
    // Stress test with large tensors
    std::vector<size_t> stress_dimensions = {100, 200, 300, 400, 500};
    
    for (size_t dim : stress_dimensions) {
        ggml_tensor* large_tensor = ggml_new_tensor_2d(ctx, GGML_TYPE_F32, dim, dim);
        if (!large_tensor) return false;
        
        // Apply multiple operations
        ggml_tensor* scaled = ggml_scale(ctx, large_tensor, 1.5f);
        if (!scaled) return false;
        
        ggml_tensor* summed = ggml_sum_rows(ctx, large_tensor);
        if (!summed) return false;
        
        // Verify operations complete without errors
        if (ggml_nelements(scaled) != ggml_nelements(large_tensor)) {
            return false;
        }
        
        if (ggml_nelements(summed) != dim) {
            return false;
        }
    }
    
    return true;
}

int main() {
    std::cout << "=== Phase 3: Neural-Symbolic Synthesis via Custom ggml Kernels ===" << std::endl;
    std::cout << "Comprehensive validation of custom kernels and tensor operations" << std::endl;
    std::cout << std::endl;
    
    Phase3TestFramework framework;
    
    // Kernel Customization Tests
    std::cout << "--- Kernel Customization ---" << std::endl;
    framework.run_test("Custom ggml_scale kernel", test_custom_ggml_scale_kernel);
    framework.run_test("Custom ggml_sum_rows kernel", test_custom_ggml_sum_rows_kernel);
    framework.run_test("Hypergraph convolution kernel", test_hypergraph_convolution_kernel);
    framework.run_test("Symbolic attention kernel", test_symbolic_attention_kernel);
    framework.run_test("Cognitive fusion kernel", test_cognitive_fusion_kernel);
    framework.run_test("Neural-symbolic utilities", test_neural_symbolic_utility_functions);
    
    // Tensor Signature Benchmarking Tests
    std::cout << "\n--- Tensor Signature Benchmarking ---" << std::endl;
    framework.run_test("Tensor signature validation", test_tensor_signature_validation);
    framework.run_test("Comprehensive benchmarking", test_comprehensive_benchmarking);
    framework.run_test("Performance stress testing", test_performance_under_stress);
    
    // Verification Tests
    std::cout << "\n--- End-to-End Verification ---" << std::endl;
    framework.run_test("Neural-symbolic inference pipeline", test_end_to_end_inference_pipeline);
    
    framework.print_summary();
    
    return framework.all_tests_passed() ? 0 : 1;
}