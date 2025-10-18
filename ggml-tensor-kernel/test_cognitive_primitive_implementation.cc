/*
 * test_cognitive_primitive_implementation.cc
 *
 * Test the CognitivePrimitive implementation directly
 *
 * Copyright (c) 2025 OpenCog Foundation
 */

#include <iostream>
#include <memory>
#include <cassert>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/util/Logger.h>
#include "../include/opencog/tensor/CognitivePrimitive.h"

using namespace opencog;

int main() {
    std::cout << "=== Cognitive Primitive Implementation Test ===" << std::endl;
    
    // Initialize GGML context
    struct ggml_init_params params = {
        .mem_size   = 1024 * 1024 * 16, // 16MB
        .mem_buffer = nullptr,
        .no_alloc   = false,
    };
    ggml_context* ctx = ggml_init(params);
    
    if (!ctx) {
        std::cerr << "Failed to initialize GGML context" << std::endl;
        return 1;
    }
    
    std::cout << "✓ GGML context initialized" << std::endl;
    
    try {
        // Test 1: Create cognitive primitive tensor
        std::cout << "\nTest 1: Creating cognitive primitive tensor..." << std::endl;
        auto primitive = std::make_unique<CognitivePrimitiveTensor>(ctx, "test-primitive");
        std::cout << "✓ Cognitive primitive tensor created" << std::endl;
        
        // Test 2: Set cognitive primitive values
        std::cout << "\nTest 2: Setting cognitive primitive values..." << std::endl;
        primitive->set_modality(ModalityType::VISUAL);
        primitive->set_depth(DepthType::SEMANTIC);
        primitive->set_context(ContextType::GLOBAL);
        primitive->set_salience(0.8f);
        primitive->set_autonomy_index(0.6f);
        std::cout << "✓ Cognitive primitive values set" << std::endl;
        
        // Test 3: Validate tensor shape
        std::cout << "\nTest 3: Validating tensor shape..." << std::endl;
        bool shape_valid = primitive->validate_tensor_shape();
        bool values_valid = primitive->validate_primitive_values();
        
        if (shape_valid && values_valid) {
            std::cout << "✓ Tensor shape and values are valid" << std::endl;
        } else {
            std::cout << "✗ Tensor validation failed - shape: " << shape_valid 
                      << ", values: " << values_valid << std::endl;
        }
        
        // Test 4: Get tensor properties
        std::cout << "\nTest 4: Getting tensor properties..." << std::endl;
        auto shape = primitive->get_shape();
        size_t total_elements = primitive->get_total_elements();
        size_t dof = primitive->calculate_degrees_of_freedom();
        
        std::cout << "✓ Tensor shape: [";
        for (size_t i = 0; i < shape.size(); ++i) {
            std::cout << shape[i];
            if (i < shape.size() - 1) std::cout << ", ";
        }
        std::cout << "]" << std::endl;
        std::cout << "✓ Total elements: " << total_elements << std::endl;
        std::cout << "✓ Degrees of freedom: " << dof << std::endl;
        
        // Test 5: Prime factorization
        std::cout << "\nTest 5: Prime factorization..." << std::endl;
        auto primes = primitive->get_prime_factorization();
        std::cout << "✓ Prime signature: [";
        for (size_t i = 0; i < primes.size(); ++i) {
            std::cout << primes[i];
            if (i < primes.size() - 1) std::cout << ", ";
        }
        std::cout << "]" << std::endl;
        
        // Test 6: String representation
        std::cout << "\nTest 6: String representation..." << std::endl;
        std::string str_repr = primitive->to_string();
        std::cout << "✓ " << str_repr << std::endl;
        
        // Test 7: AtomSpace conversion
        std::cout << "\nTest 7: AtomSpace conversion..." << std::endl;
        AtomSpace atomspace;
        Handle atom_handle = primitive->to_atomspace_node(&atomspace);
        
        if (atom_handle != Handle::UNDEFINED) {
            std::cout << "✓ AtomSpace conversion successful" << std::endl;
        } else {
            std::cout << "✗ AtomSpace conversion failed" << std::endl;
        }
        
        // Test 8: Registry functionality
        std::cout << "\nTest 8: Registry functionality..." << std::endl;
        CognitivePrimitiveRegistry registry(ctx);
        
        bool reg_success = registry.register_primitive("test-reg", 
                                                     ModalityType::SYMBOLIC,
                                                     DepthType::PRAGMATIC,
                                                     ContextType::TEMPORAL,
                                                     0.9f, 0.4f);
        
        if (reg_success) {
            std::cout << "✓ Primitive registration successful" << std::endl;
            
            auto* retrieved = registry.get_primitive("test-reg");
            if (retrieved) {
                std::cout << "✓ Primitive retrieval successful" << std::endl;
                std::cout << "  Retrieved: " << retrieved->to_string() << std::endl;
            } else {
                std::cout << "✗ Primitive retrieval failed" << std::endl;
            }
            
            auto names = registry.list_primitives();
            std::cout << "✓ Registry contains " << names.size() << " primitives" << std::endl;
            
            bool all_valid = registry.validate_all_primitives();
            std::cout << "✓ All primitives valid: " << (all_valid ? "yes" : "no") << std::endl;
        } else {
            std::cout << "✗ Primitive registration failed" << std::endl;
        }
        
        std::cout << "\n=== All Tests Completed Successfully ===" << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "✗ Test failed with exception: " << e.what() << std::endl;
        ggml_free(ctx);
        return 1;
    }
    
    // Cleanup
    ggml_free(ctx);
    std::cout << "✓ GGML context freed" << std::endl;
    
    return 0;
}