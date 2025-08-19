/*
 * CognitivePrimitiveGuile.cc
 *
 * Guile/Scheme bindings for CognitivePrimitive tensor operations
 *
 * Copyright (c) 2025 OpenCog Foundation
 */

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/util/Logger.h>

#include "CognitivePrimitive.h"

using namespace opencog;

// Global registry for managing cognitive primitives
static std::unique_ptr<CognitivePrimitiveRegistry> global_registry;
static ggml_context* global_context = nullptr;

// Helper function to ensure context is initialized
static ggml_context* ensure_context() {
    if (!global_context) {
        struct ggml_init_params params = {
            .mem_size   = 1024 * 1024 * 16, // 16MB
            .mem_buffer = nullptr,
            .no_alloc   = false,
        };
        global_context = ggml_init(params);
        
        if (!global_context) {
            throw std::runtime_error("Failed to initialize GGML context");
        }
        
        global_registry = std::make_unique<CognitivePrimitiveRegistry>(global_context);
        logger().info("Initialized cognitive primitive GGML context");
    }
    return global_context;
}

// Scheme binding functions

/**
 * Create a new cognitive primitive tensor
 */
SCM ss_create_cognitive_primitive(SCM name_scm, SCM modality_scm, SCM depth_scm, 
                                 SCM context_scm, SCM salience_scm, SCM autonomy_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    std::string modality_str = scm_to_locale_string(modality_scm);
    std::string depth_str = scm_to_locale_string(depth_scm);
    std::string context_str = scm_to_locale_string(context_scm);
    double salience = scm_to_double(salience_scm);
    double autonomy = scm_to_double(autonomy_scm);
    
    try {
        ModalityType modality = CognitivePrimitiveUtils::string_to_modality(modality_str);
        DepthType depth = CognitivePrimitiveUtils::string_to_depth(depth_str);
        ContextType context = CognitivePrimitiveUtils::string_to_context(context_str);
        
        bool success = global_registry->register_primitive(name, modality, depth, 
                                                         context, salience, autonomy);
        return success ? SCM_BOOL_T : SCM_BOOL_F;
    } catch (const std::exception& e) {
        logger().error("Failed to create cognitive primitive: %s", e.what());
        return SCM_BOOL_F;
    }
}

/**
 * Get cognitive primitive information
 */
SCM ss_get_cognitive_primitive(SCM name_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
    
    if (!primitive) {
        return SCM_BOOL_F;
    }
    
    // Return alist with primitive information
    SCM result = SCM_EOL;
    
    result = scm_acons(scm_from_locale_symbol("name"), 
                      scm_from_locale_string(name.c_str()), result);
    
    result = scm_acons(scm_from_locale_symbol("modality"), 
                      scm_from_locale_string(
                          CognitivePrimitiveUtils::modality_to_string(primitive->get_modality()).c_str()), 
                      result);
    
    result = scm_acons(scm_from_locale_symbol("depth"), 
                      scm_from_locale_string(
                          CognitivePrimitiveUtils::depth_to_string(primitive->get_depth()).c_str()), 
                      result);
    
    result = scm_acons(scm_from_locale_symbol("context"), 
                      scm_from_locale_string(
                          CognitivePrimitiveUtils::context_to_string(primitive->get_context()).c_str()), 
                      result);
    
    result = scm_acons(scm_from_locale_symbol("salience"), 
                      scm_from_double(primitive->get_salience()), result);
    
    result = scm_acons(scm_from_locale_symbol("autonomy"), 
                      scm_from_double(primitive->get_autonomy_index()), result);
    
    return result;
}

/**
 * Convert cognitive primitive to AtomSpace node
 */
SCM ss_cognitive_primitive_to_atomspace(SCM name_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
    
    if (!primitive) {
        return SCM_BOOL_F;
    }
    
    AtomSpace* as = SchemeSmob::ss_get_env_as("cognitive-primitive-to-atomspace").get();
    if (!as) {
        logger().error("No AtomSpace available");
        return SCM_BOOL_F;
    }
    
    Handle result = primitive->to_atomspace_node(as);
    return SchemeSmob::handle_to_scm(result);
}

/**
 * Validate cognitive primitive tensor shape
 */
SCM ss_validate_cognitive_primitive(SCM name_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
    
    if (!primitive) {
        return SCM_BOOL_F;
    }
    
    bool shape_valid = primitive->validate_tensor_shape();
    bool values_valid = primitive->validate_primitive_values();
    
    return (shape_valid && values_valid) ? SCM_BOOL_T : SCM_BOOL_F;
}

/**
 * Get prime factorization for cognitive primitive
 */
SCM ss_cognitive_primitive_prime_factorization(SCM name_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
    
    if (!primitive) {
        return SCM_EOL;
    }
    
    auto primes = primitive->get_prime_factorization();
    SCM result = SCM_EOL;
    
    for (auto it = primes.rbegin(); it != primes.rend(); ++it) {
        result = scm_cons(scm_from_int(*it), result);
    }
    
    return result;
}

/**
 * Get degrees of freedom for cognitive primitive
 */
SCM ss_cognitive_primitive_degrees_of_freedom(SCM name_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
    
    if (!primitive) {
        return scm_from_size_t(0);
    }
    
    return scm_from_size_t(primitive->calculate_degrees_of_freedom());
}

/**
 * List all registered cognitive primitives
 */
SCM ss_list_cognitive_primitives() {
    ensure_context();
    
    auto names = global_registry->list_primitives();
    SCM result = SCM_EOL;
    
    for (auto it = names.rbegin(); it != names.rend(); ++it) {
        result = scm_cons(scm_from_locale_string(it->c_str()), result);
    }
    
    return result;
}

/**
 * Get cognitive primitive tensor shape
 */
SCM ss_cognitive_primitive_tensor_shape() {
    SCM result = SCM_EOL;
    
    for (auto it = CognitivePrimitiveTensor::PRIMITIVE_SHAPE.rbegin(); 
         it != CognitivePrimitiveTensor::PRIMITIVE_SHAPE.rend(); ++it) {
        result = scm_cons(scm_from_size_t(*it), result);
    }
    
    return result;
}

/**
 * Perform round-trip validation between tensor and AtomSpace
 */
SCM ss_validate_round_trip_accuracy(SCM name_scm) {
    ensure_context();
    
    std::string name = scm_to_locale_string(name_scm);
    CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
    
    if (!primitive) {
        return SCM_BOOL_F;
    }
    
    AtomSpace* as = SchemeSmob::ss_get_env_as("validate-round-trip-accuracy").get();
    if (!as) {
        logger().error("No AtomSpace available");
        return SCM_BOOL_F;
    }
    
    try {
        // Convert to AtomSpace
        Handle atom_handle = primitive->to_atomspace_node(as);
        
        // Convert back from AtomSpace (simplified validation)
        auto recovered = CognitivePrimitiveTensor::from_atomspace_node(global_context, atom_handle);
        
        if (!recovered) {
            return SCM_BOOL_F;
        }
        
        // Validate that key properties match
        bool modality_match = (recovered->get_modality() == primitive->get_modality());
        bool depth_match = (recovered->get_depth() == primitive->get_depth());
        bool context_match = (recovered->get_context() == primitive->get_context());
        bool salience_match = (std::abs(recovered->get_salience() - primitive->get_salience()) < 0.01f);
        bool autonomy_match = (std::abs(recovered->get_autonomy_index() - primitive->get_autonomy_index()) < 0.01f);
        
        return (modality_match && depth_match && context_match && 
                salience_match && autonomy_match) ? SCM_BOOL_T : SCM_BOOL_F;
                
    } catch (const std::exception& e) {
        logger().error("Round-trip validation failed: %s", e.what());
        return SCM_BOOL_F;
    }
}

/**
 * Run comprehensive cognitive primitive tests
 */
SCM ss_run_cognitive_primitive_tests() {
    ensure_context();
    
    logger().info("Running cognitive primitive tests...");
    
    // Create test primitives
    std::vector<std::tuple<std::string, ModalityType, DepthType, ContextType, float, float>> test_cases = {
        {"test-visual", ModalityType::VISUAL, DepthType::SURFACE, ContextType::LOCAL, 0.8f, 0.6f},
        {"test-auditory", ModalityType::AUDITORY, DepthType::SEMANTIC, ContextType::GLOBAL, 0.5f, 0.7f},
        {"test-symbolic", ModalityType::SYMBOLIC, DepthType::PRAGMATIC, ContextType::TEMPORAL, 0.9f, 0.4f}
    };
    
    int passed_tests = 0;
    int total_tests = 0;
    
    for (const auto& test_case : test_cases) {
        const std::string& name = std::get<0>(test_case);
        ModalityType modality = std::get<1>(test_case);
        DepthType depth = std::get<2>(test_case);
        ContextType context = std::get<3>(test_case);
        float salience = std::get<4>(test_case);
        float autonomy = std::get<5>(test_case);
        
        // Test primitive creation
        total_tests++;
        if (global_registry->register_primitive(name, modality, depth, context, salience, autonomy)) {
            passed_tests++;
            logger().info("Test %s: primitive creation PASSED", name.c_str());
        } else {
            logger().warn("Test %s: primitive creation FAILED", name.c_str());
        }
        
        // Test validation
        total_tests++;
        CognitivePrimitiveTensor* primitive = global_registry->get_primitive(name);
        if (primitive && primitive->validate_tensor_shape() && primitive->validate_primitive_values()) {
            passed_tests++;
            logger().info("Test %s: validation PASSED", name.c_str());
        } else {
            logger().warn("Test %s: validation FAILED", name.c_str());
        }
    }
    
    // Test registry validation
    total_tests++;
    if (global_registry->validate_all_primitives()) {
        passed_tests++;
        logger().info("Test: registry validation PASSED");
    } else {
        logger().warn("Test: registry validation FAILED");
    }
    
    logger().info("Cognitive primitive tests completed: %d/%d passed", passed_tests, total_tests);
    
    // Return test results as alist
    SCM result = SCM_EOL;
    result = scm_acons(scm_from_locale_symbol("total-tests"), 
                      scm_from_int(total_tests), result);
    result = scm_acons(scm_from_locale_symbol("passed-tests"), 
                      scm_from_int(passed_tests), result);
    result = scm_acons(scm_from_locale_symbol("success-rate"), 
                      scm_from_double((double)passed_tests / total_tests), result);
    
    return result;
}

/**
 * Cleanup cognitive primitive resources
 */
SCM ss_cleanup_cognitive_primitives() {
    if (global_registry) {
        global_registry.reset();
    }
    
    if (global_context) {
        ggml_free(global_context);
        global_context = nullptr;
    }
    
    logger().info("Cleaned up cognitive primitive resources");
    return SCM_BOOL_T;
}

// Module initialization
extern "C" {
void opencog_cognitive_primitive_init(void);
}

void opencog_cognitive_primitive_init(void)
{
    // Register Scheme primitives
    scm_c_define_gsubr("cog-create-cognitive-primitive", 6, 0, 0, 
                      (scm_t_subr) ss_create_cognitive_primitive);
    
    scm_c_define_gsubr("cog-get-cognitive-primitive", 1, 0, 0, 
                      (scm_t_subr) ss_get_cognitive_primitive);
    
    scm_c_define_gsubr("cog-cognitive-primitive-to-atomspace", 1, 0, 0, 
                      (scm_t_subr) ss_cognitive_primitive_to_atomspace);
    
    scm_c_define_gsubr("cog-validate-cognitive-primitive", 1, 0, 0, 
                      (scm_t_subr) ss_validate_cognitive_primitive);
    
    scm_c_define_gsubr("cog-cognitive-primitive-prime-factorization", 1, 0, 0, 
                      (scm_t_subr) ss_cognitive_primitive_prime_factorization);
    
    scm_c_define_gsubr("cog-cognitive-primitive-degrees-of-freedom", 1, 0, 0, 
                      (scm_t_subr) ss_cognitive_primitive_degrees_of_freedom);
    
    scm_c_define_gsubr("cog-list-cognitive-primitives", 0, 0, 0, 
                      (scm_t_subr) ss_list_cognitive_primitives);
    
    scm_c_define_gsubr("cog-cognitive-primitive-tensor-shape", 0, 0, 0, 
                      (scm_t_subr) ss_cognitive_primitive_tensor_shape);
    
    scm_c_define_gsubr("cog-validate-round-trip-accuracy", 1, 0, 0, 
                      (scm_t_subr) ss_validate_round_trip_accuracy);
    
    scm_c_define_gsubr("cog-run-cognitive-primitive-tests", 0, 0, 0, 
                      (scm_t_subr) ss_run_cognitive_primitive_tests);
    
    scm_c_define_gsubr("cog-cleanup-cognitive-primitives", 0, 0, 0, 
                      (scm_t_subr) ss_cleanup_cognitive_primitives);
}