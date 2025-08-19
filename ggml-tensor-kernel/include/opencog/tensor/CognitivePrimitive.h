/*
 * CognitivePrimitive.h
 *
 * 5-dimensional cognitive primitive tensor implementation
 * 
 * This header defines the cognitive primitive tensor with the specific
 * 5-dimensional shape: [modality, depth, context, salience, autonomy_index]
 * as specified in Phase 1 requirements.
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_COGNITIVE_PRIMITIVE_H
#define _OPENCOG_COGNITIVE_PRIMITIVE_H

#include <vector>
#include <string>
#include <map>
#include <memory>
#include "atomspace_stub.h"
#include "ggml.h"

namespace opencog {

/**
 * Cognitive Primitive Tensor Dimensions as specified in Phase 1
 * 
 * Cognitive_Primitive_Tensor[5] = {
 *   modality: [visual, auditory, textual, symbolic],
 *   depth: [surface, semantic, pragmatic],
 *   context: [local, global, temporal],
 *   salience: [0.0, 1.0],
 *   autonomy_index: [0.0, 1.0]
 * }
 */

enum class ModalityType {
    VISUAL = 0,
    AUDITORY = 1, 
    TEXTUAL = 2,
    SYMBOLIC = 3
};

enum class DepthType {
    SURFACE = 0,
    SEMANTIC = 1,
    PRAGMATIC = 2
};

enum class ContextType {
    LOCAL = 0,
    GLOBAL = 1,
    TEMPORAL = 2
};

/**
 * CognitivePrimitiveTensor represents the 5-dimensional tensor structure
 * for encoding cognitive primitives with the specific signature
 */
class CognitivePrimitiveTensor {
public:
    // Standard tensor shape: [modality(4), depth(3), context(3), salience(continuous), autonomy(continuous)]
    static const std::vector<size_t> PRIMITIVE_SHAPE;
    
    // Dimension indices for the 5D tensor
    static const size_t MODALITY_DIM = 0;
    static const size_t DEPTH_DIM = 1;
    static const size_t CONTEXT_DIM = 2;
    static const size_t SALIENCE_DIM = 3;
    static const size_t AUTONOMY_DIM = 4;
    
private:
    ggml_tensor* tensor_;
    ggml_context* context_;
    std::string primitive_name_;
    
    // Tensor data accessors
    ModalityType modality_;
    DepthType depth_;
    ContextType context_type_;
    float salience_;        // [0.0, 1.0]
    float autonomy_index_;  // [0.0, 1.0]

public:
    /**
     * Constructor for cognitive primitive tensor
     */
    CognitivePrimitiveTensor(ggml_context* ctx, const std::string& name);
    
    /**
     * Destructor
     */
    ~CognitivePrimitiveTensor();
    
    /**
     * Set cognitive primitive values
     */
    void set_modality(ModalityType modality);
    void set_depth(DepthType depth);
    void set_context(ContextType context);
    void set_salience(float salience);
    void set_autonomy_index(float autonomy);
    
    /**
     * Get cognitive primitive values
     */
    ModalityType get_modality() const { return modality_; }
    DepthType get_depth() const { return depth_; }
    ContextType get_context() const { return context_type_; }
    float get_salience() const { return salience_; }
    float get_autonomy_index() const { return autonomy_index_; }
    
    /**
     * Encode the cognitive primitive values into the tensor
     */
    void encode_to_tensor();
    
    /**
     * Decode tensor values back to cognitive primitive structure
     */
    void decode_from_tensor();
    
    /**
     * Validate tensor shape and values
     */
    bool validate_tensor_shape() const;
    bool validate_primitive_values() const;
    
    /**
     * Get tensor handle for GGML operations
     */
    ggml_tensor* get_tensor() const { return tensor_; }
    
    /**
     * Get tensor shape information
     */
    std::vector<size_t> get_shape() const;
    size_t get_total_elements() const;
    
    /**
     * Degrees of freedom calculation
     */
    size_t calculate_degrees_of_freedom() const;
    
    /**
     * Prime factorization mapping for tensor signatures
     */
    std::vector<int> get_prime_factorization() const;
    
    /**
     * String representation for debugging
     */
    std::string to_string() const;
    
    /**
     * Convert to/from AtomSpace representation
     */
    Handle to_atomspace_node(AtomSpace* as) const;
    static std::unique_ptr<CognitivePrimitiveTensor> from_atomspace_node(
        ggml_context* ctx, const Handle& handle);
};

/**
 * CognitivePrimitiveRegistry manages collections of cognitive primitive tensors
 */
class CognitivePrimitiveRegistry {
private:
    std::map<std::string, std::unique_ptr<CognitivePrimitiveTensor>> primitives_;
    ggml_context* context_;
    
public:
    CognitivePrimitiveRegistry(ggml_context* ctx);
    ~CognitivePrimitiveRegistry();
    
    /**
     * Register a new cognitive primitive
     */
    bool register_primitive(const std::string& name, 
                          ModalityType modality, 
                          DepthType depth,
                          ContextType context,
                          float salience,
                          float autonomy);
    
    /**
     * Get registered primitive
     */
    CognitivePrimitiveTensor* get_primitive(const std::string& name);
    
    /**
     * List all registered primitives
     */
    std::vector<std::string> list_primitives() const;
    
    /**
     * Validate all registered primitives
     */
    bool validate_all_primitives() const;
    
    /**
     * Export/import primitive catalog
     */
    std::string export_catalog() const;
    bool import_catalog(const std::string& catalog_data);
};

// Utility functions for cognitive primitive tensor operations
namespace CognitivePrimitiveUtils {
    
    /**
     * Create a standard cognitive primitive with default values
     */
    std::unique_ptr<CognitivePrimitiveTensor> create_standard_primitive(
        ggml_context* ctx, const std::string& name);
    
    /**
     * Convert modality/depth/context enums to string
     */
    std::string modality_to_string(ModalityType modality);
    std::string depth_to_string(DepthType depth);
    std::string context_to_string(ContextType context);
    
    /**
     * Convert string to modality/depth/context enums
     */
    ModalityType string_to_modality(const std::string& str);
    DepthType string_to_depth(const std::string& str);
    ContextType string_to_context(const std::string& str);
    
    /**
     * Validate cognitive primitive tensor signature
     */
    bool validate_primitive_signature(const CognitivePrimitiveTensor& primitive);
}

} // namespace opencog

#endif // _OPENCOG_COGNITIVE_PRIMITIVE_H