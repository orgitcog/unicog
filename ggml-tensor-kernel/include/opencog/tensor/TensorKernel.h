/*
 * TensorKernel.h
 *
 * Unified GGML Tensor Kernel for OpenCog
 * 
 * This header defines the core tensor kernel that integrates all cognitive
 * modules (cogutil, atomspace, cogserver, etc.) as distributed tensor operations.
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_TENSOR_KERNEL_H
#define _OPENCOG_TENSOR_KERNEL_H

#include <memory>
#include <vector>
#include <unordered_map>
#include <string>
#include <functional>

#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

#include "ggml.h"
#include "TensorRegistry.h"
#include "AtomSpaceTensorMapper.h"
#include "AttentionAllocator.h"
#include "GrammarRegistry.h"
#include "DistributedExecutor.h"
#include "NeuralSymbolicBridge.h"

namespace opencog {

/**
 * TensorShape represents the dimensionality of a cognitive module's tensor field
 */
struct TensorShape {
    std::vector<size_t> dimensions;
    std::string module_name;
    std::string description;
    
    TensorShape() = default;
    TensorShape(const std::string& name, const std::vector<size_t>& dims, const std::string& desc = "")
        : module_name(name), dimensions(dims), description(desc) {}
    
    size_t total_elements() const {
        size_t total = 1;
        for (size_t dim : dimensions) {
            total *= dim;
        }
        return total;
    }
};

/**
 * CognitiveModule represents a cognitive component with tensor operations
 */
class CognitiveModule {
public:
    std::string name;
    TensorShape shape;
    ggml_tensor* tensor;
    std::function<ggml_tensor*(ggml_context*, const std::vector<ggml_tensor*>&)> operation;
    
    CognitiveModule(const std::string& module_name, const TensorShape& tensor_shape)
        : name(module_name), shape(tensor_shape), tensor(nullptr) {}
    
    virtual ~CognitiveModule() = default;
    
    // Initialize the tensor in the given context
    virtual void initialize_tensor(ggml_context* ctx);
    
    // Execute the module's tensor operation
    virtual ggml_tensor* execute(ggml_context* ctx, const std::vector<ggml_tensor*>& inputs);
};

/**
 * TensorKernel is the main unified tensor processing engine
 */
class TensorKernel {
private:
    std::unique_ptr<TensorRegistry> registry_;
    std::unique_ptr<AtomSpaceTensorMapper> atom_mapper_;
    std::unique_ptr<AttentionAllocator> attention_allocator_;
    std::unique_ptr<GrammarRegistry> grammar_registry_;
    std::unique_ptr<DistributedExecutor> distributed_executor_;
    std::unique_ptr<NeuralSymbolicBridge> neural_symbolic_bridge_;
    
    AtomSpace* atomspace_;
    ggml_context* main_context_;
    
    // Cognitive modules registry
    std::unordered_map<std::string, std::unique_ptr<CognitiveModule>> modules_;
    
    // Tensor field for distributed operations
    ggml_tensor* unified_tensor_field_;
    
    bool initialized_;
    
public:
    TensorKernel(AtomSpace* as = nullptr);
    ~TensorKernel();
    
    // Core initialization and cleanup
    bool initialize();
    void cleanup();
    
    // Module registration and management
    void register_module(const std::string& name, const TensorShape& shape);
    void register_standard_modules();
    CognitiveModule* get_module(const std::string& name);
    
    // Tensor operations
    ggml_tensor* create_tensor(const std::string& name, const std::vector<size_t>& shape);
    ggml_tensor* map_atomspace_to_tensor(const HandleSet& atoms);
    ggml_tensor* apply_attention_mask(ggml_tensor* input, const std::string& attention_type);
    
    // Neural-symbolic integration
    Handle execute_grammar_operation(const std::string& grammar_name, 
                                   const std::vector<Handle>& inputs);
    std::vector<Handle> tensor_to_atoms(ggml_tensor* tensor);
    
    // Distributed execution
    void enable_distributed_execution(const std::vector<std::string>& node_addresses);
    void execute_distributed(const std::string& operation_name, 
                           const std::vector<ggml_tensor*>& inputs);
    
    // Attention allocation
    void update_attention_weights(const std::vector<double>& feedback);
    ggml_tensor* get_attention_mask(const std::string& context);
    
    // Accessors
    AtomSpace* get_atomspace() const { return atomspace_; }
    ggml_context* get_context() const { return main_context_; }
    TensorRegistry* get_registry() const { return registry_.get(); }
    
    // Status and debugging
    bool is_initialized() const { return initialized_; }
    std::vector<std::string> get_registered_modules() const;
    void print_tensor_info(const std::string& name) const;
    
    // Static factory methods
    static std::unique_ptr<TensorKernel> create_instance(AtomSpace* as = nullptr);
    static TensorKernel* get_default_instance();
};

// Predefined tensor shapes for standard OpenCog modules
namespace StandardTensorShapes {
    extern const TensorShape COGUTIL_SHAPE;
    extern const TensorShape ATOMSPACE_SHAPE;
    extern const TensorShape COGSERVER_SHAPE;
    extern const TensorShape RELEX_SHAPE;
    extern const TensorShape PLN_SHAPE;
    extern const TensorShape ECAN_SHAPE;
    extern const TensorShape MOSES_SHAPE;
    extern const TensorShape GHOST_SHAPE;
    extern const TensorShape LOVING_AI_SHAPE;
    extern const TensorShape GAME_AI_SHAPE;
}

} // namespace opencog

#endif // _OPENCOG_TENSOR_KERNEL_H