/*
 * GrammarRegistry.h
 *
 * Dynamic grammar registry for agentic functions and compositional kernels
 */

#ifndef _OPENCOG_GRAMMAR_REGISTRY_H
#define _OPENCOG_GRAMMAR_REGISTRY_H

#include <memory>
#include <vector>
#include <unordered_map>
#include <string>
#include <functional>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

#include "ggml.h"

namespace opencog {

/**
 * GrammarFunction represents a compositional grammar operation
 */
struct GrammarFunction {
    std::string name;
    std::string description;
    std::vector<std::string> input_types;
    std::string output_type;
    std::function<Handle(const std::vector<Handle>&)> symbolic_operation;
    std::function<ggml_tensor*(ggml_context*, const std::vector<ggml_tensor*>&)> tensor_operation;
    
    GrammarFunction(const std::string& func_name,
                   const std::string& desc,
                   const std::vector<std::string>& inputs,
                   const std::string& output)
        : name(func_name), description(desc), input_types(inputs), output_type(output) {}
};

/**
 * AgenticKernel represents an agentic function with both symbolic and tensor operations
 */
struct AgenticKernel {
    std::string name;
    std::string module_origin; // RelEx, MOSES, GHOST, etc.
    std::vector<GrammarFunction> functions;
    ggml_tensor* kernel_weights;
    std::unordered_map<std::string, std::string> configuration;
    
    AgenticKernel(const std::string& kernel_name, const std::string& origin)
        : name(kernel_name), module_origin(origin), kernel_weights(nullptr) {}
};

/**
 * GrammarRegistry manages dynamic grammar library and agentic function catalog
 */
class GrammarRegistry {
private:
    AtomSpace* atomspace_;
    ggml_context* context_;
    
    // Grammar and function registries
    std::unordered_map<std::string, GrammarFunction> grammar_functions_;
    std::unordered_map<std::string, AgenticKernel> agentic_kernels_;
    
    // Compositional grammars
    std::unordered_map<std::string, std::vector<std::string>> grammar_compositions_;
    
    // Dynamic loading
    std::vector<std::string> loaded_libraries_;
    
public:
    GrammarRegistry(AtomSpace* as, ggml_context* ctx);
    ~GrammarRegistry() = default;
    
    // Grammar function registration
    void register_grammar_function(const GrammarFunction& function);
    bool has_grammar_function(const std::string& name) const;
    const GrammarFunction& get_grammar_function(const std::string& name) const;
    
    // Agentic kernel registration
    void register_agentic_kernel(const AgenticKernel& kernel);
    bool has_agentic_kernel(const std::string& name) const;
    const AgenticKernel& get_agentic_kernel(const std::string& name) const;
    
    // Execute grammar operations
    Handle execute_grammar_operation(const std::string& function_name,
                                   const std::vector<Handle>& inputs);
    ggml_tensor* execute_tensor_operation(const std::string& function_name,
                                        const std::vector<ggml_tensor*>& inputs);
    
    // Compositional grammar creation
    void create_grammar_composition(const std::string& composition_name,
                                  const std::vector<std::string>& function_sequence);
    Handle execute_composition(const std::string& composition_name,
                             const std::vector<Handle>& inputs);
    
    // Standard OpenCog module registrations
    void register_relex_functions();
    void register_moses_functions();
    void register_ghost_functions();
    void register_pln_functions();
    void register_ecan_functions();
    void register_loving_ai_functions();
    void register_game_ai_functions();
    
    // Dynamic library loading
    void load_grammar_library(const std::string& library_path);
    void unload_grammar_library(const std::string& library_path);
    std::vector<std::string> get_loaded_libraries() const;
    
    // Pattern matching integration
    void register_pattern_matcher_functions();
    Handle execute_pattern_match(const Handle& pattern, const Handle& target);
    
    // Emergent grammar dynamics
    void analyze_grammar_usage();
    std::vector<std::string> suggest_grammar_compositions();
    void optimize_grammar_performance();
    
    // Catalog management
    void export_grammar_catalog(const std::string& filename) const;
    void import_grammar_catalog(const std::string& filename);
    
    // Statistics and introspection
    size_t get_grammar_function_count() const;
    size_t get_agentic_kernel_count() const;
    std::vector<std::string> get_registered_functions() const;
    std::vector<std::string> get_registered_kernels() const;
    
    // Configuration
    void set_kernel_configuration(const std::string& kernel_name,
                                const std::string& key,
                                const std::string& value);
    std::string get_kernel_configuration(const std::string& kernel_name,
                                       const std::string& key) const;
    
    // Debugging
    void print_registry_info() const;
    void validate_grammar_consistency() const;
};

} // namespace opencog

#endif // _OPENCOG_GRAMMAR_REGISTRY_H