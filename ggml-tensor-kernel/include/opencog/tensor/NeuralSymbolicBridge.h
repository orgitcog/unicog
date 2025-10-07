/*
 * NeuralSymbolicBridge.h
 *
 * Neural-symbolic integration bridge using Scheme macros and tensor operations
 */

#ifndef _OPENCOG_NEURAL_SYMBOLIC_BRIDGE_H
#define _OPENCOG_NEURAL_SYMBOLIC_BRIDGE_H

#include <memory>
#include <vector>
#include <unordered_map>
#include <string>
#include <functional>

#include "atomspace_stub.h"
#include "ggml.h"

namespace opencog {

/**
 * TransformationRule represents a neural-symbolic transformation
 */
struct TransformationRule {
    std::string name;
    std::string description;
    std::string scheme_macro;
    std::function<ggml_tensor*(ggml_tensor*)> tensor_transform;
    std::function<Handle(const Handle&)> symbolic_transform;
    
    TransformationRule() = default;
    TransformationRule(const std::string& rule_name, const std::string& desc)
        : name(rule_name), description(desc) {}
};

/**
 * NeuralSymbolicBridge manages integration between neural tensor operations and symbolic reasoning
 */
class NeuralSymbolicBridge {
private:
    AtomSpace* atomspace_;
    ggml_context* context_;
    SchemeEval* scheme_evaluator_;
    
    // Transformation rules
    std::unordered_map<std::string, TransformationRule> transformation_rules_;
    
    // Scheme macro registry
    std::unordered_map<std::string, std::string> scheme_macros_;
    
    // Bidirectional mapping caches
    std::unordered_map<Handle, ggml_tensor*> handle_to_tensor_;
    std::unordered_map<ggml_tensor*, Handle> tensor_to_handle_;
    
public:
    NeuralSymbolicBridge(AtomSpace* as, ggml_context* ctx);
    ~NeuralSymbolicBridge();
    
    // Initialization
    void initialize();
    void load_scheme_macros();
    
    // Neural-symbolic transformations
    ggml_tensor* symbolic_to_neural(const Handle& atom);
    Handle neural_to_symbolic(ggml_tensor* tensor);
    
    // Transformation rule management
    void register_transformation_rule(const TransformationRule& rule);
    bool has_transformation_rule(const std::string& name) const;
    const TransformationRule& get_transformation_rule(const std::string& name) const;
    
    // Execute transformations
    ggml_tensor* apply_neural_transformation(const std::string& rule_name, ggml_tensor* input);
    Handle apply_symbolic_transformation(const std::string& rule_name, const Handle& input);
    
    // Scheme macro system
    void register_scheme_macro(const std::string& name, const std::string& macro_definition);
    std::string execute_scheme_macro(const std::string& macro_name, const std::vector<std::string>& args);
    
    // Bidirectional reasoning
    Handle reason_with_tensors(const Handle& query, ggml_tensor* neural_context);
    ggml_tensor* enhance_reasoning_with_neural(const Handle& symbolic_knowledge);
    
    // Pattern matching with neural enhancement
    HandleSet pattern_match_with_neural_guidance(const Handle& pattern, ggml_tensor* guidance);
    ggml_tensor* extract_neural_patterns_from_symbolic(const HandleSet& symbolic_patterns);
    
    // Learning and adaptation
    void learn_transformation_from_examples(const std::vector<std::pair<Handle, ggml_tensor*>>& examples);
    void adapt_transformations_based_on_feedback(const std::vector<double>& feedback);
    
    // Standard OpenCog integrations
    void register_pln_integration();
    void register_ecan_integration();
    void register_pattern_matcher_integration();
    
    // Hypergraph neural operations
    ggml_tensor* compute_hypergraph_embeddings(const HandleSet& atoms);
    HandleSet reconstruct_hypergraph_from_embeddings(ggml_tensor* embeddings);
    
    // Cognitive primitives
    ggml_tensor* encode_cognitive_primitive(const std::string& primitive_name, const Handle& atom);
    Handle decode_cognitive_primitive(const std::string& primitive_name, ggml_tensor* tensor);
    
    // Emergent dynamics
    void analyze_emergent_patterns();
    std::vector<Handle> extract_emergent_symbolic_patterns();
    std::vector<ggml_tensor*> extract_emergent_neural_patterns();
    
    // Debugging and introspection
    void print_transformation_rules() const;
    void print_scheme_macros() const;
    void validate_neural_symbolic_consistency() const;
    
    // Configuration
    void set_scheme_evaluator(SchemeEval* evaluator);
    void set_transformation_threshold(double threshold);
    
    // Statistics
    size_t get_transformation_count() const;
    size_t get_cached_mappings_count() const;
    double get_transformation_accuracy() const;
    
    // Cleanup
    void clear_caches();
    void clear_transformation_rules();
    
private:
    // Internal helper methods
    void initialize_standard_transformations();
    void create_bidirectional_mapping(const Handle& atom, ggml_tensor* tensor);
    void cleanup_stale_mappings();
    float compute_symbolic_similarity(const Handle& atom1, const Handle& atom2);
    
    // Scheme integration helpers
    Handle evaluate_scheme_expression(const std::string& expression);
    std::string handle_to_scheme_string(const Handle& atom);
    Handle scheme_string_to_handle(const std::string& scheme_str);
    
    // Member variables (incomplete - need to see full private section)
    std::vector<double> feedback_history_;
};

} // namespace opencog

#endif // _OPENCOG_NEURAL_SYMBOLIC_BRIDGE_H