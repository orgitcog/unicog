/*
 * AttentionAllocator.h
 *
 * Implements ECAN (Economic Attention Networks) as adaptive tensor attention masks
 */

#ifndef _OPENCOG_ATTENTION_ALLOCATOR_H
#define _OPENCOG_ATTENTION_ALLOCATOR_H

#include <memory>
#include <vector>
#include <unordered_map>
#include <string>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

#include "ggml.h"

namespace opencog {

/**
 * AttentionContext represents different attention allocation contexts
 */
struct AttentionContext {
    std::string name;
    std::vector<double> weights;
    std::vector<std::string> focus_areas;
    double decay_rate;
    
    AttentionContext(const std::string& context_name, 
                    const std::vector<double>& attention_weights,
                    double decay = 0.95)
        : name(context_name), weights(attention_weights), decay_rate(decay) {}
};

/**
 * AttentionAllocator implements ECAN-style attention allocation using tensor operations
 */
class AttentionAllocator {
private:
    AtomSpace* atomspace_;
    ggml_context* context_;
    
    // Attention contexts
    std::unordered_map<std::string, AttentionContext> contexts_;
    
    // Attention tensors
    ggml_tensor* attention_weights_;
    ggml_tensor* attention_mask_;
    ggml_tensor* economic_flow_;
    
    // ECAN parameters
    double total_attention_budget_;
    double stimulation_threshold_;
    double decay_rate_;
    double rent_collection_rate_;
    
    // Attention allocation history
    std::vector<std::vector<double>> attention_history_;
    size_t max_history_size_;
    
public:
    AttentionAllocator(AtomSpace* as, ggml_context* ctx);
    ~AttentionAllocator() = default;
    
    // Initialize attention system
    void initialize(size_t num_atoms, double budget = 1000.0);
    
    // Attention allocation
    ggml_tensor* allocate_attention(const HandleSet& atoms);
    ggml_tensor* compute_attention_mask(const std::string& context_name);
    void update_attention_weights(const std::vector<double>& feedback);
    
    // Economic attention flow
    void simulate_economic_flow(const HandleSet& atoms);
    ggml_tensor* get_economic_flow_tensor();
    void apply_rent_collection();
    
    // Attention contexts
    void register_context(const std::string& name, const AttentionContext& context);
    void switch_context(const std::string& context_name);
    std::vector<std::string> get_registered_contexts() const;
    
    // Adaptive attention mechanisms
    void adapt_attention_based_on_feedback(const std::vector<double>& performance_feedback);
    void adapt_attention_based_on_usage(const std::vector<Handle>& recently_used_atoms);
    
    // ECAN-specific operations
    void stimulate_atoms(const HandleSet& atoms, double stimulation_amount);
    void apply_decay();
    HandleSet get_atoms_above_threshold(double threshold);
    
    // Attention statistics
    double get_total_attention() const;
    double get_attention_for_atom(const Handle& atom) const;
    std::vector<Handle> get_most_attended_atoms(size_t count) const;
    
    // Membrane routing (P-System inspired)
    void setup_membrane_boundaries(const std::vector<HandleSet>& membranes);
    ggml_tensor* route_attention_through_membranes(ggml_tensor* input);
    
    // Tensor operations
    ggml_tensor* apply_attention_mask(ggml_tensor* input, const std::string& context);
    ggml_tensor* compute_attention_gradients(ggml_tensor* loss);
    
    // Configuration
    void set_attention_budget(double budget);
    void set_decay_rate(double rate);
    void set_stimulation_threshold(double threshold);
    
    // Statistics and debugging
    void print_attention_distribution() const;
    void export_attention_history(const std::string& filename) const;
    std::vector<double> get_attention_weights_vector() const;
    
    // Accessors
    ggml_tensor* get_attention_weights() const { return attention_weights_; }
    ggml_tensor* get_attention_mask() const { return attention_mask_; }
    double get_total_budget() const { return total_attention_budget_; }
};

} // namespace opencog

#endif // _OPENCOG_ATTENTION_ALLOCATOR_H