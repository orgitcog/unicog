/*
 * NeuralSymbolicBridge.h
 *
 * Enhanced Neural-Symbolic Integration Bridge for Phase III transition
 * Implements advanced bi-directional mapping between neural and symbolic representations
 */

#ifndef _OPENCOG_NEURAL_SYMBOLIC_BRIDGE_H
#define _OPENCOG_NEURAL_SYMBOLIC_BRIDGE_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <functional>
#include <atomic>
#include <mutex>

namespace opencog {

/**
 * Neural Representation Structure
 * Represents neural network activations and patterns
 */
struct NeuralRepresentation {
    std::vector<double> activations;
    std::vector<double> weights;
    std::map<std::string, double> attention_scores;
    uint64_t timestamp;
    double confidence;
    std::string layer_id;
};

/**
 * Symbolic Representation Structure  
 * Represents symbolic knowledge structures
 */
struct SymbolicRepresentation {
    std::string atom_type;
    std::string atom_name;
    std::vector<std::string> relationships;
    std::map<std::string, double> truth_values;
    std::map<std::string, double> attention_values;
    uint64_t version;
    double semantic_weight;
};

/**
 * Integration Mapping
 * Defines bi-directional mappings between neural and symbolic representations
 */
struct IntegrationMapping {
    std::string mapping_id;
    std::string neural_layer;
    std::string symbolic_atom;
    double mapping_strength;
    std::vector<double> transformation_matrix;
    std::function<SymbolicRepresentation(const NeuralRepresentation&)> neural_to_symbolic;
    std::function<NeuralRepresentation(const SymbolicRepresentation&)> symbolic_to_neural;
};

/**
 * Enhanced Neural-Symbolic Integration Bridge
 * 
 * Provides advanced bi-directional integration between neural networks
 * and symbolic reasoning systems, supporting:
 * - Real-time translation between representations
 * - Adaptive mapping strength adjustment
 * - Emergent pattern recognition
 * - Attention-driven integration
 */
class NeuralSymbolicBridge
{
private:
    // Integration mappings
    std::map<std::string, IntegrationMapping> active_mappings_;
    std::map<std::string, std::vector<std::string>> neural_to_symbolic_index_;
    std::map<std::string, std::vector<std::string>> symbolic_to_neural_index_;
    
    // Attention mechanism
    std::map<std::string, double> neural_attention_weights_;
    std::map<std::string, double> symbolic_attention_weights_;
    double attention_decay_factor_;
    
    // Integration metrics
    std::atomic<uint64_t> integration_operations_;
    std::atomic<double> average_integration_time_;
    std::atomic<double> integration_accuracy_;
    
    // Adaptive parameters
    double mapping_learning_rate_;
    double confidence_threshold_;
    size_t max_active_mappings_;
    
    // Thread safety
    mutable std::mutex integration_mutex_;
    std::atomic<bool> integration_active_;

public:
    NeuralSymbolicBridge(double learning_rate = 0.01,
                        double confidence_threshold = 0.7,
                        size_t max_mappings = 1000);
    ~NeuralSymbolicBridge();

    /**
     * Create bi-directional mapping between neural and symbolic representations
     */
    bool create_mapping(const std::string& mapping_id,
                       const std::string& neural_layer,
                       const std::string& symbolic_atom,
                       double initial_strength = 0.5);

    /**
     * Translate neural representation to symbolic form
     */
    SymbolicRepresentation neural_to_symbolic(const NeuralRepresentation& neural_rep,
                                             const std::string& target_mapping = "");

    /**
     * Translate symbolic representation to neural form
     */
    NeuralRepresentation symbolic_to_neural(const SymbolicRepresentation& symbolic_rep,
                                           const std::string& target_mapping = "");

    /**
     * Update mapping strength based on integration success
     */
    void update_mapping_strength(const std::string& mapping_id,
                                double success_rate,
                                double confidence);

    /**
     * Perform attention-driven integration
     */
    struct AttentionIntegration {
        std::vector<SymbolicRepresentation> highlighted_symbols;
        std::vector<NeuralRepresentation> highlighted_neural;
        double attention_focus;
        std::string integration_context;
    };
    AttentionIntegration integrate_with_attention(
        const std::vector<NeuralRepresentation>& neural_patterns,
        const std::vector<SymbolicRepresentation>& symbolic_patterns);

    /**
     * Detect emergent patterns across neural-symbolic boundary
     */
    struct EmergentPattern {
        std::string pattern_id;
        std::vector<std::string> neural_components;
        std::vector<std::string> symbolic_components;
        double pattern_strength;
        double emergence_confidence;
        std::string description;
    };
    std::vector<EmergentPattern> detect_emergent_patterns(
        const std::vector<NeuralRepresentation>& neural_history,
        const std::vector<SymbolicRepresentation>& symbolic_history);

    /**
     * Adaptive learning for mapping optimization
     */
    void perform_adaptive_learning(const std::vector<std::pair<NeuralRepresentation, 
                                                              SymbolicRepresentation>>& training_pairs);

    /**
     * Get integration statistics
     */
    struct IntegrationStats {
        uint64_t total_integrations;
        double average_integration_time_ms;
        double integration_accuracy;
        size_t active_mappings;
        double neural_attention_coverage;
        double symbolic_attention_coverage;
        std::map<std::string, double> mapping_strengths;
    };
    IntegrationStats get_integration_statistics() const;

    /**
     * Set attention weights for neural components
     */
    void set_neural_attention(const std::map<std::string, double>& attention_weights);

    /**
     * Set attention weights for symbolic components  
     */
    void set_symbolic_attention(const std::map<std::string, double>& attention_weights);

    /**
     * Get current attention state
     */
    std::pair<std::map<std::string, double>, std::map<std::string, double>> get_attention_state() const;

    /**
     * Export integration mappings for analysis
     */
    std::vector<IntegrationMapping> export_mappings() const;

    /**
     * Import pre-trained integration mappings
     */
    bool import_mappings(const std::vector<IntegrationMapping>& mappings);

    /**
     * Clear all mappings and reset bridge
     */
    void reset_bridge();

    /**
     * Enable/disable integration processing
     */
    void set_integration_active(bool active);

    /**
     * Get mapping by ID
     */
    IntegrationMapping get_mapping(const std::string& mapping_id) const;

    /**
     * Remove mapping
     */
    bool remove_mapping(const std::string& mapping_id);

    /**
     * Get all active mapping IDs
     */
    std::vector<std::string> get_active_mapping_ids() const;

private:
    /**
     * Apply attention weighting to representations
     */
    double apply_attention_weighting(const std::string& component_id, bool is_neural);

    /**
     * Update integration metrics
     */
    void update_integration_metrics(double integration_time, double accuracy);

    /**
     * Prune weak mappings to maintain performance
     */
    void prune_weak_mappings();

    /**
     * Calculate semantic similarity between representations
     */
    double calculate_semantic_similarity(const NeuralRepresentation& neural,
                                        const SymbolicRepresentation& symbolic);

    /**
     * Generate default transformation functions
     */
    std::function<SymbolicRepresentation(const NeuralRepresentation&)> 
        create_default_neural_to_symbolic_transform();
    
    std::function<NeuralRepresentation(const SymbolicRepresentation&)> 
        create_default_symbolic_to_neural_transform();
};

} // namespace opencog

#endif // _OPENCOG_NEURAL_SYMBOLIC_BRIDGE_H