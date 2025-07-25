/*
 * NeuralSymbolicBridge.cc
 *
 * Implementation of Enhanced Neural-Symbolic Integration Bridge
 */

#include "NeuralSymbolicBridge.h"
#include <iostream>
#include <algorithm>
#include <chrono>
#include <cmath>
#include <random>

namespace opencog {

NeuralSymbolicBridge::NeuralSymbolicBridge(double learning_rate,
                                         double confidence_threshold,
                                         size_t max_mappings)
    : mapping_learning_rate_(learning_rate)
    , confidence_threshold_(confidence_threshold)
    , max_active_mappings_(max_mappings)
    , attention_decay_factor_(0.95)
    , integration_operations_(0)
    , average_integration_time_(0.0)
    , integration_accuracy_(0.0)
    , integration_active_(true)
{
    std::cout << "Neural-Symbolic Bridge initialized with learning rate: " 
              << learning_rate << ", confidence threshold: " 
              << confidence_threshold << std::endl;
}

NeuralSymbolicBridge::~NeuralSymbolicBridge() {
    std::cout << "Neural-Symbolic Bridge shutting down. Total integrations: " 
              << integration_operations_.load() << std::endl;
}

bool NeuralSymbolicBridge::create_mapping(const std::string& mapping_id,
                                         const std::string& neural_layer,
                                         const std::string& symbolic_atom,
                                         double initial_strength) {
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    if (active_mappings_.size() >= max_active_mappings_) {
        std::cout << "Maximum mappings reached. Pruning weak mappings..." << std::endl;
        prune_weak_mappings();
    }
    
    IntegrationMapping mapping;
    mapping.mapping_id = mapping_id;
    mapping.neural_layer = neural_layer;
    mapping.symbolic_atom = symbolic_atom;
    mapping.mapping_strength = initial_strength;
    
    // Create default transformation functions
    mapping.neural_to_symbolic = create_default_neural_to_symbolic_transform();
    mapping.symbolic_to_neural = create_default_symbolic_to_neural_transform();
    
    active_mappings_[mapping_id] = mapping;
    
    // Update indices
    neural_to_symbolic_index_[neural_layer].push_back(mapping_id);
    symbolic_to_neural_index_[symbolic_atom].push_back(mapping_id);
    
    std::cout << "Created mapping: " << mapping_id 
              << " (Neural: " << neural_layer << " <-> Symbolic: " << symbolic_atom 
              << ") with strength: " << initial_strength << std::endl;
    
    return true;
}

SymbolicRepresentation NeuralSymbolicBridge::neural_to_symbolic(
    const NeuralRepresentation& neural_rep,
    const std::string& target_mapping) {
    
    if (!integration_active_.load()) {
        return SymbolicRepresentation{};
    }
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    SymbolicRepresentation result;
    
    if (!target_mapping.empty()) {
        // Use specific mapping
        auto it = active_mappings_.find(target_mapping);
        if (it != active_mappings_.end()) {
            result = it->second.neural_to_symbolic(neural_rep);
        }
    } else {
        // Find best mapping based on layer match and attention
        double best_score = 0.0;
        std::string best_mapping;
        
        for (const auto& pair : active_mappings_) {
            const IntegrationMapping& mapping = pair.second;
            if (mapping.neural_layer == neural_rep.layer_id) {
                double attention_weight = apply_attention_weighting(mapping.neural_layer, true);
                double score = mapping.mapping_strength * attention_weight;
                if (score > best_score) {
                    best_score = score;
                    best_mapping = pair.first;
                }
            }
        }
        
        if (!best_mapping.empty()) {
            result = active_mappings_[best_mapping].neural_to_symbolic(neural_rep);
        }
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
    
    integration_operations_.fetch_add(1);
    update_integration_metrics(duration.count() / 1000.0, result.semantic_weight);
    
    return result;
}

NeuralRepresentation NeuralSymbolicBridge::symbolic_to_neural(
    const SymbolicRepresentation& symbolic_rep,
    const std::string& target_mapping) {
    
    if (!integration_active_.load()) {
        return NeuralRepresentation{};
    }
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    NeuralRepresentation result;
    
    if (!target_mapping.empty()) {
        // Use specific mapping
        auto it = active_mappings_.find(target_mapping);
        if (it != active_mappings_.end()) {
            result = it->second.symbolic_to_neural(symbolic_rep);
        }
    } else {
        // Find best mapping based on atom match and attention
        double best_score = 0.0;
        std::string best_mapping;
        
        for (const auto& pair : active_mappings_) {
            const IntegrationMapping& mapping = pair.second;
            if (mapping.symbolic_atom == symbolic_rep.atom_name) {
                double attention_weight = apply_attention_weighting(mapping.symbolic_atom, false);
                double score = mapping.mapping_strength * attention_weight;
                if (score > best_score) {
                    best_score = score;
                    best_mapping = pair.first;
                }
            }
        }
        
        if (!best_mapping.empty()) {
            result = active_mappings_[best_mapping].symbolic_to_neural(symbolic_rep);
        }
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
    
    integration_operations_.fetch_add(1);
    update_integration_metrics(duration.count() / 1000.0, result.confidence);
    
    return result;
}

void NeuralSymbolicBridge::update_mapping_strength(const std::string& mapping_id,
                                                  double success_rate,
                                                  double confidence) {
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    auto it = active_mappings_.find(mapping_id);
    if (it != active_mappings_.end()) {
        IntegrationMapping& mapping = it->second;
        
        // Adaptive learning: strengthen successful mappings, weaken unsuccessful ones
        double adjustment = mapping_learning_rate_ * (success_rate - 0.5) * confidence;
        mapping.mapping_strength = std::clamp(mapping.mapping_strength + adjustment, 0.0, 1.0);
        
        std::cout << "Updated mapping strength for " << mapping_id 
                  << " to " << mapping.mapping_strength 
                  << " (adjustment: " << adjustment << ")" << std::endl;
    }
}

NeuralSymbolicBridge::AttentionIntegration NeuralSymbolicBridge::integrate_with_attention(
    const std::vector<NeuralRepresentation>& neural_patterns,
    const std::vector<SymbolicRepresentation>& symbolic_patterns) {
    
    AttentionIntegration result;
    result.attention_focus = 0.0;
    result.integration_context = "attention_driven_integration";
    
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    // Apply attention weighting to highlight important patterns
    for (const auto& neural_rep : neural_patterns) {
        double attention_weight = apply_attention_weighting(neural_rep.layer_id, true);
        if (attention_weight > confidence_threshold_) {
            result.highlighted_neural.push_back(neural_rep);
            result.attention_focus += attention_weight;
        }
    }
    
    for (const auto& symbolic_rep : symbolic_patterns) {
        double attention_weight = apply_attention_weighting(symbolic_rep.atom_name, false);
        if (attention_weight > confidence_threshold_) {
            result.highlighted_symbols.push_back(symbolic_rep);
            result.attention_focus += attention_weight;
        }
    }
    
    // Normalize attention focus
    if (!result.highlighted_neural.empty() || !result.highlighted_symbols.empty()) {
        result.attention_focus /= (result.highlighted_neural.size() + result.highlighted_symbols.size());
    }
    
    std::cout << "Attention integration completed. Focus: " << result.attention_focus 
              << ", Neural patterns: " << result.highlighted_neural.size()
              << ", Symbolic patterns: " << result.highlighted_symbols.size() << std::endl;
    
    return result;
}

std::vector<NeuralSymbolicBridge::EmergentPattern> NeuralSymbolicBridge::detect_emergent_patterns(
    const std::vector<NeuralRepresentation>& neural_history,
    const std::vector<SymbolicRepresentation>& symbolic_history) {
    
    std::vector<EmergentPattern> patterns;
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    // Simple pattern detection based on co-occurrence and temporal proximity
    for (size_t i = 0; i < neural_history.size(); ++i) {
        for (size_t j = 0; j < symbolic_history.size(); ++j) {
            double similarity = calculate_semantic_similarity(neural_history[i], symbolic_history[j]);
            
            if (similarity > confidence_threshold_) {
                EmergentPattern pattern;
                pattern.pattern_id = "pattern_" + std::to_string(patterns.size());
                pattern.neural_components.push_back(neural_history[i].layer_id);
                pattern.symbolic_components.push_back(symbolic_history[j].atom_name);
                pattern.pattern_strength = similarity;
                pattern.emergence_confidence = neural_history[i].confidence * symbolic_history[j].semantic_weight;
                pattern.description = "Emergent correlation between " + neural_history[i].layer_id + 
                                    " and " + symbolic_history[j].atom_name;
                
                patterns.push_back(pattern);
            }
        }
    }
    
    std::cout << "Detected " << patterns.size() << " emergent patterns" << std::endl;
    return patterns;
}

NeuralSymbolicBridge::IntegrationStats NeuralSymbolicBridge::get_integration_statistics() const {
    std::lock_guard<std::mutex> lock(integration_mutex_);
    
    IntegrationStats stats;
    stats.total_integrations = integration_operations_.load();
    stats.average_integration_time_ms = average_integration_time_.load();
    stats.integration_accuracy = integration_accuracy_.load();
    stats.active_mappings = active_mappings_.size();
    
    // Calculate attention coverage
    stats.neural_attention_coverage = neural_attention_weights_.size() > 0 ? 
        std::accumulate(neural_attention_weights_.begin(), neural_attention_weights_.end(), 0.0,
            [](double sum, const auto& pair) { return sum + pair.second; }) / neural_attention_weights_.size() : 0.0;
    
    stats.symbolic_attention_coverage = symbolic_attention_weights_.size() > 0 ?
        std::accumulate(symbolic_attention_weights_.begin(), symbolic_attention_weights_.end(), 0.0,
            [](double sum, const auto& pair) { return sum + pair.second; }) / symbolic_attention_weights_.size() : 0.0;
    
    // Export mapping strengths
    for (const auto& pair : active_mappings_) {
        stats.mapping_strengths[pair.first] = pair.second.mapping_strength;
    }
    
    return stats;
}

void NeuralSymbolicBridge::set_neural_attention(const std::map<std::string, double>& attention_weights) {
    std::lock_guard<std::mutex> lock(integration_mutex_);
    neural_attention_weights_ = attention_weights;
    std::cout << "Updated neural attention weights for " << attention_weights.size() << " components" << std::endl;
}

void NeuralSymbolicBridge::set_symbolic_attention(const std::map<std::string, double>& attention_weights) {
    std::lock_guard<std::mutex> lock(integration_mutex_);
    symbolic_attention_weights_ = attention_weights;
    std::cout << "Updated symbolic attention weights for " << attention_weights.size() << " components" << std::endl;
}

// Private helper methods
double NeuralSymbolicBridge::apply_attention_weighting(const std::string& component_id, bool is_neural) {
    if (is_neural) {
        auto it = neural_attention_weights_.find(component_id);
        return (it != neural_attention_weights_.end()) ? it->second : 0.5; // default weight
    } else {
        auto it = symbolic_attention_weights_.find(component_id);
        return (it != symbolic_attention_weights_.end()) ? it->second : 0.5; // default weight
    }
}

void NeuralSymbolicBridge::update_integration_metrics(double integration_time, double accuracy) {
    // Simple exponential moving average
    double alpha = 0.1;
    double current_avg = average_integration_time_.load();
    average_integration_time_.store(alpha * integration_time + (1 - alpha) * current_avg);
    
    double current_acc = integration_accuracy_.load();
    integration_accuracy_.store(alpha * accuracy + (1 - alpha) * current_acc);
}

void NeuralSymbolicBridge::prune_weak_mappings() {
    // Remove mappings with strength below threshold
    auto it = active_mappings_.begin();
    while (it != active_mappings_.end()) {
        if (it->second.mapping_strength < 0.1) {
            std::cout << "Pruning weak mapping: " << it->first << " (strength: " << it->second.mapping_strength << ")" << std::endl;
            it = active_mappings_.erase(it);
        } else {
            ++it;
        }
    }
}

double NeuralSymbolicBridge::calculate_semantic_similarity(const NeuralRepresentation& neural,
                                                          const SymbolicRepresentation& symbolic) {
    // Simple similarity based on confidence and attention scores
    double neural_score = neural.confidence;
    double symbolic_score = symbolic.semantic_weight;
    
    // Add attention bonus if available
    for (const auto& pair : neural.attention_scores) {
        neural_score += pair.second * 0.1;
    }
    
    for (const auto& pair : symbolic.attention_values) {
        symbolic_score += pair.second * 0.1;
    }
    
    return std::min(1.0, (neural_score + symbolic_score) / 2.0);
}

std::function<SymbolicRepresentation(const NeuralRepresentation&)> 
NeuralSymbolicBridge::create_default_neural_to_symbolic_transform() {
    return [](const NeuralRepresentation& neural) -> SymbolicRepresentation {
        SymbolicRepresentation symbolic;
        symbolic.atom_type = "ConceptNode";
        symbolic.atom_name = "neural_" + neural.layer_id;
        symbolic.semantic_weight = neural.confidence;
        symbolic.version = neural.timestamp;
        
        // Convert neural activations to truth values
        if (!neural.activations.empty()) {
            double avg_activation = std::accumulate(neural.activations.begin(), neural.activations.end(), 0.0) / neural.activations.size();
            symbolic.truth_values["strength"] = avg_activation;
            symbolic.truth_values["confidence"] = neural.confidence;
        }
        
        return symbolic;
    };
}

std::function<NeuralRepresentation(const SymbolicRepresentation&)> 
NeuralSymbolicBridge::create_default_symbolic_to_neural_transform() {
    return [](const SymbolicRepresentation& symbolic) -> NeuralRepresentation {
        NeuralRepresentation neural;
        neural.layer_id = "symbolic_" + symbolic.atom_name;
        neural.confidence = symbolic.semantic_weight;
        neural.timestamp = symbolic.version;
        
        // Convert truth values to neural activations
        auto strength_it = symbolic.truth_values.find("strength");
        if (strength_it != symbolic.truth_values.end()) {
            neural.activations.push_back(strength_it->second);
        }
        
        auto confidence_it = symbolic.truth_values.find("confidence");
        if (confidence_it != symbolic.truth_values.end()) {
            neural.weights.push_back(confidence_it->second);
        }
        
        return neural;
    };
}

void NeuralSymbolicBridge::set_integration_active(bool active) {
    integration_active_.store(active);
    std::cout << "Neural-Symbolic integration " << (active ? "enabled" : "disabled") << std::endl;
}

void NeuralSymbolicBridge::reset_bridge() {
    std::lock_guard<std::mutex> lock(integration_mutex_);
    active_mappings_.clear();
    neural_to_symbolic_index_.clear();
    symbolic_to_neural_index_.clear();
    neural_attention_weights_.clear();
    symbolic_attention_weights_.clear();
    integration_operations_.store(0);
    average_integration_time_.store(0.0);
    integration_accuracy_.store(0.0);
    std::cout << "Neural-Symbolic Bridge reset completed" << std::endl;
}

} // namespace opencog