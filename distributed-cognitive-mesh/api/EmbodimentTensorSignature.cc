/*
 * EmbodimentTensorSignature.cc
 *
 * Implementation of Embodiment Tensor Signature system
 */

#include "EmbodimentTensorSignature.h"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <sstream>
#include <iomanip>

namespace opencog {
namespace embodiment {

// ===== EmbodimentTensorSignature Implementation =====

EmbodimentTensorSignature::EmbodimentTensorSignature() 
    : action_confidence(0.5), tensor_validity(1.0) {
    // Initialize arrays to zero
    sensory_modality.fill(0.0);
    motor_command.fill(0.0);
    temporal_context.fill(0.0);
    embodiment_state.fill(0.0);
    interaction_mode.fill(0.0);
    feedback_loop.fill(0.0);
    
    // Initialize spatial coordinates
    spatial_coordinates = {0.0, 0.0, 0.0, 0.0};
    
    // Set timestamp
    timestamp = std::chrono::steady_clock::now();
    
    // Default to virtual embodiment in present context
    embodiment_state[VIRTUAL] = 1.0;
    temporal_context[PRESENT] = 1.0;
    interaction_mode[PASSIVE] = 1.0;
    feedback_loop[OPEN] = 1.0;
}

EmbodimentTensorSignature::EmbodimentTensorSignature(const std::string& agent_id) 
    : EmbodimentTensorSignature() {
    this->agent_id = agent_id;
}

std::vector<double> EmbodimentTensorSignature::to_vector() const {
    std::vector<double> result;
    result.reserve(element_count());
    
    // Add sensory modality (4 elements)
    for (const auto& val : sensory_modality) {
        result.push_back(val);
    }
    
    // Add motor command (3 elements)
    for (const auto& val : motor_command) {
        result.push_back(val);
    }
    
    // Add spatial coordinates (4 elements)
    result.push_back(spatial_coordinates.x);
    result.push_back(spatial_coordinates.y);
    result.push_back(spatial_coordinates.z);
    result.push_back(spatial_coordinates.orientation);
    
    // Add temporal context (3 elements)
    for (const auto& val : temporal_context) {
        result.push_back(val);
    }
    
    // Add action confidence (1 element)
    result.push_back(action_confidence);
    
    // Add embodiment state (3 elements)
    for (const auto& val : embodiment_state) {
        result.push_back(val);
    }
    
    // Add interaction mode (3 elements)
    for (const auto& val : interaction_mode) {
        result.push_back(val);
    }
    
    // Add feedback loop (3 elements)
    for (const auto& val : feedback_loop) {
        result.push_back(val);
    }
    
    return result;
}

void EmbodimentTensorSignature::from_vector(const std::vector<double>& data) {
    if (data.size() != element_count()) {
        return; // Invalid data size
    }
    
    size_t idx = 0;
    
    // Load sensory modality (4 elements)
    for (size_t i = 0; i < sensory_modality.size(); ++i, ++idx) {
        sensory_modality[i] = data[idx];
    }
    
    // Load motor command (3 elements)
    for (size_t i = 0; i < motor_command.size(); ++i, ++idx) {
        motor_command[i] = data[idx];
    }
    
    // Load spatial coordinates (4 elements)
    spatial_coordinates.x = data[idx++];
    spatial_coordinates.y = data[idx++];
    spatial_coordinates.z = data[idx++];
    spatial_coordinates.orientation = data[idx++];
    
    // Load temporal context (3 elements)
    for (size_t i = 0; i < temporal_context.size(); ++i, ++idx) {
        temporal_context[i] = data[idx];
    }
    
    // Load action confidence (1 element)
    action_confidence = data[idx++];
    
    // Load embodiment state (3 elements)
    for (size_t i = 0; i < embodiment_state.size(); ++i, ++idx) {
        embodiment_state[i] = data[idx];
    }
    
    // Load interaction mode (3 elements)
    for (size_t i = 0; i < interaction_mode.size(); ++i, ++idx) {
        interaction_mode[i] = data[idx];
    }
    
    // Load feedback loop (3 elements)
    for (size_t i = 0; i < feedback_loop.size(); ++i, ++idx) {
        feedback_loop[i] = data[idx];
    }
    
    // Update timestamp
    timestamp = std::chrono::steady_clock::now();
}

bool EmbodimentTensorSignature::validate() const {
    // Check action confidence range
    if (action_confidence < 0.0 || action_confidence > 1.0) {
        return false;
    }
    
    // Check tensor validity
    if (tensor_validity < 0.0 || tensor_validity > 1.0) {
        return false;
    }
    
    // Check for NaN values
    for (const auto& val : sensory_modality) {
        if (std::isnan(val) || std::isinf(val)) return false;
    }
    for (const auto& val : motor_command) {
        if (std::isnan(val) || std::isinf(val)) return false;
    }
    for (const auto& val : temporal_context) {
        if (std::isnan(val) || std::isinf(val)) return false;
    }
    for (const auto& val : embodiment_state) {
        if (std::isnan(val) || std::isinf(val)) return false;
    }
    for (const auto& val : interaction_mode) {
        if (std::isnan(val) || std::isinf(val)) return false;
    }
    for (const auto& val : feedback_loop) {
        if (std::isnan(val) || std::isinf(val)) return false;
    }
    
    return true;
}

double EmbodimentTensorSignature::magnitude() const {
    std::vector<double> vec = to_vector();
    double sum_squares = 0.0;
    for (double val : vec) {
        sum_squares += val * val;
    }
    return std::sqrt(sum_squares);
}

void EmbodimentTensorSignature::normalize() {
    std::vector<double> vec = to_vector();
    double mag = magnitude();
    
    if (mag > 0.0) {
        for (auto& val : vec) {
            val /= mag;
        }
        from_vector(vec);
    }
}

double EmbodimentTensorSignature::similarity(const EmbodimentTensorSignature& other) const {
    std::vector<double> vec1 = to_vector();
    std::vector<double> vec2 = other.to_vector();
    
    if (vec1.size() != vec2.size()) {
        return 0.0;
    }
    
    // Compute cosine similarity
    double dot_product = 0.0;
    double norm1 = 0.0;
    double norm2 = 0.0;
    
    for (size_t i = 0; i < vec1.size(); ++i) {
        dot_product += vec1[i] * vec2[i];
        norm1 += vec1[i] * vec1[i];
        norm2 += vec2[i] * vec2[i];
    }
    
    norm1 = std::sqrt(norm1);
    norm2 = std::sqrt(norm2);
    
    if (norm1 > 0.0 && norm2 > 0.0) {
        return dot_product / (norm1 * norm2);
    }
    
    return 0.0;
}

void EmbodimentTensorSignature::update_sensory_input(SensoryModality modality, double intensity) {
    if (modality >= 0 && modality < sensory_modality.size()) {
        sensory_modality[modality] = std::max(0.0, std::min(1.0, intensity));
        timestamp = std::chrono::steady_clock::now();
    }
}

void EmbodimentTensorSignature::update_motor_command(MotorCommand command, double value) {
    if (command >= 0 && command < motor_command.size()) {
        motor_command[command] = value;
        timestamp = std::chrono::steady_clock::now();
    }
}

void EmbodimentTensorSignature::update_spatial_position(double x, double y, double z, double orientation) {
    spatial_coordinates.x = x;
    spatial_coordinates.y = y;
    spatial_coordinates.z = z;
    spatial_coordinates.orientation = orientation;
    timestamp = std::chrono::steady_clock::now();
}

double EmbodimentTensorSignature::calculate_cognitive_load() const {
    // Calculate load based on active tensor components
    double load = 0.0;
    
    // Sensory load
    for (const auto& val : sensory_modality) {
        load += val * val;
    }
    
    // Motor load
    for (const auto& val : motor_command) {
        load += val * val * 0.8; // Motor commands slightly less weight
    }
    
    // Temporal processing load
    for (const auto& val : temporal_context) {
        load += val * 0.6; // Temporal processing moderate weight
    }
    
    // Interaction complexity
    for (const auto& val : interaction_mode) {
        load += val * 0.4;
    }
    
    // Normalize to 0-1 range
    return std::min(1.0, load / 10.0);
}

EmbodimentTensorSignature::SensoryModality EmbodimentTensorSignature::get_dominant_sensory_modality() const {
    auto max_it = std::max_element(sensory_modality.begin(), sensory_modality.end());
    return static_cast<SensoryModality>(std::distance(sensory_modality.begin(), max_it));
}

EmbodimentTensorSignature::EmbodimentState EmbodimentTensorSignature::get_current_embodiment_state() const {
    auto max_it = std::max_element(embodiment_state.begin(), embodiment_state.end());
    return static_cast<EmbodimentState>(std::distance(embodiment_state.begin(), max_it));
}

std::string EmbodimentTensorSignature::get_summary() const {
    std::ostringstream oss;
    oss << "EmbodimentTensor[" << agent_id << "] ";
    oss << "Cognitive_Load:" << std::fixed << std::setprecision(3) << calculate_cognitive_load() << " ";
    oss << "Embodiment:" << (get_current_embodiment_state() == VIRTUAL ? "Virtual" : 
                            get_current_embodiment_state() == PHYSICAL ? "Physical" : "Hybrid") << " ";
    oss << "Confidence:" << action_confidence;
    return oss.str();
}

// ===== EmbodimentTensorManager Implementation =====

EmbodimentTensorManager::EmbodimentTensorManager(size_t max_history) 
    : max_history_size_(max_history) {
}

void EmbodimentTensorManager::register_agent(const std::string& agent_id, 
                                           const EmbodimentTensorSignature& tensor) {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    agent_tensors_[agent_id] = tensor;
    tensor_history_[agent_id].push_back(tensor);
}

void EmbodimentTensorManager::update_agent_tensor(const std::string& agent_id, 
                                                const EmbodimentTensorSignature& tensor) {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    agent_tensors_[agent_id] = tensor;
    
    auto& history = tensor_history_[agent_id];
    history.push_back(tensor);
    
    // Maintain history size limit
    if (history.size() > max_history_size_) {
        history.erase(history.begin(), history.begin() + (history.size() - max_history_size_));
    }
}

EmbodimentTensorSignature EmbodimentTensorManager::get_agent_tensor(const std::string& agent_id) const {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    auto it = agent_tensors_.find(agent_id);
    if (it != agent_tensors_.end()) {
        return it->second;
    }
    return EmbodimentTensorSignature(); // Return default tensor if not found
}

std::vector<std::string> EmbodimentTensorManager::get_active_agents() const {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    std::vector<std::string> agents;
    for (const auto& pair : agent_tensors_) {
        agents.push_back(pair.first);
    }
    return agents;
}

double EmbodimentTensorManager::calculate_tensor_correlation(const std::string& agent1, 
                                                           const std::string& agent2) const {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    auto it1 = agent_tensors_.find(agent1);
    auto it2 = agent_tensors_.find(agent2);
    
    if (it1 != agent_tensors_.end() && it2 != agent_tensors_.end()) {
        return it1->second.similarity(it2->second);
    }
    
    return 0.0;
}

EmbodimentTensorManager::TensorStats EmbodimentTensorManager::get_tensor_statistics() const {
    std::lock_guard<std::mutex> lock(manager_mutex_);
    
    TensorStats stats;
    stats.active_agents = agent_tensors_.size();
    stats.total_tensor_updates = 0;
    
    double total_load = 0.0;
    std::vector<double> similarity_values;
    
    for (const auto& pair : agent_tensors_) {
        total_load += pair.second.calculate_cognitive_load();
        
        // Count updates from history
        auto hist_it = tensor_history_.find(pair.first);
        if (hist_it != tensor_history_.end()) {
            stats.total_tensor_updates += hist_it->second.size();
        }
    }
    
    if (stats.active_agents > 0) {
        stats.average_cognitive_load = total_load / stats.active_agents;
        
        // Calculate diversity as average pairwise dissimilarity
        double total_similarity = 0.0;
        size_t pair_count = 0;
        
        auto agents = get_active_agents();
        for (size_t i = 0; i < agents.size(); ++i) {
            for (size_t j = i + 1; j < agents.size(); ++j) {
                total_similarity += calculate_tensor_correlation(agents[i], agents[j]);
                pair_count++;
            }
        }
        
        if (pair_count > 0) {
            stats.tensor_diversity = 1.0 - (total_similarity / pair_count);
        } else {
            stats.tensor_diversity = 1.0;
        }
    } else {
        stats.average_cognitive_load = 0.0;
        stats.tensor_diversity = 0.0;
    }
    
    return stats;
}

} // namespace embodiment
} // namespace opencog